unit Request;

{$mode ObjFPC}{$H+}

interface



uses
  Classes, SysUtils,neuroengineapi, strutils,llama, Math, Generics.Collections, OpenAIClient, OpenAIDtos,OptionsForm;

type
  TTokenList = class(specialize TList<Tllama_token>)
  public
    function Data(const P: Integer = 0): Pllama_token;
  end;


type

TUpdate_token_callback = procedure (message:string);

// Handle Neuroengine.ai requests
TRequestThread = class(TThread)
protected
  procedure Execute; override;
public
  ModelName: string;
  maxContextLen: Integer;
  PromptToAnswer: Unicodestring;
  Response: Unicodestring;
  PartialAnswer: Unicodestring;
  constructor Create(Service: string;maxCTXLen:Integer);
end;

// Handle Llama.cpp engine
TllamaCPPThread = class(TRequestThread)
private
  Ctx     : Pllama_context;
  Model   : Pllama_model;
  Params  : Tllama_model_params;
  n_vocab: longint;
  candidates: array of Tllama_token_data;
  candidates_p: Tllama_token_data_array;
  EmbdInp : TTokenList;
  min_keep, n_probs,
  C,n_gen : Integer;
  SSTokens: TTokenList;
  logits: Psingle;
  token_id: Tllama_token;
  max_context_size,max_tokens_list_size: Integer;
  Loaded : boolean;
  ModelFName: string;
protected
  procedure Execute; override;
public
  constructor Create(ModelPath:string;llm: Pllama_model; Prams: Tllama_model_params;maxCTXLen:Integer);
//  function Load():Boolean;
 end;

// Handle ChatGPT engine
TChatGPTThread = class(TRequestThread)
protected
  procedure Execute; override;
private
  FClient: IOpenAIClient;
  FMessages: TChatCompletionRequestMessageList;
  procedure AddMessage(const Role, Content: string);
  function AskQuestion(const Question: string): string;
public
  constructor Create(ModelPath: string;apikey: string;maxCTXLen:Integer);
 end;


implementation

function TTokenList.Data(const P: Integer = 0): Pllama_token;
begin
  Result := @FItems[P];
end;

{TChatGPTThread}
constructor TChatGPTThread.Create(ModelPath: string;apikey: string;maxCTXLen:Integer);
begin
  inherited Create(ModelPath,maxCTXLen); // Create the thread suspended
  FMessages := TChatCompletionRequestMessageList.Create;
  FClient := TOpenAIClient.Create;
  FClient.Config.AccessToken := apikey;
end;


procedure TChatGPTThread.AddMessage(const Role, Content: string);
var
  Msg: TChatCompletionRequestMessage;
begin
  Msg := TChatCompletionRequestMessage.Create;
  FMessages.Add(Msg);
  Msg.Role := Role;
  Msg.Content := Content;
end;


function TChatGPTThread.AskQuestion(const Question: string): string;
var
  Request: TCreateChatCompletionRequest;
  ResponseG: TCreateChatCompletionResponse;
  SourceMsg, TargetMsg: TChatCompletionRequestMessage;
begin
  ResponseG := nil;
  Request := TCreateChatCompletionRequest.Create;
  try
    AddMessage('user', Question);
    Request.Model := self.ModelName;
    Request.MaxTokens := self.maxContextLen; // Be careful as this can quickly consume your API quota.
    for SourceMsg in FMessages do
    begin
      TargetMsg := TChatCompletionRequestMessage.Create;
      Request.Messages.Add(TargetMsg);
      TargetMsg.Role := SourceMsg.Role;
      TargetMsg.Content := SourceMsg.Content;
    end;
    try
        ResponseG := FClient.OpenAI.CreateChatCompletion(Request);
    except
      Result := 'Connection error.';
      exit;
    end;
    if Assigned(ResponseG.Choices) and (ResponseG.Choices.Count > 0) then
    begin
      Result := ResponseG.Choices[0].Message.Content;
      AddMessage('assistant', Result);
    end
    else
      Result := '';
  finally
    Request.Free;
    ResponseG.Free;
  end;
end;


procedure TChatGPTThread.Execute;
var
  prompt:UnicodeString;
  answer:UnicodeString;
  p:Integer;
begin
//  Sleep(1000); // Simulate some work for 2 seconds
  Response:=AskQuestion(PromptToAnswer);
  Terminate;
end;

{TllamaCPPThread}
constructor TllamaCPPThread.Create(ModelPath:string;llm: Pllama_model; Prams: Tllama_model_params;maxCTXLen:Integer);
begin
  inherited Create(ModelPath,maxCTXLen); // Create the thread suspended
//  self.Loaded:=False;
  self.ModelFName:=ModelPath;
  self.Model:=llm;
  self.Params:=Prams;
  SSTokens := TTokenList.Create;
  EmbdInp := TTokenList.Create;
end;

function HexToUnicode(const HexString: string): WideString;
var
  HexValue: string;
  CodePoint: LongWord;
  i: Integer;
  InsideHtmlTag: Boolean;
begin
  Result := '';
  i := 1;
  InsideHtmlTag := False;

  while i <= Length(HexString) do
  begin
    if HexString[i] = '<' then
    begin
      InsideHtmlTag := True;
      Result := Result + HexString[i];
      Inc(i);
    end
    else if HexString[i] = '>' then
    begin
      InsideHtmlTag := False;
      Result := Result + HexString[i];
      Inc(i);
    end
    else if (InsideHtmlTag) and (HexString[i] = '0') then
    begin
      Inc(i); // Skip '0x'
      Inc(i); // Skip '0x'

      // Extract the hexadecimal value
      HexValue := '';
      while (i <= Length(HexString)) and (HexString[i] <> '>') do
      begin
        HexValue := HexValue + HexString[i];
        Inc(i);
      end;

      // Convert the hexadecimal value to a Unicode code point
      if TryStrToInt('$' + HexValue, LongInt(CodePoint)) then
        Result := Result + WideChar(CodePoint)
      else
        raise Exception.Create('Invalid hexadecimal value: ' + HexValue);
    end
    else
    begin
      // Append non-hexadecimal characters or characters inside HTML tags as is
      Result := Result + HexString[i];
      Inc(i);
    end;
  end;
end;

{LLama.cpp Inference code}
procedure TllamaCPPThread.Execute;
var
  TokenStr:RawByteString;
  answer:String;
  p:Integer;
  S: String;
  N_THREADS: Integer;
  TOP_K: Integer;
  TOP_P: Single;
  TYPICAL_P: Single;
  TFS_Z: Single;
  TEMP: Single;
  uni: Byte;
  ctxParams:Tllama_context_params;
begin
  if self.Model=nil then
     begin
       Response:='This local AI is not loaded. Please open the AI file first.';
       exit;
     end;
  N_THREADS:=StrToIntDef(Settings.ComboBoxThreads.Text,4);
  TOP_K:= StrToIntDef(Settings.LabeledEditK.Text,40);
  TOP_P:= StrToFloatDef(Settings.LabeledEditP.Text,0.88);
  TYPICAL_P:= 1.0;
  TFS_Z:= 1.0;
  TEMP:= StrToFloatDef(Settings.LabeledEditTemperature.Text, 1.0);
  SetExceptionMask(GetExceptionMask + [exOverflow,exZeroDivide,exInvalidOp]); // God dammit, llama.cpp
  llama_backend_init(False);
  ctxParams:=llama_context_default_params;
  ctxParams.n_threads:=N_THREADS;
  ctxParams.n_threads_batch:=N_THREADS;
  Ctx:=llama_new_context_with_model(Model,ctxParams);
  max_context_size     := Min(llama_n_ctx(ctx),self.maxContextLen);
  max_tokens_list_size := max_context_size - 4;
  n_gen := Min(StrToIntDef(Settings.LabeledEditMaxLen.Text,1024), max_context_size);
  S := PromptToAnswer;
  // tokenize the prompt
  EmbdInp.Count := Length(S) + 1;
  C := llama_tokenize(Model, PChar(S), Length(S), EmbdInp.Data, EmbdInp.Count, False,False);
  candidates:=[];
      // main loop
    while llama_get_kv_cache_token_count(ctx) < n_gen do
        begin

        llama_eval(Ctx, EmbdInp.Data, C, llama_get_kv_cache_token_count(ctx));
        EmbdInp.Clear;

        // sample the next token
        n_vocab := llama_n_vocab(Model);
        SetLength(candidates, n_vocab);
        logits := llama_get_logits(ctx);

        for p := 0 to n_vocab - 1 do
        begin
          candidates[p].id := p;
          candidates[p].logit := logits[p];
          candidates[p].p := 0.0;
        end;

        candidates_p.data := @candidates[0];
        candidates_p.size := n_vocab;
        candidates_p.sorted := False;

        n_probs:=1;
        min_keep := Max(1, n_probs);

        llama_sample_top_k(ctx, @candidates_p, top_k, min_keep);
        llama_sample_tail_free(ctx, @candidates_p, tfs_z, min_keep);
        llama_sample_typical(ctx, @candidates_p, typical_p, min_keep);
        llama_sample_top_p(ctx, @candidates_p, top_p, min_keep);
        llama_sample_temperature(ctx, @candidates_p, temp);
        token_id:=llama_sample_token(ctx, @candidates_p);
        // Alternative greedy sampling
  //      token_id:=llama_sample_token_greedy(ctx, @candidates_p);
        if token_id = llama_token_eos(Model) then
            begin
              break;
            end
            else begin
                  TokenStr:=llama_token_get_text(Model, token_id);
                  TokenStr:=StringReplace(TokenStr,'▁',' ',[rfReplaceAll]);
                  if LowerCase(TokenStr)='user' then
                      break;
                  {Try to detect encoded chars, why you do this llama.cpp?}
                  if length(Tokenstr)=6 then
                     begin
                       if (TokenStr[1]='<') and (TokenStr[6]='>') then
                          begin
                          TokenStr := Copy(TokenStr, 4, Length(TokenStr) - 4); // Remove the angle brackets
                          p := hex2dec(TokenStr); // Convert hexadecimal to decimal
                          uni := p; // Convert decimal to char
                          TokenStr:=chr(uni);
                          end;
                     end;
                  if token_id = llama_token_nl(Model) then
                       answer:=answer+#10
                  else answer:=answer+TokenStr;
                  {Update gui token by token}
                  self.PartialAnswer:=answer;
                  EmbdInp.Add(token_id);
                  C:=1;
                  end;
        end;
  llama_free(Ctx);
  SSTokens.Free;
  EmbdInp.Free;
  Response:=answer;
  self.PartialAnswer:='';
  Terminate;
end;

{TRequestThread}

constructor TRequestThread.Create(Service: string;maxCTXLen:Integer);
begin
  inherited Create(True); // Create the thread suspended
  self.ModelName:=Service;
  self.maxContextLen:=maxCTXLen;
end;

function UnicodeToEscape(const UnicodeStr: string): AnsiString;
var
  i, len: Integer;
  AnsiChr: AnsiChar;
begin
  Result := '';
  len := Length(UnicodeStr);
  i := 1;
  while i <= len do
  begin
    if (UnicodeStr[i] = '#') and (i + 5 <= len) and (UnicodeStr[i + 1] = '$') then
    begin
      AnsiChr := AnsiChar(StrToInt('$' + Copy(UnicodeStr, i + 2, 4)));
      Result := Result + '\' + AnsiChr;
      Inc(i, 6);
    end
    else
    begin
      Result := Result + UnicodeStr[i];
      Inc(i);
    end;
  end;
end;

procedure TRequestThread.Execute;
var
  prompt:UnicodeString;
  answer:UnicodeString;
  p:Integer;
  TOP_K: Integer;
  TOP_P: Single;
  TEMP: Single;
  max_new_len:Integer;
begin
  TOP_K:= StrToIntDef(Settings.LabeledEditK.Text,40);
  TOP_P:= StrToFloatDef(Settings.LabeledEditP.Text,0.88);
  TEMP:= StrToFloatDef(Settings.LabeledEditTemperature.Text, 1.0);
  max_new_len := self.maxContextLen;

  answer:=QueryAI(self.ModelName,PromptToAnswer,TEMP,TOP_P,TOP_K,1.2,max_new_len,0,True);
  if answer='' then begin
    Response:='Connection error.';
    Terminate;
    Exit;
    end;
  // Find answer and cut it
  p:=0;
  if (p=0) then
     begin
     answer:=Copy(answer,p+Length(PromptToAnswer),Length(answer));
     answer:=Trim(answer);
     p:=PosEx('user:',LowerCase(answer));
     if (p>0) then
        Delete(answer,p-2,Length(answer));
     Response:=Trim(answer);
     end;

  Terminate;
end;

end.

