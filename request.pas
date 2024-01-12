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
  // Llama context
  Ctx     : Pllama_context;
  // Llama model
  Model   : Pllama_model;
  // Llama model parameters
  Params  : Tllama_model_params;
  // Vocabulary size
  n_vocab: longint;
  // Array to store candidate tokens
  candidates: array of Tllama_token_data;
  candidates_p: Tllama_token_data_array;
  // Input embeddings
  EmbdInp : TTokenList;
  // Minimum number of kept candidates, number of probabilities,
  // constants for calculations, and number of generated tokens
  min_keep, n_probs, C,n_gen : Integer;
  // Tokens for the sequence-to-sequence task
  SSTokens: TTokenList;
  // Logits (raw prediction scores) from the model
  logits: Psingle;
  // Token ID
  token_id: Tllama_token;
  // Maximum context size and maximum size for token lists
  max_context_size,max_tokens_list_size: Integer;
  // Flag indicating if the model is loaded
  Loaded : boolean;
  // Model file name
  ModelFName: string;
protected
  procedure Execute; override;
public
  // Constructor with required parameters
  constructor Create(ModelPath:string;llm: Pllama_model; Prams: Tllama_model_params;maxCTXLen:Integer);
 end;

// Handle ChatGPT engine
TChatGPTThread = class(TRequestThread)
protected
  // Overridden method to execute the thread
  procedure Execute; override;
private
  // OpenAI client instance
  FClient: IOpenAIClient;
  // List of messages in the conversation
  FMessages: TChatCompletionRequestMessageList;
  // Method to add a message into the conversation history
  procedure AddMessage(const Role, Content: string);
  // Method to ask a question and get an answer
  function AskQuestion(const Question: string): string;
public
   // Constructor with required parameters
  constructor Create(ModelPath: string;apikey: string;maxCTXLen:Integer);
 end;


implementation

function TTokenList.Data(const P: Integer = 0): Pllama_token;
begin
  Result := @FItems[P];
end;

{TChatGPTThread}

// Initialize the TChatGPTThread object
constructor TChatGPTThread.Create(ModelPath: string;apikey: string;maxCTXLen:Integer);
begin
  // Call the parent constructor to create the thread suspended
  inherited Create(ModelPath,maxCTXLen); // Create the thread suspended
  // Allocate memory for storing messages in the conversation
  FMessages := TChatCompletionRequestMessageList.Create;
  // Set up the OpenAI client with access token (API Key)
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
  Request: TCreateChatCompletionRequest; // Object to hold the request parameters.
  ResponseG: TCreateChatCompletionResponse; // Object to hold the response from the OpenAI API.
  SourceMsg, TargetMsg: TChatCompletionRequestMessage; // Message objects to transfer data between the local message list and the request object.
begin
  ResponseG := nil; // Initialize the response object as nil.
  Request := TCreateChatCompletionRequest.Create; // Create a new request object.
  try
    AddMessage('user', Question); // Add the user's question to the messages list.
    Request.Model := self.ModelName; // Set the requested model name.
    Request.MaxTokens := self.maxContextLen; {// Set the maximum number of tokens (words/characters) allowed in context.
                           // Be cautious with this setting, as using too many tokens may exhaust your API quota.}
    for SourceMsg in FMessages do // Iterate through the local messages list.
    begin
      TargetMsg := TChatCompletionRequestMessage.Create; // Create a new target message object.
      Request.Messages.Add(TargetMsg); // Add the target message object to the request object's messages list.
      TargetMsg.Role := SourceMsg.Role; // Copy the role ('system', 'user', or 'assistant') from source to target.
      TargetMsg.Content := SourceMsg.Content; {// Copy the content (the text of the message) from source to target.
                             // Note that each message is limited by the max_tokens value set earlier.}
    end;
    try
        ResponseG := FClient.OpenAI.CreateChatCompletion(Request); {// Send the request to the OpenAI API and store the result in ResponseG.}
    except
      Result := 'Connection error.'; {// If there was an issue connecting to the API, return an error message.}
      exit;
    end;
    if Assigned(ResponseG.Choices) and (ResponseG.Choices.Count > 0) then
    begin
      Result := ResponseG.Choices[0].Message.Content; {// Get the generated response from the first choice in the Choices array.
                               // Store the response in the function's output variable.}
      AddMessage('assistant', Result); {// Add the AI-generated response to the local messages list.}
    end
    else
      Result := ''; {// If no valid choices are available, return an empty string.}
  finally
    Request.Free; {// Free allocated memory resources after use.}
    ResponseG.Free;
  end;
end;

{Starts a conversation via PromptToAnswer, gets AI response, then ends.}
procedure TChatGPTThread.Execute;
var
  prompt:UnicodeString;
  answer:UnicodeString;
  p:Integer;
begin
  Response:=AskQuestion(PromptToAnswer);
  Terminate;
end;

{TllamaCPPThread}


// Constructor initializes Llama CPP Thread object with given parameters
// Args: ModelPath (string): Path to load the pretrained model from
//       llm (Pllama_model): Pointer to Llama model instance
//       Params (Tllama_model_params): Configuration settings for the model
//       maxCTXLen (Integer): Maximum context length for the model
constructor TllamaCPPThread.Create(ModelPath:string;llm: Pllama_model; Prams: Tllama_model_params;maxCTXLen:Integer);
begin
  inherited Create(ModelPath,maxCTXLen); // Create the thread suspended
  self.ModelFName:=ModelPath;
  self.Model:=llm;
  self.Params:=Prams;
  SSTokens := TTokenList.Create;
  EmbdInp := TTokenList.Create;
end;

{ Purpose: Converts a given hexadecimal encoded string into its equivalent wide string representation.
          Ignores invalid hexadecimal values and handles special cases where hexadecimal values appear
          inside HTML tags by copying them directly without modification.
}

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
    {
      Handle specific case where we have a valid hexadecimal value embedded in an HTML tag
      1) Skip over leading zeros ('0x')
      2) Collect consecutive digits until reaching terminating condition (either another angle bracket or end of string)
      3) Attempt conversion from collected hexadecimal string to Unicode codepoint
      4) If successful, append resulting wide char to output string
      5) Otherwise, throw exception indicating invalid hexadecimal value
      }
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
// Procedure executed by the thread
procedure TllamaCPPThread.Execute;
var
  // Declare variables used in the procedure
  TokenStr: RawByteString;
  answer: String;
  p: Integer;
  S: String;
  N_THREADS: Integer;
  TOP_K: Integer;
  TOP_P: Single;
  TYPICAL_P: Single;
  TFS_Z: Single;
  TEMP: Single;
  uni: Byte;
  ctxParams: Tllama_context_params;
begin
  // Check if model exists, return error message if not present
  if self.Model = nil then
  begin
    Response := 'This local AI is not loaded. Please open the AI file first.';
    exit;
  end;

  // Assign basic parameter values from settings
  N_THREADS := StrToIntDef(Settings.ComboBoxThreads.Text, 4);
  TOP_K := StrToIntDef(Settings.LabeledEditK.Text, 40);
  TOP_P := StrToFloatDef(Settings.LabeledEditP.Text, 0.88);
  TYPICAL_P := 1.0;
  TFS_Z := 1.0;
  TEMP := StrToFloatDef(Settings.LabeledEditTemperature.Text, 1.0);

  // Initialize exception handling
  SetExceptionMask(GetExceptionMask + [exOverflow, exZeroDivide, exInvalidOp]);

  // Llama backend initialization
  llama_backend_init(False);

  // Context parameter setup
  ctxParams := llama_context_default_params;
  ctxParams.n_threads := N_THREADS;
  ctxParams.n_threads_batch := N_THREADS;
  ctxParams.offload_kqv := Settings.CheckBoxGPUOffloadCache.Enabled;

  // Create context with given model and parameters
  Ctx := llama_new_context_with_model(Model, ctxParams);

  // Calculate maximum context size and number of tokens based on context length limit
  max_context_size := Min(llama_n_ctx(ctx), self.maxContextLen);
  max_tokens_list_size := max_context_size - 4;
  n_gen := Min(StrToIntDef(Settings.LabeledEditMaxLen.Text, 1024), max_context_size);

  // Get the prompt text and tokenize it
  S := PromptToAnswer;
  EmbdInp.Count := Length(S) + 1;
  C := llama_tokenize(Model, PChar(S), Length(S), EmbdInp.Data, EmbdInp.Count, False, False);

  candidates:=[];

  // Main loop starts here
    while llama_get_kv_cache_token_count(ctx) < n_gen do
        begin
          // Check if the context has been terminated, and if so, exit the loop
          if self.Terminated then break;
          // Run the Llama evaluation function with the input data, model, and cache size
          llama_eval(Ctx, EmbdInp.Data, C, llama_get_kv_cache_token_count(ctx));
          EmbdInp.Clear;
          // Sample the next token by initializing an array of candidate tokens and their corresponding logits
          n_vocab := llama_n_vocab(Model);
          SetLength(candidates, n_vocab);
          logits := llama_get_logits(ctx);
          for p := 0 to n_vocab - 1 do
          begin
            // Initialize each candidate token with its ID, logit, and probability (initially set to zero)
            candidates[p].id := p;
            candidates[p].logit := logits[p];
            candidates[p].p := 0.0;
          end;
          // Prepare the candidates list for sorting and sampling
          candidates_p.data := @candidates[0];
          candidates_p.size := n_vocab;
          candidates_p.sorted := False;
          // Calculate various parameters used for sampling, such as number of probabilities, minimum keep value, etc.
          n_probs:=1;
          min_keep := Max(1, n_probs);
          // Perform several different types of sampling on the candidate tokens based on specific criteria
          llama_sample_top_k(ctx, @candidates_p, top_k, min_keep);
          llama_sample_tail_free(ctx, @candidates_p, tfs_z, min_keep);
          llama_sample_typical(ctx, @candidates_p, typical_p, min_keep);
          llama_sample_top_p(ctx, @candidates_p, top_p, min_keep);
          llama_sample_temperature(ctx, @candidates_p, temp);
          // Select the final token ID based on the sampled probabilities
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
                  TokenStr:=StringReplace(TokenStr,'Ġ',' ',[rfReplaceAll]);
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
                  // Based on whether the current token ID corresponds to a newline token, update the answer string accordingly
                  if token_id = llama_token_nl(Model) then
                    answer := answer + #10
                  else
                    answer := answer + TokenStr;
                  // Update the GUI with the partial answer, which includes the answer string so far
                  self.PartialAnswer := '<b><center>(Press ESC to stop generating)</center></b>' + answer;
                  // Add the current token ID to the list of embedded inputs
                  EmbdInp.Add(token_id);
                  // Reset the counter variable C to 1
                  C := 1;
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

// Function converting a given Unicode string into its escaped sequence representation
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

{ Procedure handling request thread execution for Neuroengine.ai API}
procedure TRequestThread.Execute;
var
  prompt, answer,TokenStr: UnicodeString;
  p,i: Integer;
  TOP_K, TOP_P, TEMP: Single;
  max_new_len: Integer;
  streamkey:String;
  gettokens:Integer;
begin
  { Retrieve configuration values for K, P, and temperature parameters }
  TOP_K := StrToIntDef(Settings.LabeledEditK.Text, 40);
  TOP_P := StrToFloatDef(Settings.LabeledEditP.Text, 0.88);
  TEMP := StrToFloatDef(Settings.LabeledEditTemperature.Text, 1.0);
  max_new_len := Self.MaxContextLen;
  {Generate streamkey}
  Randomize;
  SetLength(streamkey, 32);
  for i := 1 to 32 do
    streamkey[i] := Chr(Ord('A') + Random(26)); // Generates a random uppercase letter
  { Call QueryAI function to obtain AI generated answer }
  answer:='';
  i:=0;
  gettokens:=20;
  while (True) do
        begin
        // Check if the context has been terminated, and if so, exit the loop
        if self.Terminated then break;
        TokenStr := QueryAI(Self.ModelName, PromptToAnswer, TEMP, TOP_P, TOP_K, 1.2, max_new_len, 0, True,streamkey,gettokens);
        {Increase tokens to get so the first is fast, but the optimize for llm time}
        gettokens:=gettokens+5;
        if Length(TokenStr)=0 then
           break;
        answer := answer + TokenStr;
        if (i=0) then
           begin
           answer:=Copy(answer,Length(PromptToAnswer),Length(answer));
           end;
        { Processing the obtained answer by removing prompts and extracting relevant content }
        p:=PosEx('user:',LowerCase(answer));
        if (p>0) then
           Delete(answer,p-2,Length(answer));
        // Update the GUI with the partial answer, which includes the answer string so far
        self.PartialAnswer := '<b><center>(Press ESC to stop generating)</center></b>' + answer;
        if (p>0) then break;
        i:=i+1;
        end;
  { Error checking and processing empty/null answers }
  if Answer = '' then
  begin
    Response := 'Connection error.';
    Terminate;
    Exit;
  end;
  Response:=Trim(answer);
  self.PartialAnswer:='';
  Terminate;
end;

end.

