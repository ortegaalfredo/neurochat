program llama_cli;

uses
  Classes, SysUtils, Generics.Collections,llama, Math, StrUtils;

const
  N_THREADS = 4;
  TOP_K = 40;
  TOP_P = 0.88;
  TYPICAL_P = 1.0;
  TFS_Z = 1.0;
  TEMP = 2;

type
  TTokenList = class(specialize TList<Tllama_token>)
  public
    function Data(const P: Integer = 0): Pllama_token;
  end;

function TTokenList.Data(const P: Integer = 0): Pllama_token;
begin
  Result := @FItems[P];
end;

var
  Ctx     : Pllama_context;
  Model   : Pllama_model;
  Params  : Tllama_context_params;
  S,
  ModelFName: String;
  TokenStr: String;
  IsInteractive: Boolean = False;
  Prompt  : String;


procedure ParseParameters;
var
  I: Integer = 1;
  procedure Increase;
  begin
    Inc(I);
    if I > ParamCount then
      raise Exception.Create('Invalid parameter');
  end;

begin
  if ParamCount = 0 then
  begin
    Writeln('Usage: llama-cli -m <model_name> -p <prompt>');
    Halt;
  end;
  while I <= ParamCount do
  begin
    case ParamStr(I) of
      '-i':
        begin
          IsInteractive := True;
        end;
      '-m':
        begin
          Increase;
          ModelFName := ParamStr(I);
        end;
      '-p':
        begin
          Increase;
          Prompt := ParamStr(I);
        end;
      '-h':
        begin
          Writeln(' -h: This help screen');
          Writeln(' -i: Interactive mode');
          Writeln(' -m: Path to model file');
          Writeln(' -p: Prompt');
          Halt;
        end;
    end;
    Inc(I);
  end;
  if (Prompt = '') and (not IsInteractive) then
    raise Exception.Create('No prompt provided.');
end;
var
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

begin
  ParseParameters;
  SetExceptionMask(GetExceptionMask + [exOverflow,exZeroDivide,exInvalidOp]); // God dammit, llama.cpp
  llama_backend_init(False);
  Params := llama_context_default_params;
  SSTokens := TTokenList.Create;
  EmbdInp := TTokenList.Create;
  ModelFName := ModelFName;

  // Load model
  Model := llama_load_model_from_file(PChar(ModelFName), Params);
  if Model = nil then
    raise Exception.Create('Failed to load model');

  if IsInteractive then
    Writeln(#10'Interactive mode is ON'#10);
   Ctx:=llama_new_context_with_model(Model,Params);

  repeat
    if not IsInteractive then
    begin
      Writeln;
      Writeln(Prompt);
    end else
    begin
      Prompt := '';
      Write('>');
      Readln(Prompt);
    end;

    max_context_size     := llama_n_ctx(ctx);
    max_tokens_list_size := max_context_size - 4;
    n_gen := Min(1024, max_context_size);

    S := Prompt;

    // tokenize the prompt
    EmbdInp.Count := Length(Prompt) + 1;
    C := llama_tokenize(Ctx, PChar(Prompt), Length(Prompt), EmbdInp.Data, EmbdInp.Count, False);
    candidates:=[];

    // main loop
    while llama_get_kv_cache_token_count(ctx) < n_gen do
        begin
        llama_eval(Ctx, EmbdInp.Data, C, llama_get_kv_cache_token_count(ctx), N_THREADS);
        EmbdInp.Clear;

        // sample the next token
        n_vocab := llama_n_vocab(ctx);
        SetLength(candidates, n_vocab);
        logits := llama_get_logits(ctx);

        for token_id := 0 to n_vocab - 1 do
        begin
          candidates[token_id].id := token_id;
          candidates[token_id].logit := logits[token_id];
          candidates[token_id].p := 0.0;
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
        if token_id = llama_token_eos(ctx) then
            begin
              Write(' <EOS>');
              break;
            end
            else begin
                  TokenStr:=llama_token_get_text(Ctx, token_id);
                  TokenStr:=StringReplace(TokenStr,'▁',' ',[rfReplaceAll]);
                  if token_id = llama_token_nl(ctx) then
                       Write(#10)
                  else Write(TokenStr);
                  EmbdInp.Add(token_id);
                  C:=1;
                  end;
    end;
    Writeln;
  until not IsInteractive;
  llama_free(Ctx);
  SSTokens.Free;
  EmbdInp.Free;
end.
