// API for llama.cpp 324f3403d54ae4499a1d68623161015f7419fb76 Thu Sep 21 21:08:20 2023
unit llama;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{$mode objfpc}{$H+}
{$macro on}
{$ifdef windows}
  {$define LLAMACALL:=stdcall}
  {$define LLAMALIB:='libllama.dll'}
{$else}
  {$define LLAMACALL:=cdecl}
  {$define LLAMALIB:='libllama.so'}
{$endif}
{$packrecords C}
{$packenum 4}

interface

  uses
  ctypes, dynlibs;

{
  Automatically converted by H2Pas 1.0.0 from ./llama.h
  The following command line parameters were used:
    -T
    -v
    -d
    -D
    -l
    libllama.so
    ./llama.h
}

  const
    External_library=LLAMALIB; {Setup as you need}

  Type

    Pchar  = ^char;

    Tllama_model = record
        {undefined structure}
      end;
    Pllama_model = ^Tllama_model;

    Tllama_context = record
        {undefined structure}
      end;
    Pllama_context=^Tllama_context;


    Tllama_token = longint;
    Pllama_token = ^Tllama_token;

    Tllama_log_level = (LLAMA_LOG_LEVEL_ERROR := 2,LLAMA_LOG_LEVEL_WARN := 3,
      LLAMA_LOG_LEVEL_INFO := 4);

    Tllama_vocab_type = (LLAMA_VOCAB_TYPE_SPM := 0,LLAMA_VOCAB_TYPE_BPE := 1
      );

    Tllama_token_type = (LLAMA_TOKEN_TYPE_UNDEFINED := 0,LLAMA_TOKEN_TYPE_NORMAL := 1,
      LLAMA_TOKEN_TYPE_UNKNOWN := 2,LLAMA_TOKEN_TYPE_CONTROL := 3,
      LLAMA_TOKEN_TYPE_USER_DEFINED := 4,
      LLAMA_TOKEN_TYPE_UNUSED := 5,LLAMA_TOKEN_TYPE_BYTE := 6
      );

    Tllama_ftype = (LLAMA_FTYPE_ALL_F32 := 0,LLAMA_FTYPE_MOSTLY_F16 := 1,
      LLAMA_FTYPE_MOSTLY_Q4_0 := 2,LLAMA_FTYPE_MOSTLY_Q4_1 := 3,
      LLAMA_FTYPE_MOSTLY_Q4_1_SOME_F16 := 4,
      LLAMA_FTYPE_MOSTLY_Q8_0 := 7,LLAMA_FTYPE_MOSTLY_Q5_0 := 8,
      LLAMA_FTYPE_MOSTLY_Q5_1 := 9,LLAMA_FTYPE_MOSTLY_Q2_K := 10,
      LLAMA_FTYPE_MOSTLY_Q3_K_S := 11,LLAMA_FTYPE_MOSTLY_Q3_K_M := 12,
      LLAMA_FTYPE_MOSTLY_Q3_K_L := 13,LLAMA_FTYPE_MOSTLY_Q4_K_S := 14,
      LLAMA_FTYPE_MOSTLY_Q4_K_M := 15,LLAMA_FTYPE_MOSTLY_Q5_K_S := 16,
      LLAMA_FTYPE_MOSTLY_Q5_K_M := 17,LLAMA_FTYPE_MOSTLY_Q6_K := 18,
      LLAMA_FTYPE_GUESSED := 1024);


    Tllama_token_data = record
        id : Tllama_token;
        logit : single;
        p : single;
      end;
    Pllama_token_data = ^Tllama_token_data;

    TBool = cbool;
    TSize_t = csize_t;
    Tuint32_t = cuint;
    Tint32_t = cint;
    Tdouble = cdouble;
    single = cfloat;
    Psingle = ^single;
    Tfloat = cfloat;
    Tint64_t = clong;
    Tuint64_t = culong;
    Tuint8_t = cuchar;
    Tint8_t = cchar;

    Tllama_token_data_array = record
        data : ^Tllama_token_data;
        size : Tsize_t;
        sorted : TBool;
      end;
    Pllama_token_data_array = ^Tllama_token_data_array;

    Tllama_progress_callback = procedure (progress:single; ctx:pointer);cdecl;

    Tllama_model_kv_override_type = (LLAMA_KV_OVERRIDE_INT,LLAMA_KV_OVERRIDE_FLOAT,
      LLAMA_KV_OVERRIDE_BOOL);
    Taaa = record
        case longint of
          0 : ( int_value : longint );
          1 : ( float_value : Tdouble );
          2 : ( bool_value : Tbool );
        end;
    Tllama_model_kv_override = record
        key : array[0..127] of char;
        tag : Tllama_model_kv_override_type;
        aa: Taaa;
      end;
    Pllama_model_kv_override = ^Tllama_model_kv_override;


    Tllama_model_params = record
        n_gpu_layers : Tint32_t;
        main_gpu : Tint32_t;
        tensor_split : ^single;
        progress_callback : Tllama_progress_callback;
        progress_callback_user_data : pointer;
        kv_overrides:Pllama_model_kv_override;
        vocab_only : TBool;
        use_mmap : TBool;
        use_mlock : TBool;
          end;

    Tllama_context_params = record
        seed : Tuint32_t;
        n_ctx : Tint32_t;
        n_batch : Tint32_t;
        n_threads : Tint32_t;
        n_threads_batch : Tint32_t;
        rope_scaling_type : Tint8_t;
        rope_freq_base : single;
        rope_freq_scale : single;
        yarn_ext_factor: single;  // YaRN extrapolation mix factor, negative = from model
        yarn_attn_factor: single; // YaRN magnitude scaling factor
        yarn_beta_fast: single;   // YaRN low correction dim
        yarn_beta_slow: single;   // YaRN high correction dim
        yarn_orig_ctx: Tuint32_t;    // YaRN original context size

        type_k: Tuint32_t;    // YaRN original context size
        type_v: Tuint32_t;    // YaRN original context size

        mul_mat_q : TBool;
        logits_all : TBool;
        embedding : TBool;
        offload_kqv : TBool;
      end;



    Tllama_log_callback = procedure (level:Tllama_log_level; text:Pchar; user_data:pointer);LLAMACALL;

    Tllama_model_quantize_params = record
        nthread : longint;
        ftype : Tllama_ftype;
        allow_requantize : TBool;
        quantize_output_tensor : TBool;
        only_copy : TBool;
      end;
    Tllama_grammar = record
        {undefined structure}
      end;

    Tllama_gretype = (LLAMA_GRETYPE_END := 0,LLAMA_GRETYPE_ALT := 1,
      LLAMA_GRETYPE_RULE_REF := 2,LLAMA_GRETYPE_CHAR := 3,
      LLAMA_GRETYPE_CHAR_NOT := 4,LLAMA_GRETYPE_CHAR_RNG_UPPER := 5,
      LLAMA_GRETYPE_CHAR_ALT := 6);


    Tllama_grammar_element = record
        _type : Tllama_gretype;
        value : Tuint32_t;
      end;
    Tllama_timings = record
        t_start_ms : Tdouble;
        t_end_ms : Tdouble;
        t_load_ms : Tdouble;
        t_sample_ms : Tdouble;
        t_p_eval_ms : Tdouble;
        t_eval_ms : Tdouble;
        n_sample : Tint32_t;
        n_p_eval : Tint32_t;
        n_eval : Tint32_t;
      end;

  function llama_context_default_params: Tllama_context_params; LLAMACALL;external External_library name 'llama_context_default_params';

  function llama_model_default_params:   Tllama_model_params;   LLAMACALL;external External_library name 'llama_model_default_params';

  procedure llama_backend_init(numa:TBool);LLAMACALL;external External_library name 'llama_backend_init';

  procedure llama_backend_free;LLAMACALL;external External_library name 'llama_backend_free';


  function llama_load_model_from_file(path_model:Pchar; params:Tllama_model_params):Pllama_model;LLAMACALL;external External_library name 'llama_load_model_from_file';

  procedure llama_free_model(model:Pllama_model);LLAMACALL;external External_library name 'llama_free_model';

  function llama_new_context_with_model(model:Pllama_model; params:Tllama_context_params):Pllama_context;LLAMACALL;external External_library name 'llama_new_context_with_model';

  procedure llama_free(ctx: Pllama_context);LLAMACALL;external External_library name 'llama_free';

  function llama_time_us:Tint64_t;LLAMACALL;external External_library name 'llama_time_us';

  function llama_max_devices:longint;LLAMACALL;external External_library name 'llama_max_devices';

  function llama_mmap_supported:TBool;LLAMACALL;external External_library name 'llama_mmap_supported';

  function llama_mlock_supported:TBool;LLAMACALL;external External_library name 'llama_mlock_supported';


  function llama_n_vocab(ctx:Pllama_model):longint;LLAMACALL;external External_library name 'llama_n_vocab';


  function llama_n_ctx(ctx:Pllama_context):longint;LLAMACALL;external External_library name 'llama_n_ctx';


  function llama_n_ctx_train(ctx:Pllama_context):longint;LLAMACALL;external External_library name 'llama_n_ctx_train';


  function llama_n_embd(ctx:Pllama_context):longint;LLAMACALL;external External_library name 'llama_n_embd';


  function llama_model_n_vocab(model:Pllama_model):longint;LLAMACALL;external External_library name 'llama_model_n_vocab';


  function llama_model_n_ctx(model:Pllama_model):longint;LLAMACALL;external External_library name 'llama_model_n_ctx';


  function llama_model_n_ctx_train(model:Pllama_model):longint;LLAMACALL;external External_library name 'llama_model_n_ctx_train';


  function llama_model_n_embd(model:Pllama_model):longint;LLAMACALL;external External_library name 'llama_model_n_embd';


  function llama_model_desc(model:Pllama_model; buf:Pchar; buf_size:Tsize_t):longint;LLAMACALL;external External_library name 'llama_model_desc';


  function llama_model_size(model:Pllama_model):Tuint64_t;LLAMACALL;external External_library name 'llama_model_size';


  function llama_model_n_params(model:Pllama_model):Tuint64_t;LLAMACALL;external External_library name 'llama_model_n_params';




  function llama_model_quantize(fname_inp:Pchar; fname_out:Pchar; var params:Tllama_model_quantize_params):longint;LLAMACALL;external External_library name 'llama_model_quantize';


  function llama_get_kv_cache_token_count(ctx:Pllama_context):longint;LLAMACALL;external External_library name 'llama_get_kv_cache_token_count';

  procedure llama_set_rng_seed(ctx:Pllama_context; seed:Tuint32_t);LLAMACALL;external External_library name 'llama_set_rng_seed';


  function llama_get_state_size(ctx:Pllama_context):Tsize_t;LLAMACALL;external External_library name 'llama_get_state_size';

  function llama_copy_state_data(ctx:Pllama_context; var dst:Tuint8_t):Tsize_t;LLAMACALL;external External_library name 'llama_copy_state_data';

  function llama_set_state_data(ctx:Pllama_context; var src:Tuint8_t):Tsize_t;LLAMACALL;external External_library name 'llama_set_state_data';


  function llama_load_session_file(ctx:Pllama_context; path_session:Pchar; var tokens_out:Tllama_token; n_token_capacity:Tsize_t; var n_token_count_out:Tsize_t):TBool;LLAMACALL;external External_library name 'llama_load_session_file';



  function llama_save_session_file(ctx:Pllama_context; path_session:Pchar; var tokens:Tllama_token; n_token_count:Tsize_t):TBool;LLAMACALL;external External_library name 'llama_save_session_file';


  function llama_eval(ctx:Pllama_context; tokens:Pllama_token; n_tokens:longint; n_past:longint):longint;LLAMACALL;external External_library name 'llama_eval';


  function llama_eval_embd(ctx:Pllama_context; var embd:single; n_tokens:longint; n_past:longint; n_threads:longint):longint;LLAMACALL;external External_library name 'llama_eval_embd';


  function llama_eval_export(ctx:Pllama_context; fname:Pchar):longint;LLAMACALL;external External_library name 'llama_eval_export';

  function llama_get_logits(ctx:Pllama_context):Psingle;LLAMACALL;external External_library name 'llama_get_logits';

  function llama_get_embeddings(ctx:Pllama_context):Psingle;LLAMACALL;external External_library name 'llama_get_embeddings';



  function llama_token_get_text(ctx:Pllama_model; token:Tllama_token):Pchar;LLAMACALL;external External_library name 'llama_token_get_text';


  function llama_token_get_score(ctx:Pllama_context; token:Tllama_token):single;LLAMACALL;external External_library name 'llama_token_get_score';


  function llama_token_bos(ctx:Pllama_model):Tllama_token;LLAMACALL;external External_library name 'llama_token_bos';


  function llama_token_eos(ctx:Pllama_model):Tllama_token;LLAMACALL;external External_library name 'llama_token_eos';


  function llama_token_nl(ctx:Pllama_model):Tllama_token;LLAMACALL;external External_library name 'llama_token_nl';


  function llama_tokenize(ctx:Pllama_model; text:Pchar; text_len:longint; tokens:Pllama_token; n_max_tokens:longint;
             add_bos:TBool; special: Tbool):longint;LLAMACALL;external External_library name 'llama_tokenize';



  function llama_tokenize_with_model(model:Pllama_model; text:Pchar; text_len:longint; var tokens:Tllama_token; n_max_tokens:longint; 
             add_bos:TBool):longint;LLAMACALL;external External_library name 'llama_tokenize_with_model';


  function llama_token_to_piece(ctx:Pllama_context; token:Tllama_token; buf:Pchar; length:longint):longint;LLAMACALL;external External_library name 'llama_token_to_piece';


  function llama_token_to_piece_with_model(model:Pllama_model; token:Tllama_token; buf:Pchar; length:longint):longint;LLAMACALL;external External_library name 'llama_token_to_piece_with_model';

  procedure llama_grammar_free(var grammar:Tllama_grammar);LLAMACALL;external External_library name 'llama_grammar_free';

  procedure llama_sample_repetition_penalty(ctx:Pllama_context; var candidates:Tllama_token_data_array; var last_tokens:Tllama_token; last_tokens_size:Tsize_t; penalty:single);LLAMACALL;external External_library name 'llama_sample_repetition_penalty';

  procedure llama_sample_frequency_and_presence_penalties(ctx:Pllama_context; var candidates:Tllama_token_data_array; var last_tokens:Tllama_token; last_tokens_size:Tsize_t; alpha_frequency:single; 
              alpha_presence:single);LLAMACALL;external External_library name 'llama_sample_frequency_and_presence_penalties';

  procedure llama_sample_classifier_free_guidance(ctx:Pllama_context; var candidates:Tllama_token_data_array; var guidance_ctx:Tllama_context; scale:single);LLAMACALL;external External_library name 'llama_sample_classifier_free_guidance';

  procedure llama_sample_softmax(ctx:Pllama_context; var candidates:Tllama_token_data_array);LLAMACALL;external External_library name 'llama_sample_softmax';

  procedure llama_sample_top_k(ctx:Pllama_context; candidates:Pllama_token_data_array; k:longint; min_keep:Tsize_t);LLAMACALL;external External_library name 'llama_sample_top_k';

  procedure llama_sample_top_p(ctx:Pllama_context; candidates:Pllama_token_data_array; p:single; min_keep:Tsize_t);LLAMACALL;external External_library name 'llama_sample_top_p';

  procedure llama_sample_tail_free(ctx:Pllama_context; candidates:Pllama_token_data_array; z:single; min_keep:Tsize_t);LLAMACALL;external External_library name 'llama_sample_tail_free';

  procedure llama_sample_typical(ctx:Pllama_context; candidates:Pllama_token_data_array; p:single; min_keep:Tsize_t);LLAMACALL;external External_library name 'llama_sample_typical';

  procedure llama_sample_temperature(ctx:Pllama_context; candidates:Pllama_token_data_array; temp:single);LLAMACALL;external External_library name 'llama_sample_temperature';


  procedure llama_sample_grammar(ctx:Pllama_context; var candidates:Tllama_token_data_array; var grammar:Tllama_grammar);LLAMACALL;external External_library name 'llama_sample_grammar';

  function llama_sample_token_mirostat(ctx:Pllama_context; var candidates:Tllama_token_data_array; tau:single; eta:single; m:longint; 
             var mu:single):Tllama_token;LLAMACALL;external External_library name 'llama_sample_token_mirostat';

  function llama_sample_token_mirostat_v2(ctx:Pllama_context; var candidates:Tllama_token_data_array; tau:single; eta:single; var mu:single):Tllama_token;LLAMACALL;external External_library name 'llama_sample_token_mirostat_v2';

  function llama_sample_token_greedy(ctx:Pllama_context; candidates:Pllama_token_data_array):Tllama_token;LLAMACALL;external External_library name 'llama_sample_token_greedy';

  function llama_sample_token(ctx:Pllama_context; candidates:Pllama_token_data_array):Tllama_token;LLAMACALL;external External_library name 'llama_sample_token';

  procedure llama_grammar_accept_token(ctx:Pllama_context; var grammar:Tllama_grammar; token:Tllama_token);LLAMACALL;external External_library name 'llama_grammar_accept_token';



  type
    Tllama_beam_view = record
        tokens : ^Tllama_token;
        n_tokens : Tsize_t;
        p : single;
        eob : TBool;
      end;

    Tllama_beams_state = record
        beam_views : ^Tllama_beam_view;
        n_beams : Tsize_t;
        common_prefix_length : Tsize_t;
        last_call : TBool;
      end;


    Tllama_beam_search_callback_fn_t = procedure (callback_data:pointer; para2:Tllama_beams_state);LLAMACALL;

  procedure llama_beam_search(ctx:Pllama_context; callback:Tllama_beam_search_callback_fn_t; callback_data:pointer; n_beams:Tsize_t; n_past:longint; 
              n_predict:longint; n_threads:longint);LLAMACALL;external External_library name 'llama_beam_search';

  {   struct llama_timings llama_get_timings(struct llama_context * ctx); }
  procedure llama_print_timings(ctx:Pllama_context);LLAMACALL;external External_library name 'llama_print_timings';

  procedure llama_reset_timings(ctx:Pllama_context);LLAMACALL;external External_library name 'llama_reset_timings';


  function llama_print_system_info:Pchar;LLAMACALL;external External_library name 'llama_print_system_info';

  procedure llama_log_set(log_callback:Tllama_log_callback; user_data:pointer);LLAMACALL;external External_library name 'llama_log_set';


implementation


end.
