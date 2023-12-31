unit neuroengineapi;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, fpjson, jsonparser,fphttpclient;

Type
  Pchar  = ^char;

    function QueryAI(LLMName: String;Prompt: UnicodeString;temperature: Double;top_p: Double;top_k:Double;repetition_penalty:Double;max_new_len:Integer;seed:Integer;raw:Boolean): UnicodeString;
    function QueryAPI(LLMName: String;APIMessage: String):UnicodeString;

implementation


{ Neuroengine API implementation}

// Constructs and sends an API request to the designated LLMName endpoint while handling exceptions and returning the decoded JSON response.
function QueryAPI(LLMName: String;APIMessage: String):UnicodeString;
var
  URL: string;
  RawByteStr: RawByteString;
  JSONData: TJSONData;
  RegularStr: String;
begin
URL := format('https://api.neuroengine.ai/%s',[LLMName]); // Neuroengine endpoint

try
  // Create an HTTP client and set up the request
  RawByteStr := TFPHTTPClient.SimpleFormPost(URL,APIMessage);
  RegularStr := RawByteStr;
  // Parse the regular string as JSON data
  JSONData := GetJSON(RegularStr);
  QueryAPI:=JSONData.AsJSON;
  JSONData.Free;
except
//  on E: Exception do
  QueryAPI:='';
end;

end;

// Escapes special characters within a given Unicode string by replacing them with their respective escape sequences.
function EscapeString(const AValue: Unicodestring): Unicodestring;
const
  ESCAPE = '\';
  QUOTATION_MARK = '"';
  REVERSE_SOLIDUS = '\';
  SOLIDUS = '/';
  BACKSPACE = #8;
  FORM_FEED = #12;
  NEW_LINE = #10;
  CARRIAGE_RETURN = #13;
  HORIZONTAL_TAB = #9;
var
  AChar: Char;
begin
  Result := '';
  for AChar in AValue do
  begin
    case AChar of
      QUOTATION_MARK: Result := Result + ESCAPE + QUOTATION_MARK;
      REVERSE_SOLIDUS: Result := Result + ESCAPE + REVERSE_SOLIDUS;
      SOLIDUS: Result := Result + ESCAPE + SOLIDUS;
      BACKSPACE: Result := Result + ESCAPE + 'b';
      FORM_FEED: Result := Result + ESCAPE + 'f';
      NEW_LINE: Result := Result + ESCAPE + 'n';
      CARRIAGE_RETURN: Result := Result + ESCAPE + 'r';
      HORIZONTAL_TAB: Result := Result + ESCAPE + 't';
      else
      begin
        if (Integer(AChar) < 32) or (Integer(AChar) > 126) then
          Result := Result + ESCAPE + 'u' + IntToHex(Integer(AChar), 4)
        else
          Result := Result + AChar;
      end;
    end;
  end;
end;

// Sends a query to a specific AI language model using provided parameters and returns the response as a raw string.
function QueryAI(LLMName: String;Prompt: UnicodeString;temperature: Double;top_p: Double;top_k:Double;repetition_penalty:Double;max_new_len:Integer;seed:Integer;raw:Boolean): UnicodeString;
var
  JSONToSend: Unicodestring;
  JSONData: TJSONData;
  rawString:String;
begin
     QueryAI:='';
     if (raw=True) then
        rawString:='"True"'
     else rawString:='"False"';
     Prompt:=EscapeString(Prompt);
     // Prepare the JSON data to send in the POST request
     JSONToSend := format('{"message": "%s",'+
                           '"temperature": %f,'+
                           '"top_p":%f,'+
                           '"top_k":%f,'+
                           '"repetition_penalty":%f,'+
                           '"max_new_len":%d,'+
                           '"seed":%d,'+
                           '"raw" :'+rawString+
                           '}'
                           ,[Prompt,temperature,top_p,top_k,repetition_penalty,max_new_len,seed]);
     QueryAI:=QueryAPI(LLMName,JSONToSend);
     if QueryAI='' then exit;
     JSONData := GetJSON(QueryAI);
     QueryAI:=TJSONObject(JSONData).Find('reply').AsUnicodeString;

end;

end.

