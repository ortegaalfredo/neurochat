unit Chat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,HtmlView, Controls,HtmlGlobals,llama,request,MarkdownProcessor, MarkdownUtils,OptionsForm,StrUtils,
  fpjson, jsonparser,lclintf;

type
     { Class representing a Personality object related to AI prompts }
     TPersonality = class
     public
       { Property storing the personality name }
       Name: String;
       { Properties defining pre-prompt and end-prompt strings }
       preprompt: String;
       endprompt: String;
       { Constructor initializing new instances of TPersonality }
       constructor Create(const pprompt: String; const eprompt: String);
       { Static method to create a TPersonality instance from JSON data }
       class function FromJSON(LJSONObject: TJSONObject): TPersonality; static;
       { Method serializing current TPersonality state into JSON format }
       function ToJSON: TJSONObject;
     end;

     // Enum representing different types of AI services.
     AIType = (AIT_Neuroengine = 1, AIT_LlamaCPP = 2, AIT_ChatGPT = 3);
     { TChat }
     TChat = class
       public
         // Properties
         HtmlViewer: THtmlViewer; // HTML viewer component to display the chat.
         ServiceName: String; // Name of the AI service.
         SearchIndex: Integer; // Index for searching through chat history.
         // Llama.cpp specific properties
         llamagguf: Pllama_model; // Pointer to the Llama model.
         Params: Tllama_model_params; // Parameters for the Llama model.

         ServiceType: AIType; // Type of the AI service.
         Personality: TPersonality; // Personality of the AI service.
         Chatlines: TStringList; // List to store chat lines.
         outhtml: TStringList; // List to store HTML output.
         max_context_len: Integer; // Maximum context length for the chat.
         color: String; // Color of the chat background.
         requestThread: TRequestThread; // Thread for making requests.
         UpdateTokenCallback: TUpdate_token_callback; // Callback for updating tokens.
         // Constructors
         constructor Create(const AServiceName: string; sAItype: AIType;backgroundcolor:String = '#EBF5FB';maxContextlen:Integer = 512);
         // Methods
         procedure terminateRequestThread(); // Terminates the request thread.
         procedure createRequestThread(); // Creates a new request thread.
         function buildHtmlChat(chat: Tstrings) : Unicodestring; // Builds HTML chat from the given chat strings.
         procedure refreshHtml(); // Refreshes the HTML viewer with the latest chat.
         {Utility json functions}
         function SerializeStringListToJsonArray(stringList: TStringList): TJSONArray; // Serializes a string list to a JSON array.
         function DeserializeJsonArrayStringToStringList(jsonArray: TJSONArray): TStringList;  // Deserializes a JSON array to a string list.
         {Save and load object as JSON}
         function toJson():TJSONObject; // Converts the TChat object to a JSON object.
         class function FromJSON(LJSONObject: TJSONObject): TChat; // Creates a new TChat object from a JSON object.
         procedure OnHotSpotClickEvent(Sender: TObject; const SRC: ThtString; var Handled: Boolean); // Event handler for hot spot click event.
       end;

     { TNeuroengineService - This is a record type that encapsulates the configuration for a Neuroengine AI service in the chat system. }
     TNeuroengineService = record
       name: String; // Name of the Neuroengine service.
       prePrompt: String; // Prompt to be added before the user's message.
       endPrompt: String; // Prompt to be added after the user's message.
       oper: String; // Operator of the service
       comment: String; // Comment or description for the Neuroengine service.
     end;


var
    md : TMarkdownProcessor;
implementation

{TPersonality}

constructor TPersonality.Create(const pprompt:String;const eprompt:String);
begin
  self.preprompt:=pprompt;
  self.endprompt:=eprompt;
  self.name:='assistant';
end;

{ ToJSON - Converts the current TPersonality instance into a JSON object.
           The resulting JSON object contains three properties: 'name', 'preprompt', and 'endprompt'. }
function TPersonality.ToJSON: TJSONObject;
var
  LJSONObject: TJSONObject;
begin
  LJSONObject := TJSONObject.Create;
  try
    LJSONObject.Add('name', name);
    LJSONObject.Add('preprompt', preprompt);
    LJSONObject.Add('endprompt', endprompt);
    Result := LJSONObject;
  finally
  end;
end;

{ FromJSON - Creates a new TPersonality instance by parsing the provided JSON object.
              If successful, the method initializes the 'name', 'preprompt', and 'endprompt' properties based on the contents of the JSON object. }
class function TPersonality.FromJSON(LJSONObject: TJSONObject): TPersonality;
begin
  Result := TPersonality.Create('', ''); // Default values, you may adjust as needed
  try
    //LJSONObject := GetJSON(json) as TJSONObject;

    if Assigned(LJSONObject) then
    begin
      Result.Name := LJSONObject.Get('name', Result.Name);
      Result.preprompt := LJSONObject.Get('preprompt', Result.preprompt);
      Result.endprompt := LJSONObject.Get('endprompt', Result.endprompt);
    end;
  finally
  end;
end;


{ TChat }

{ TChat.Create - Initializes a new TChat instance with the specified settings}
constructor TChat.Create(const AServiceName: string; sAItype: AIType;backgroundcolor:String;maxContextlen:Integer);
var
  endprompt,preprompt:string;
begin
if (md=nil) then
    begin
    md:= TMarkdownProcessor.createDialect(mdTxtMark);
    md.UnSafe:=True;
    end;
self.ServiceName:=AServiceName;
self.ServiceType:=sAItype;
self.Chatlines:= TStringList.Create;
self.outhtml:=TStringList.Create;
self.requestThread:=Nil;
self.max_context_len:=maxContextlen;
{Default personality: Assistant}
preprompt:='A chat between a curious user and an assistant.';
endprompt:='Assistant: ';
self.color:=backgroundcolor;
self.Personality:=TPersonality.Create(preprompt,endprompt);
self.SearchIndex:=0;
self.UpdateTokenCallback:=nil;
end;

{ TChat.toJson - Serializes the current TChat instance into a JSON object containing various properties like ServiceName, ServiceType, Color, Personality, ChatLines, etc. }
function TChat.toJson: TJSONObject;
var
  LJSONObject, Perso: TJSONObject;
  Chats: TJSONArray;
begin
  LJSONObject := TJSONObject.Create;
  try
    Perso := self.Personality.ToJSON;
    Chats := self.SerializeStringListToJsonArray(self.Chatlines);
    LJSONObject.Add('Name', self.ServiceName);
    LJSONObject.Add('type', Ord(self.ServiceType));
    LJSONObject.Add('color', self.Color);
    LJSONObject.Add('personality', Perso);
    LJSONObject.Add('chatlines', Chats);
    Result := LJSONObject;
  finally
  end;
end;

{ TChat.FromJSON - Deserializes a given JSON object back into a TChat instance
  along with all associated data including ServiceName, ServiceType, Color,
  Personality, ChatLines, etc. }
class function TChat.FromJSON(LJSONObject: TJSONObject): TChat;
var
  Name,cColor,line:String;
  c,I:Integer;
  ctype: AIType;
  jPersonality: TJSONObject;
  jChatlines: TJSONArray;
  sChatlines:TStringList;
begin
  Result:=nil;
  jChatlines:=nil;
  jPersonality:=nil;
  Name := LJSONObject.Get('Name', '');
  cColor := LJSONObject.Get('color', '');
  C := LJSONObject.Get('type', 0);
  jPersonality := LJSONObject.Get('personality',jPersonality);
  jChatlines := LJSONObject.Get('chatlines',jChatlines);
  if jPersonality=nil then
      exit;
    case C of
      1: ctype:=AIT_Neuroengine;
      2: ctype:=AIT_LlamaCPP;
      3: ctype:=AIT_ChatGPT;
      else
        exit;
    end;
  Result:=TChat.Create(Name,ctype,cColor);
  Result.Personality:=TPersonality.FromJSON(jPersonality);
  sChatlines:=Result.DeserializeJsonArrayStringToStringList(jChatlines);
  Result.Chatlines.AddStrings(sChatlines);
  for I:=0 to sChatlines.Count-1 do
      begin
      line:=sChatlines[I];
      if StartsStr('User: ',line) then
           Result.outhtml.Add('### '+ line)
      else Result.outhtml.Add(line);
      end;
  sChatlines.Free;
end;

procedure TChat.terminateRequestThread();
begin
requestThread.Terminate;
requestThread.Free;
end;

{ TChat.createRequestThread - Creates a specific type of Request Thread based on the Service Type property of the TChat instance.
This could be Neuroengine, LlamaCPP, or ChatGPT threads. Each one handles different service requests accordingly. }
procedure TChat.createRequestThread();
begin
  Case Self.ServiceType of
    AIT_Neuroengine:
      RequestThread := TRequestThread.Create(Self.ServiceName, Self.Max_context_len);
    AIT_LlamaCPP:
      RequestThread := TllamaCPPThread.Create(Self.ServiceName, Self.llamagguf, Self.Params, Self.Max_context_len);
    AIT_ChatGPT:
      RequestThread := TChatGPTThread.Create(Self.ServiceName, Settings.LabeledEditApiKey.Text, Self.Max_context_len);
  end;
end;

{ EscapeHtml - Replaces special characters (<, >, &) within a provided HTML-encoded string using their respective character entities (&lt;, &gt;, &amp;).
Enhances security by preventing cross-site scripting attacks when rendering untrusted user inputs. }
function EscapeHtml(const Input: string): string;
var
  CharIndex: Integer;
  CurrentChar: Char;
begin
  Result := '';

  for CharIndex := 1 to Length(Input) do
  begin
    CurrentChar := Input[CharIndex];

    case CurrentChar of
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '&': Result := Result + '&amp;';
    else
      Result := Result + CurrentChar;
    end;
  end;
end;

// Generates HTML representation of a given chat log stored in a TStrings object.
function TChat.buildHtmlChat(chat: Tstrings) : Unicodestring;
var
  q:Integer;
  str:Unicodestring;
begin
Result:='';
for q:=0 to chat.Count-1 do
    begin
    str:=chat.Strings[q];
    if StartsStr('### User: ',str) then
        Result:=Result+'<tr style="align: center;background-color: '+color+';padding: 0px"><td><pre>'+EscapeHtml(RightStr(chat.Strings[q],length(chat.Strings[q])-10))+'</pre></td></tr>'
    else
      if StartsStr('<SYSTEM>',chat.Strings[q]) then
          Result:=Result+'<tr style="color:#808080;font-size: 80%;"><td>'+chat.Strings[q]+'</td></tr>'
      else
        Result:=Result+'<tr><td style="border: none; padding: 50px;">'+md.process(chat.Strings[q])+'</td></tr>';
    end;
Result := '<html><head><meta charset="UTF-8"></head><body style="background-color:white"><table style="width: 100%">'+Result+'</table></body></html>';
end;

// Opens a hyperlink passed through the SRC parameter from the sender HTMLViewer.
procedure TChat.OnHotSpotClickEvent(Sender: TObject; const SRC: ThtString; var Handled: Boolean);
begin
OpenURL(SRC);
Handled:=True;
end;

{ Refreshes the HTML viewer with updated content generated by buildHtmlChat() method.
Assigns the OnHotSpotClickEvent handler to manage click events on active hotspots. }
procedure TChat.refreshHtml();
var
  html : Unicodestring;
begin
html := self.buildHtmlChat(self.outhtml);
self.HTMLViewer.LoadCursor:=crDefault;
self.HtmlViewer.LoadFromString(html,self.ServiceName);
self.HTMLViewer.OnHotSpotClick:=@OnHotSpotClickEvent;
end;

{
  Converts a given TStringList instance to a TJSONArray containing all strings within the list. Returns the resulting TJSONArray.
}
function TChat.SerializeStringListToJsonArray(stringList: TStringList): TJSONArray;
var
  jsonArray: TJSONArray;
  i:Integer;
begin
  // Create a JSON array
  jsonArray := TJSONArray.Create;

  try
    // Convert the TStringList to a JSON array
    for i:=0 to stringList.Count-1 do
        jsonArray.Add(stringList[i]);
    Result := jsonArray;
  finally
  end;
end;

{
  Deserializes a JSON array (passed via the jsonArray argument) into a TStringList instance.
  Frees up memory used by the JSON array before returning the populated TStringList.
}
function TChat.DeserializeJsonArrayStringToStringList(jsonArray: TJSONArray): TStringList;
var
  I:Integer;
begin
  Result := TStringList.Create;

  try
    if Assigned(jsonArray) then
    begin
      // Populate the TStringList with the strings from the JSON array
      for I := 0 to jsonArray.Count -1 do
            Result.Add(jsonArray.Items[I].AsString);
    end;
  finally
    // Free the JSON array
    jsonArray.Free;
  end;
end;

end.

