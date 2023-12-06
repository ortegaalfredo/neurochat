{
Copyright (C) Miguel A. Risco-Castillo

FPC-markdown is a fork of Grahame Grieve <grahameg@gmail.com>
Delphi-markdown https://github.com/grahamegrieve/delphi-markdown

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
}

Unit MarkdownProcessor;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, MarkdownUtils;

Type

  { TMarkdownProcessor }

  TMarkdownProcessor = {abstract} class
  private
    FConfig: TConfiguration;
  protected
    function GetUnSafe: boolean; virtual; abstract;
    procedure SetUnSafe(const Value: boolean); virtual; abstract;
  public
    class function CreateDialect(dialect : TMarkdownDialect) : TMarkdownProcessor;
    function process(source : String) : String; virtual; abstract;
    property config: TConfiguration read FConfig write FConfig;
    // when Unsafe = true, then the processor can create scripts etc.
    property UnSafe : boolean read GetUnSafe write SetUnSafe;
  end;

implementation

uses
  MarkdownDaringFireball,
  MarkdownCommonMark,
  MarkdownTxtMark;

{ TMarkdownProcessor }

class function TMarkdownProcessor.CreateDialect(dialect: TMarkdownDialect): TMarkdownProcessor;
begin
  case dialect of
    mdDaringFireball : result := TMarkdownDaringFireball.Create;
    mdCommonMark : result := TMarkdownCommonMark.Create;
    mdTxtMark : result := TMarkdownTxtMark.Create;
  else
    raise Exception.Create('Unknown Markdown dialect');
  end;
end;

end.
