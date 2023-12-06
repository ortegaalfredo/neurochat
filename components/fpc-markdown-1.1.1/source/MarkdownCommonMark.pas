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

Unit MarkdownCommonMark;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TypInfo,
  MarkdownProcessor, MarkdownDaringFireball, MarkdownUtils;

Type

  TMarkdownCommonMark = class(TMarkdownDaringFireball)
  private
  protected
  public
    Constructor Create;
    Destructor Destroy; override;
    function process(source: String): String; override;
  end;

implementation


{ TMarkdownCommonMark }

constructor TMarkdownCommonMark.Create;
begin
  inherited;
  Config.Dialect:=mdCommonMark;
end;

destructor TMarkdownCommonMark.Destroy;
begin
  inherited;
end;

function TMarkdownCommonMark.process(source: String): String;
begin
  result:=inherited process(source);
end;


end.
