FPC-markdown
============

Markdown Processor for FPC. 

Basic Information
-----------------

This is a Pascal (FPC) library that processes markdown to HTML.
At present the following dialects of markdown are supported:

* The Daring Fireball dialect
 (see <https://daringfireball.net/projects/markdown/>)

* Enhanced TxtMark dialect
 (translated from <https://github.com/rjeschke/txtmark>)

* Initial support for CommonMark dialect
 (translated from <http://commonmark.org/>)

Wishlist: PEGDown (Github dialect), CommonMark, etc.

All you need to use the library is FPC version 3.0.4 or newer.


Using the Library
-----------------

Declare a variable of the class TMarkdownProcessor:

     var
       md : TMarkdownProcessor;

Create a TMarkdownProcessor (MarkdownProcessor.pas) of the dialect you want:

       md := TMarkdownProcessor.createDialect(mdDaringFireball)
  
Decide whether you want to allow active content

       md.UnSafe := true;
  
Note: you should only set this to true if you *need* to - active content can be a significant safety/security issue.  
 
Generate HTML fragments from Markdown content:

       html := md.process(markdown); 
  
Note that the HTML returned is an HTML fragment, not a full HTML page.  
  
Do not forget to dispose of the object after the use:

       md.free

Examples
--------
The demo folder contains two demonstrative [Lazarus](http://www.lazarus-ide.org/) programs,
the most advanced, `MarkdownHTML`, needs the [HTMLViewer](https://github.com/BerndGabriel/HtmlViewer) control.

License
-------
Copyright (C) Miguel A. Risco-Castillo

FPC-markdown implementation is a fork of Grahame Grieve pascal port
[Delphi-markdown](https://github.com/grahamegrieve/delphi-markdown)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.


