; -- Example1.iss --
; Demonstrates copying 3 files and creating an icon.

; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

[Setup]
AppName=Neurochat
AppVersion=0.3-dev
WizardStyle=modern
DefaultDirName={autopf}\Neurochat
DefaultGroupName=Neurochat
UninstallDisplayIcon={app}\Neurochat.exe
Compression=lzma2
SolidCompression=yes
OutputDir=c:\ai\neurochat

[Files]
Source: "..\..\Neurochat.exe"; DestDir: "{app}"
Source: "..\..\llama.dll"; DestDir: "{app}"
Source: "..\..\libeay32.dll"; DestDir: "{app}"
Source: "..\..\ssleay32.dll"; DestDir: "{app}"

[Icons]
Name: "{group}\Neurochat"; Filename: "{app}\Neurochat.exe"
