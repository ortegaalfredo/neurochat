unit OptionsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons,
  StdCtrls, ExtCtrls,IniFiles;

type

  { TSettings }

  TSettings = class(TForm)
    BitBtnOK: TBitBtn;
    BitBtnCancel: TBitBtn;
    CheckBoxGPUOffloadCache: TCheckBox;
    CheckBoxGPU: TCheckBox;
    CheckBoxAutosave: TCheckBox;
    ComboBoxThreads: TComboBox;
    ComboBoxModels: TComboBox;
    ComboBoxGPULayers: TComboBox;
    EditPromptPersonalityName: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelGPU: TLabel;
    LabeledEditTemperature: TLabeledEdit;
    LabeledEditK: TLabeledEdit;
    LabeledEditP: TLabeledEdit;
    LabeledEditMaxLen: TLabeledEdit;
    LabeledEditApiKey: TLabeledEdit;
    MemoPersonalityDescription: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure BitBtnOKClick(Sender: TObject);
    procedure CheckBoxGPUChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    IniFile: TIniFile;
  end;

var
  Settings: TSettings;

implementation

{$R *.lfm}

{ TSettings }
procedure TSettings.FormCreate(Sender: TObject);
begin
  IniFile := TIniFile.Create(GetUserDir+'/.neurochat.ini');
  try
    self.CheckBoxAutosave.Checked:=IniFile.ReadBool('Common','autosave',True);
    self.LabeledEditTemperature.Text:=FloatToStr(IniFile.ReadFloat('Common','temperature',1.0));
    self.LabeledEditK.Text:=IntToStr(IniFile.ReadInteger('Common','top_k',40));
    self.LabeledEditP.Text:=FloatToStr(IniFile.ReadFloat('Common','top_p',0.95));
    self.LabeledEditMaxLen.Text:=IntToStr(IniFile.ReadInteger('Common','max_new_len',1024));

    self.LabeledEditApiKey.Text:=IniFile.ReadString('ChatGPT-API', 'api-key','');
    self.ComboBoxModels.Text:=IniFile.ReadString('ChatGPT-API', 'custom-model','');

    self.CheckBoxGPU.Checked:=IniFile.ReadBool('local-ai','gpu',False);
    self.ComboBoxThreads.Text:=IntToStr(IniFile.ReadInteger('local-ai','threads',4));
    self.ComboBoxGPULayers.Text:=IntToStr(IniFile.ReadInteger('local-ai','gpulayers',2048));
    self.CheckBoxGPUOffloadCache.Checked:=IniFile.ReadBool('local-ai','gpucache',False);
    self.CheckBoxGPUChange(nil);

    self.EditPromptPersonalityName.Text:=IniFile.ReadString('Custom-Personality', 'custom-name',self.EditPromptPersonalityName.Text);
    self.MemoPersonalityDescription.Text:=IniFile.ReadString('Custom-Personality', 'custom-description',self.MemoPersonalityDescription.Text);
  finally
    IniFile.Free;
  end;
end;

procedure TSettings.BitBtnOKClick(Sender: TObject);
begin
    IniFile := TIniFile.Create(GetUserDir+'/.neurochat.ini');
  try
    IniFile.WriteBool('Common','autosave',self.CheckBoxAutosave.Checked);
    IniFile.WriteFloat('Common','temperature',StrToFloatDef(self.LabeledEditTemperature.Text,1.0));
    IniFile.WriteInteger('Common','top_k',StrToIntDef(self.LabeledEditK.Text,40));
    IniFile.WriteFloat('Common','top_p',StrToFloatDef(self.LabeledEditP.Text,0.95));
    IniFile.WriteInteger('Common','max_new_len',StrToIntDef(self.LabeledEditMaxLen.Text,1024));

    IniFile.WriteString('ChatGPT-API', 'api-key', self.LabeledEditApiKey.Text);
    IniFile.WriteString('ChatGPT-API', 'custom-model',self.ComboBoxModels.Text);

    IniFile.WriteBool('local-ai','gpu',self.CheckBoxGPU.Checked);
    IniFile.WriteInteger('local-ai','threads',StrToIntDef(self.ComboBoxThreads.Text,4));
    IniFile.WriteInteger('local-ai','gpulayers',StrToIntDef(self.ComboBoxGPULayers.Text,2048));
    IniFile.WriteBool('local-ai','gpucache',self.CheckBoxGPUOffloadCache.Checked);

    IniFile.WriteString('Custom-Personality', 'custom-name', self.EditPromptPersonalityName.Text);
    IniFile.WriteString('Custom-Personality', 'custom-description', self.MemoPersonalityDescription.Text);
  finally
    IniFile.Free;
  end;
end;

procedure TSettings.CheckBoxGPUChange(Sender: TObject);
begin
if CheckBoxGPU.State=cbChecked then
   begin
   self.CheckBoxGPUOffloadCache.Enabled:=True;
   self.ComboBoxGPULayers.Enabled:=True;
   self.LabelGPU.Enabled:=True;
   end
else
    begin
    self.CheckBoxGPUOffloadCache.Enabled:=False;
    self.ComboBoxGPULayers.Enabled:=False;
    self.LabelGPU.Enabled:=False;
    end
end;

end.

