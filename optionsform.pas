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
    CheckBoxAutosave: TCheckBox;
    CheckBoxGPU: TCheckBox;
    ComboBoxThreads: TComboBox;
    ComboBoxModels: TComboBox;
    EditPromptPersonalityName: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
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

    self.ComboBoxThreads.Text:=IntToStr(IniFile.ReadInteger('local-ai','threads',4));
    self.CheckBoxGPU.Checked:=IniFile.ReadBool('local-ai','gpu',False);

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

    IniFile.WriteInteger('local-ai','threads',StrToIntDef(self.ComboBoxThreads.Text,4));
    IniFile.WriteBool('local-ai','gpu',self.CheckBoxGPU.Checked);

    IniFile.WriteString('Custom-Personality', 'custom-name', self.EditPromptPersonalityName.Text);
    IniFile.WriteString('Custom-Personality', 'custom-description', self.MemoPersonalityDescription.Text);
  finally
    IniFile.Free;
  end;
end;

end.

