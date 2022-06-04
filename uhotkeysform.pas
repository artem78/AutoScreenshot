unit uHotKeysForm;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  uUtilsMore;

type

  { THotKeysForm }

  THotKeysForm = class(TForm)
    ButtonPanel: TButtonPanel;
    StartStopAutoCaptureKeyComboBox: TComboBox;
    StartStopAutoCaptureAltCheckBox: TCheckBox;
    StartStopAutoCaptureShiftCheckBox: TCheckBox;
    StartStopAutoCaptureCtrlCheckBox: TCheckBox;
    StartStopAutoCaptureLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    procedure SetStartStopAutoCaptureKey(const AKey: THotKey);
    function GetStartStopAutoCaptureKey: THotKey;
    procedure FillKeysComboBox;
  public
    property StartStopAutoCaptureKey: THotKey read GetStartStopAutoCaptureKey
                         write SetStartStopAutoCaptureKey;
  end;

var
  HotKeysForm: THotKeysForm;

implementation

uses
  LCLType, LCLProc;

{$R *.lfm}

{ THotKeysForm }

procedure THotKeysForm.FormCreate(Sender: TObject);
begin
  FillKeysComboBox;
end;

procedure THotKeysForm.SetStartStopAutoCaptureKey(const AKey: THotKey);
var
  Idx: Integer;
begin
  StartStopAutoCaptureAltCheckBox.Checked := ssAlt in AKey.ShiftState;
  StartStopAutoCaptureCtrlCheckBox.Checked := ssCtrl in AKey.ShiftState;
  StartStopAutoCaptureShiftCheckBox.Checked := ssShift in AKey.ShiftState;

  Idx := StartStopAutoCaptureKeyComboBox.Items.IndexOfObject(TObject(Integer(AKey.Key)));
  StartStopAutoCaptureKeyComboBox.ItemIndex := Idx;
end;

function THotKeysForm.GetStartStopAutoCaptureKey: THotKey;
var
  SelIdx: Integer;
begin
  Result.ShiftState := [];

  if StartStopAutoCaptureAltCheckBox.Checked then
    Include(Result.ShiftState, ssAlt);

  if StartStopAutoCaptureCtrlCheckBox.Checked then
    Include(Result.ShiftState, ssCtrl);

  if StartStopAutoCaptureShiftCheckBox.Checked then
    Include(Result.ShiftState, ssShift);

  SelIdx := StartStopAutoCaptureKeyComboBox.ItemIndex;
  Result.Key := Integer(StartStopAutoCaptureKeyComboBox.Items.Objects[SelIdx]);
end;

procedure THotKeysForm.FillKeysComboBox;
var
  Key: Word;
  KeyName: String;
begin
  StartStopAutoCaptureKeyComboBox.Clear;
  StartStopAutoCaptureKeyComboBox.Items.BeginUpdate;

  for Key := VK_BACK to VK_SCROLL do
  // VK_BROWSER_BACK to VK_OEM_CLEAR
  begin
    KeyName := KeyAndShiftStateToKeyString(Key, []);
    if not KeyStringIsIrregular(KeyName) then
      StartStopAutoCaptureKeyComboBox.Items.AddObject(KeyName, TObject(Integer(Key)));
  end;

  StartStopAutoCaptureKeyComboBox.Items.EndUpdate;
end;

end.

