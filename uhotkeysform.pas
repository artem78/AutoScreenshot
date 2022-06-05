unit uHotKeysForm;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  uUtilsMore;

type

  { THotKeyControl }

  THotKeyControl = class(TCustomControl)
  private
    // Embeded components
    AltCheckBox: TCheckBox;
    CtrlCheckBox: TCheckBox;
    ShiftCheckBox: TCheckBox;
    KeyComboBox: TComboBox;

    procedure SetHotKey(const AHotKey: THotKey);
    function GetHotKey: THotKey;

    procedure FillKeyComboBox;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property HotKey: THotKey read GetHotKey write SetHotKey;
  end;

  { THotKeysForm }

  THotKeysForm = class(TForm)
    ButtonPanel: TButtonPanel;
    procedure FormCreate(Sender: TObject);
  private
    StartStopAutoCaptureLabel: TLabel;
    StartStopAutoCaptureHotKeyControl: THotKeyControl;

    procedure SetStartStopAutoCaptureKey(const AHotKey: THotKey);
    function GetStartStopAutoCaptureKey: THotKey;
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

{ THotKeyControl }

procedure THotKeyControl.SetHotKey(const AHotKey: THotKey);
var
  Idx: Integer;
begin
  AltCheckBox.Checked := ssAlt in AHotKey.ShiftState;
  CtrlCheckBox.Checked := ssCtrl in AHotKey.ShiftState;
  ShiftCheckBox.Checked := ssShift in AHotKey.ShiftState;

  Idx := KeyComboBox.Items.IndexOfObject(TObject(Integer(AHotKey.Key)));
  KeyComboBox.ItemIndex := Idx;
end;

function THotKeyControl.GetHotKey: THotKey;
var
  SelIdx: Integer;
begin
  Result.ShiftState := [];

  if AltCheckBox.Checked then
    Include(Result.ShiftState, ssAlt);

  if CtrlCheckBox.Checked then
    Include(Result.ShiftState, ssCtrl);

  if ShiftCheckBox.Checked then
    Include(Result.ShiftState, ssShift);

  SelIdx := KeyComboBox.ItemIndex;
  Result.Key := Integer(KeyComboBox.Items.Objects[SelIdx]);
end;

procedure THotKeyControl.FillKeyComboBox;
var
  Key: Word;
  KeyName: String;
begin
  KeyComboBox.Clear;
  KeyComboBox.Items.BeginUpdate;

  for Key := VK_BACK to VK_SCROLL do
  // VK_BROWSER_BACK to VK_OEM_CLEAR
  begin
    KeyName := KeyAndShiftStateToKeyString(Key, []);
    if not KeyStringIsIrregular(KeyName) then
      KeyComboBox.Items.AddObject(KeyName, TObject(Integer(Key)));
  end;

  KeyComboBox.Items.EndUpdate;
end;

constructor THotKeyControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do   // ???
    SetInitialBounds(0, 0, CX, CY);

  AltCheckBox := TCheckBox.Create(Self);
  with AltCheckBox do
  begin
    Parent := Self;
    //SetSubComponent(true);
    Caption := 'Alt';
    //Name := '...';
  end;

  CtrlCheckBox := TCheckBox.Create(Self);
  with CtrlCheckBox do
  begin
    Parent := Self;
    //SetSubComponent(true);
    Caption := 'Ctrl';
    //Name := '...';
  end;

  ShiftCheckBox := TCheckBox.Create(Self);
  with ShiftCheckBox do
  begin
    Parent := Self;
    //SetSubComponent(true);
    Caption := 'Shift';
    //Name := '...';
  end;

  KeyComboBox := TComboBox.Create(Self);
  with KeyComboBox do
  begin
    Parent := Self;
    //SetSubComponent(true);
    Constraints.MinWidth := 100;
    //Name := '...';
    Style := csDropDownList;
    //AutoSize := True;
  end;
    FillKeyComboBox;

  ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  ChildSizing.ControlsPerLine := 4;
  ChildSizing.HorizontalSpacing := 8;
  AutoSize := True;

end;

destructor THotKeyControl.Destroy;
begin
  //KeyComboBox.Free;
  //ShiftCheckBox.Free;
  //CtrlCheckBox.Free;
  //AltCheckBox.Free;

  inherited Destroy;
end;

{ THotKeysForm }

procedure THotKeysForm.FormCreate(Sender: TObject);
begin
  StartStopAutoCaptureLabel := TLabel.Create(Self);
  with StartStopAutoCaptureLabel do
  begin
    Caption := 'Start/stop auto capture:';
    Parent := Self;
  end;

  StartStopAutoCaptureHotKeyControl := THotKeyControl.Create(Self);
  with StartStopAutoCaptureHotKeyControl do
  begin
    Parent := Self;
  end;
end;

procedure THotKeysForm.SetStartStopAutoCaptureKey(const AHotKey: THotKey);
begin
  StartStopAutoCaptureHotKeyControl.HotKey := AHotKey;
end;

function THotKeysForm.GetStartStopAutoCaptureKey: THotKey;
begin
  Result := StartStopAutoCaptureHotKeyControl.HotKey;
end;

end.

