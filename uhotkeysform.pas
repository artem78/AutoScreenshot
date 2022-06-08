unit uHotKeysForm;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  ExtCtrls, uUtilsMore;

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
    Panel: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    Labels: array [0..2] of TLabel;
    HotKeyControls: array [0..2] of THotKeyControl;

    procedure SetStartAutoCaptureKey(const AHotKey: THotKey);
    function GetStartAutoCaptureKey: THotKey;
    procedure SetStopAutoCaptureKey(const AHotKey: THotKey);
    function GetStopAutoCaptureKey: THotKey;
    procedure SetSingleCaptureKey(const AHotKey: THotKey);
    function GetSingleCaptureKey: THotKey;

    function Validate(out AErrorMsg: string): Boolean;
  public
    property StartAutoCaptureKey: THotKey read GetStartAutoCaptureKey
                         write SetStartAutoCaptureKey;
    property StopAutoCaptureKey: THotKey read GetStopAutoCaptureKey
                         write SetStopAutoCaptureKey;
    property SingleCaptureKey: THotKey read GetSingleCaptureKey
                         write SetSingleCaptureKey;
  end;

var
  HotKeysForm: THotKeysForm;

implementation

uses
  LCLType, LCLProc;

{$R *.lfm}

const
  StartAutoCaptureIdx = 0;
  StopAutoCaptureIdx  = 1;
  SingleCaptureIdx    = 2;

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
  SelIdx := KeyComboBox.ItemIndex;
  if SelIdx = -1 then
    Result.Key := VK_UNKNOWN
  else
    Result.Key := Integer(KeyComboBox.Items.Objects[SelIdx]);

  Result.ShiftState := [];

  if Result.Key <> VK_UNKNOWN then
  begin // Not needed to set Alt/Shift/Ctrl state for empty key
    if AltCheckBox.Checked then
      Include(Result.ShiftState, ssAlt);

    if CtrlCheckBox.Checked then
      Include(Result.ShiftState, ssCtrl);

    if ShiftCheckBox.Checked then
      Include(Result.ShiftState, ssShift);
  end;
end;

procedure THotKeyControl.FillKeyComboBox;
var
  Key: Word;
  KeyName: String;
begin
  KeyComboBox.Clear;
  KeyComboBox.Items.BeginUpdate;

  KeyComboBox.Items.AddObject('(None)', TObject(Integer(VK_UNKNOWN)));

  for Key := VK_BACK to VK_SCROLL do
  // VK_BROWSER_BACK to VK_OEM_CLEAR
  begin
    case Key of
      VK_BACK: KeyName        := 'Backspace';
      VK_NUMPAD0..VK_NUMPAD9: KeyName := 'Numpad ' + IntToStr(Key - VK_NUMPAD0);
      VK_MULTIPLY: KeyName    := 'Numpad *';
      VK_ADD: KeyName         := 'Numpad +';
      VK_SEPARATOR: KeyName   := 'Numpad Separator'; // what is this?
      VK_SUBTRACT: KeyName    := 'Numpad -';
      VK_DECIMAL: KeyName     := 'Numpad . (Del)';
      VK_DIVIDE: KeyName      := 'Numpad /';
      VK_RETURN: KeyName      := 'Numpad Enter'

      else KeyName := KeyAndShiftStateToKeyString(Key, []);
    end;

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
    Constraints.MinWidth := 120;
    //Name := '...';
    Style := csDropDownList;
    //AutoSize := True; // ToDo: Do not work, fix
  end;
  FillKeyComboBox;

  ChildSizing.Layout := cclLeftToRightThenTopToBottom;
  ChildSizing.ControlsPerLine := 4;
  ChildSizing.HorizontalSpacing := 5;
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
var
  I: Integer;
  Texts: Array [0..2] of String;
begin
  Texts[StartAutoCaptureIdx] := 'Start auto capture';
  Texts[StopAutoCaptureIdx]  := 'Stop auto capture';
  Texts[SingleCaptureIdx]    := 'Single capture';

  for I := 0 to 2 do
  begin
    // Create label
    Labels[I] := TLabel.Create(Self);
    with Labels[I] do
    begin
      Caption := Texts[I] + ':';
      BorderSpacing.CellAlignHorizontal := ccaRightBottom;
      BorderSpacing.CellAlignVertical := ccaCenter;
      Parent := Self.Panel;
    end;

    // Create hotkey control
    HotKeyControls[I] := THotKeyControl.Create(Self);
    with HotKeyControls[I] do
    begin
      BorderSpacing.CellAlignVertical := ccaCenter;
      Parent := Self.Panel;
    end;
  end;
end;

procedure THotKeysForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  ErrorMsg: String;
begin
  CanClose := True;

  if ModalResult = mrOK then
  begin
    if not Validate(ErrorMsg) then
    begin
      CanClose := False;
      MessageDlg(ErrorMsg, {mtError} mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure THotKeysForm.SetStartAutoCaptureKey(const AHotKey: THotKey);
begin
  HotKeyControls[StartAutoCaptureIdx].HotKey := AHotKey;
end;

function THotKeysForm.GetStartAutoCaptureKey: THotKey;
begin
  Result := HotKeyControls[StartAutoCaptureIdx].HotKey;
end;

procedure THotKeysForm.SetStopAutoCaptureKey(const AHotKey: THotKey);
begin
  HotKeyControls[StopAutoCaptureIdx].HotKey := AHotKey;
end;

function THotKeysForm.GetStopAutoCaptureKey: THotKey;
begin
  Result := HotKeyControls[StopAutoCaptureIdx].HotKey;
end;

procedure THotKeysForm.SetSingleCaptureKey(const AHotKey: THotKey);
begin
  HotKeyControls[SingleCaptureIdx].HotKey := AHotKey;
end;

function THotKeysForm.GetSingleCaptureKey: THotKey;
begin
  Result := HotKeyControls[SingleCaptureIdx].HotKey;
end;

function THotKeysForm.Validate(out AErrorMsg: string): boolean;
  function HasDuplicates: Boolean;
  var
    I1, I2: Integer;
  begin
    Result := False;

    for I1 := Low(HotKeyControls) + 1 to High(HotKeyControls) do
    begin
      for I2 := Low(HotKeyControls) to I1 - 1 do
      begin
        if (HotKeyControls[I1].HotKey = HotKeyControls[I2].HotKey) and not HotKeyControls[I1].HotKey.IsEmpty then
          Exit(True);
      end;
    end;
  end;

begin
  Result := not HasDuplicates;

  if Result then
    AErrorMsg := ''
  else
    AErrorMsg := 'Hotkey already in use!';
end;

end.

