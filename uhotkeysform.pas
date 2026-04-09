unit uHotKeysForm;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

// ToDo: Too much code duplicates

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
    procedure AutoSizeComboBox;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property HotKey: THotKey read GetHotKey write SetHotKey;
  end;

  TSavingCallback = function(ASender: TObject; out AErrorMsg: string): Boolean of object;

  { THotKeysForm }

  THotKeysForm = class(TForm)
    ButtonPanel: TButtonPanel;
    Panel: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    Labels: array [0..2] of TLabel;
    HotKeyControls: array [0..2] of THotKeyControl;
    SavingCallback: TSavingCallback;

    procedure SetStartAutoCaptureKey(const AHotKey: THotKey);
    function GetStartAutoCaptureKey: THotKey;
    procedure SetStopAutoCaptureKey(const AHotKey: THotKey);
    function GetStopAutoCaptureKey: THotKey;
    procedure SetSingleCaptureKey(const AHotKey: THotKey);
    function GetSingleCaptureKey: THotKey;
    procedure SetStartAutoCaptureMarked(AVal: Boolean);
    procedure SetStopAutoCaptureMarked(AVal: Boolean);
    procedure SetSingleCaptureMarked(AVal: Boolean);

    procedure Translate;
  public
    constructor Create(TheOwner: TComponent; ASavingCallback: TSavingCallback = Nil){; override};

    property StartAutoCaptureKey: THotKey read GetStartAutoCaptureKey
                         write SetStartAutoCaptureKey;
    property StopAutoCaptureKey: THotKey read GetStopAutoCaptureKey
                         write SetStopAutoCaptureKey;
    property SingleCaptureKey: THotKey read GetSingleCaptureKey
                         write SetSingleCaptureKey;
    property StartAutoCaptureMarked: Boolean write SetStartAutoCaptureMarked;
    property StopAutoCaptureMarked: Boolean write SetStopAutoCaptureMarked;
    property SingleCaptureMarked: Boolean write SetSingleCaptureMarked;
  end;

var
  HotKeysForm: THotKeysForm;

implementation

uses
  LCLType, LCLProc, LCLIntf, Math, uLocalization;

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

  Idx := KeyComboBox.Items.IndexOfObject(TObject({Integer}PtrUInt(AHotKey.Key)));
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
    Result.Key := {Integer}PtrUInt(KeyComboBox.Items.Objects[SelIdx]);

  Result.ShiftState := [];

  if not Result.IsEmpty then
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
  KeyIdentifier, KeyName: String;
begin
  KeyComboBox.Clear;
  KeyComboBox.Items.BeginUpdate;

  KeyComboBox.Items.AddObject(Localizer.I18N('NoHotKey'), TObject(Integer(VK_UNKNOWN)));

  for Key := VK_BACK to VK_SCROLL do
  // VK_BROWSER_BACK to VK_OEM_CLEAR
  begin
    KeyIdentifier := KeyAndShiftStateToKeyString(Key, []);
    if KeyStringIsIrregular(KeyIdentifier) then
      Continue; // Skip unknown (unused) key codes

    KeyName := KeyIdentifier; // Default value
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
    end;

    // Try to find translation for key if possible
    KeyName := Localizer.I18N('Key' + KeyIdentifier, KeyName);

    {// just for test
    KeyComboBox.Items.AddObject(KeyName + ' | ' + KeyIdentifier + ' | #'
             + IntToStr(key), TObject({Integer}PtrUInt(Key)));}

    KeyComboBox.Items.AddObject(KeyName, TObject({Integer}PtrUInt(Key)));
  end;

  KeyComboBox.Items.EndUpdate;
end;

procedure THotKeyControl.AutoSizeComboBox;
const
  SPACING = {12} 20;
var
  I, TextMaxWidth, Metr: integer;
  Bmp: TBitmap;
begin
  TextMaxWidth := 0;
  Bmp := TBitmap.Create;
  try
    Bmp.Canvas.Font.Assign(KeyComboBox.Font);

    for I := 0 to KeyComboBox.Items.Count - 1 do
    begin
      TextMaxWidth := Max(TextMaxWidth, {KeyComboBox}Bmp.Canvas.font.GetTextWidth(KeyComboBox.Items[I]));
    end;

    Metr := GetSystemMetrics(SM_CXVSCROLL);
    KeyComboBox{.Width}.Constraints.MinWidth := TextMaxWidth + Metr + SPACING;
  finally
    Bmp.Free;
  end;
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
    Caption := Localizer.I18N('KeyAlt', 'Alt');
    //Name := '...';
  end;

  CtrlCheckBox := TCheckBox.Create(Self);
  with CtrlCheckBox do
  begin
    Parent := Self;
    //SetSubComponent(true);
    Caption := Localizer.I18N('KeyCtrl', 'Ctrl');
    //Name := '...';
  end;

  ShiftCheckBox := TCheckBox.Create(Self);
  with ShiftCheckBox do
  begin
    Parent := Self;
    //SetSubComponent(true);
    Caption := Localizer.I18N('KeyShift', 'Shift');
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
  end;
  FillKeyComboBox;
  AutoSizeComboBox;

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
  Translate;

  Texts[StartAutoCaptureIdx] := Localizer.I18N('StartAutoCapture');
  Texts[StopAutoCaptureIdx]  := Localizer.I18N('StopAutoCapture');
  Texts[SingleCaptureIdx]    := Localizer.I18N('SingleCapture');

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

procedure THotKeysForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  ErrorMsg: String;
begin
  CanClose := True;

  if ModalResult = mrOK then
  begin
    if Assigned(SavingCallback) and not SavingCallback(self, ErrorMsg) then
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

procedure THotKeysForm.SetStartAutoCaptureMarked(AVal: Boolean);
begin
  with Labels[StartAutoCaptureIdx] do
  begin
    if AVal then
    begin
      Font.Color := clRed;
      Font.Bold := True;
    end
    else
    begin
      Font.Color := clDefault;
      Font.Bold := False;
    end;
  end;
end;

procedure THotKeysForm.SetStopAutoCaptureMarked(AVal: Boolean);
begin
  with Labels[StopAutoCaptureIdx] do
  begin
    if AVal then
    begin
      Font.Color := clRed;
      Font.Bold := True;
    end
    else
    begin
      Font.Color := clDefault;
      Font.Bold := False;
    end;
  end;
end;

procedure THotKeysForm.SetSingleCaptureMarked(AVal: Boolean);
begin
  with Labels[SingleCaptureIdx] do
  begin
    if AVal then
    begin
      Font.Color := clRed;
      Font.Bold := True;
    end
    else
    begin
      Font.Color := clDefault;
      Font.Bold := False;
    end;
  end;
end;

procedure THotKeysForm.Translate;
begin
  Caption := Localizer.I18N('Hotkeys');
  ButtonPanel.OKButton.Caption := Localizer.I18N('Save');
  ButtonPanel.CancelButton.Caption := Localizer.I18N('Cancel');
end;

constructor THotKeysForm.Create(TheOwner: TComponent;
  ASavingCallback: TSavingCallback);
begin
  inherited Create(TheOwner);

  SavingCallback := ASavingCallback;
end;

end.

