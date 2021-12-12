unit uCapturedAreaFrame;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls;

type
  TOnChangeCallbackProc = procedure;

  { TCapturedAreaFrame }

  TCapturedAreaFrame = class(TFrame)
    CapturedAreaGroupBox: TGroupBox;
    AllMonitorsRadioButton: TRadioButton;
    MonitorComboBox: TComboBox;
    SelectedMonitorRadioButton: TRadioButton;
    procedure AllMonitorsRadioButtonChange(Sender: TObject);
    procedure MonitorComboBoxChange(Sender: TObject);
    procedure SelectedMonitorRadioButtonChange(Sender: TObject);
  private
    FOnChangeCallback: TOnChangeCallbackProc;

    function GetMonitorId: Integer;
    procedure SetMonitorId(AMonitorId: Integer);

    procedure UpdateControlsVisibility;
    procedure DoOnChangeCallback;
  public
    procedure UpdateMonitorList; // ToDo: make private
    procedure FillMonitorList;   // ToDo: make private
    procedure Translate;


    property MonitorId: Integer read GetMonitorId write SetMonitorId;
    property OnChange: TOnChangeCallbackProc write FOnChangeCallback;
  end;

implementation

{$R *.lfm}

uses
  Windows, uLocalization;

const
  NoMonitorId = -1;
  MonitorWithCursor = -2;

{ TCapturedAreaFrame }

procedure TCapturedAreaFrame.SelectedMonitorRadioButtonChange(Sender: TObject);
begin
  UpdateControlsVisibility;
  DoOnChangeCallback;
end;

procedure TCapturedAreaFrame.AllMonitorsRadioButtonChange(Sender: TObject);
begin
  UpdateControlsVisibility;
  DoOnChangeCallback;
end;

procedure TCapturedAreaFrame.MonitorComboBoxChange(Sender: TObject);
begin
  DoOnChangeCallback;
end;

function TCapturedAreaFrame.GetMonitorId: Integer;
begin
  if AllMonitorsRadioButton.Checked or (MonitorComboBox.ItemIndex <= 0) then
    Result := NoMonitorId
  else if MonitorComboBox.ItemIndex = 0 then
    Result := MonitorWithCursor
  else
    Result := MonitorComboBox.ItemIndex - 1;
end;

procedure TCapturedAreaFrame.SetMonitorId(AMonitorId: Integer);
begin
  if AMonitorId = NoMonitorId then
  begin
    AllMonitorsRadioButton.Checked := True;
    //MonitorComboBox.Enabled := False;
  end
  else if AMonitorId = MonitorWithCursor then
  begin
    SelectedMonitorRadioButton.Checked := True;
    //MonitorComboBox.Enabled := True;
    MonitorComboBox.ItemIndex := 0;
  end
  else if (AMonitorId >= 0) and (AMonitorId < {Screen.MonitorCount} MonitorComboBox.Items.Count - 1) then
  begin
    SelectedMonitorRadioButton.Checked := True;
    //MonitorComboBox.Enabled := True;
    MonitorComboBox.ItemIndex := AMonitorId + 1;
  end
  else
    raise Exception.CreateFmt('Monitor id=%d not exists', [AMonitorId]);

  UpdateControlsVisibility;
end;

procedure TCapturedAreaFrame.UpdateControlsVisibility;
begin
  MonitorComboBox.Enabled := SelectedMonitorRadioButton.Checked;
end;

procedure TCapturedAreaFrame.DoOnChangeCallback;
begin
  if Assigned(FOnChangeCallback) then
    FOnChangeCallback();
end;

procedure TCapturedAreaFrame.UpdateMonitorList;
begin
  // Update array in Screen variable first
  Screen.UpdateMonitors;

  // Allow to choose monitor if more than one
  if Screen.MonitorCount >= 2 then
  begin // Multiple monitors
    MonitorComboBox.Enabled := True;
    SelectedMonitorRadioButton.Enabled := True;

    // Fix out of bounds
    if MonitorId >= Screen.MonitorCount then
      //MonitorId := Screen.MonitorCount - 1; // Last
      MonitorId := NoMonitorId;
  end
  else
  begin // Only one monitor available
    MonitorComboBox.Enabled := False;
    SelectedMonitorRadioButton.Enabled := False;
    MonitorId := NoMonitorId;
    //MonitorId := 0;
  end;

  // Fill monitors combobox
  FillMonitorList;

  UpdateControlsVisibility;
end;

procedure TCapturedAreaFrame.FillMonitorList;
var
  Idx, SelIdx: Integer;
  Str: WideString;
  IsLocalizationLoaded: Boolean;

begin
  IsLocalizationLoaded := True;
  try
    Localizer.I18N('test...')
  except
    IsLocalizationLoaded := False;
  end;

  if not IsLocalizationLoaded then
    Exit;


  // Fill combobox with monitor list
  with MonitorComboBox do
  begin
    //SelId := MonitorId;
    SelIdx := MonitorComboBox.ItemIndex;

    Items.Clear;
    {Items.Append(WideFormat(Localizer.I18N('AllMonitorsInfo'),
        [GetSystemMetrics(SM_CXVIRTUALSCREEN),
         GetSystemMetrics(SM_CYVIRTUALSCREEN)]
    ));}

    Items.Append(Localizer.I18N('MonitorWithCursor'));

    for Idx := 0 to Screen.MonitorCount - 1 do
    begin
      Str := WideFormat(Localizer.I18N('MonitorInfo'),
          [Screen.Monitors[Idx].MonitorNum + 1, // Start numeration from 1
           Screen.Monitors[Idx].Width,
           Screen.Monitors[Idx].Height]
      );
      // ToDo: Also may show screen model, diagonal size
      if Screen.Monitors[Idx].Primary then
        Str := Str + ' - ' + Localizer.I18N('Primary');

      Items.Append(Str);
    end;

    // Restore previous selected item after strings updated
    //MonitorId := SelId;
    MonitorComboBox.ItemIndex := SelIdx;
  end;
end;

procedure TCapturedAreaFrame.Translate;
begin
  CapturedAreaGroupBox.Caption := Localizer.I18N('CapturedArea');
  AllMonitorsRadioButton.Caption := WideFormat(Localizer.I18N('AllMonitorsInfo'),
        [GetSystemMetrics(SM_CXVIRTUALSCREEN),
         GetSystemMetrics(SM_CYVIRTUALSCREEN)]);
  SelectedMonitorRadioButton.Caption := Localizer.I18N('UsedMonitor') + ':';
end;

end.

