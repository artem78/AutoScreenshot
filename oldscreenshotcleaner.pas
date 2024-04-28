unit OldScreenshotCleaner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls;

type
  TIntervalUnit = (iuHours, iuDays, iuWeeks, iuMonths);

  TInterval = record
    Val: Cardinal;
    Unit_: TIntervalUnit;
  end;

  TOldScreenshotCleanerChangeCallback = procedure of object;

  { TOldScreenshotCleaner }

  TOldScreenshotCleaner = class
  private
    FMaxAge: TInterval;
    FOnChangeCallback: TOldScreenshotCleanerChangeCallback;

    Timer: TTimer;

    procedure SetMaxAge(AMaxAge: TInterval);
    procedure SetActive(AActive: Boolean);
    function GetActive: Boolean;
    procedure DoOnTimer(ASender: TObject);
    procedure UpdateUI;
    function GetMaxDateTime: TDateTime;
    property MaxDateTime: TDateTime read GetMaxDateTime;
    procedure DeleteOldFiles;
  public
    constructor Create;
    destructor Destroy; override;

    property MaxAge: TInterval read FMaxAge write SetMaxAge;
    property Active: Boolean read GetActive write SetActive;
    property OnChangeCallback: TOldScreenshotCleanerChangeCallback
               read FOnChangeCallback write FOnChangeCallback;
    procedure Start;
    procedure Stop;
  end;

  operator explicit (const AInterval: TInterval): String;
  operator explicit (const AStr: String): TInterval;
  operator - (ADateTime: TDateTime; AInterval: TInterval): TDateTime;

implementation

uses LazLoggerBase, uUtils, ScreenGrabber, DateUtils, StrUtils,
  ///////////
  umainform, Forms
  ///////////
  ;

const
  RunInterval: Integer =
  {$IFOPT D+}
    5 * MSecsPerSec; // 5 seconds
  {$Else}
    30 * MSecsPerSec * SecsPerMin; // 30 minutes
  {$ENDIF}

operator explicit (const AInterval: TInterval): String;
var
  UnitShortName: Char;
begin
  case AInterval.Unit_ of
    iuHours:  UnitShortName := 'h';
    iuDays:   UnitShortName := 'd';
    iuWeeks:  UnitShortName := 'w';
    iuMonths: UnitShortName := 'm';
  end;

  Result := IntToStr(AInterval.Val) + UnitShortName;
end;

operator explicit (const AStr: String): TInterval;
var
  UnitShortName: Char;
begin
  if AStr.IsEmpty then
    raise Exception.Create('Empty string given');

  UnitShortName := AStr[Length(AStr)];
  case UnitShortName of
    'h': Result.Unit_ := iuHours;
    'd': Result.Unit_ := iuDays;
    'w': Result.Unit_ := iuWeeks;
    'm': Result.Unit_ := iuMonths;
    else raise Exception.CreateFmt('Unknown unit character ''%s''', [UnitShortName]);
  end;
  Result.Val := StrToInt(Copy(AStr, 1, Length(AStr) - 1));
end;

operator - (ADateTime: TDateTime; AInterval: TInterval): TDateTime;
begin
  case AInterval.Unit_ of
    iuHours:  Result := IncHour(ADateTime, -AInterval.Val);
    iuDays:   Result := IncDay(ADateTime, -AInterval.Val);
    iuWeeks:  Result := IncDay(ADateTime, -AInterval.Val * 7);
    iuMonths: Result := IncMonth(ADateTime, -AInterval.Val);
  end;
end;


{ TOldScreenshotCleaner }

procedure TOldScreenshotCleaner.SetMaxAge(AMaxAge: TInterval);
begin
  //if (FMaxAge.Unit_ = AMaxAge.Unit_) and (FMaxAge.Val = AMaxAge.Val) then
  //  Exit;

  FMaxAge := AMaxAge;

  if Assigned(FOnChangeCallback) then
    FOnChangeCallback;
end;

procedure TOldScreenshotCleaner.SetActive(AActive: Boolean);
begin
  //if FActive = AActive then
  //  Exit;

  if AActive then
    Start
  else
    Stop;

  if Assigned(FOnChangeCallback) then
    FOnChangeCallback;
end;

function TOldScreenshotCleaner.GetActive: Boolean;
begin
  Result := Timer.Enabled;
end;

procedure TOldScreenshotCleaner.DoOnTimer(ASender: TObject);
//var
//  MaxDateTime: TDateTime;
  //ImgExts: TStringList;
 // ImgFmt: TImageFormat;
//  Dir: String;
begin
  // Set normal timer interval at first run
  if Timer.Interval <> RunInterval then
    Timer.Interval := RunInterval;

  //MaxDateTime := Now - MaxAge;
  //DebugLn('MaxDateTime=', DateTimeToStr(MaxDateTime));

  //ImgExts := TStringList.Create;
  try
    //for ImgFmt in TImageFormat do
    //  ImgExts.Append(ImageFormatInfoArray[ImgFmt].Extension);
    ////DebugLn('ImgExts=', ImgExts.CommaText);

  //  Dir := MainForm.OutputDirEdit.Directory {TODO: remake this!};
  //  Assert(not Dir.IsEmpty, 'Wrong path!');
  //  Assert(Dir <> '/', 'Wrong path!');

    //DebugLn('Start clearing old screenshots until ', DateTimeToStr(MaxDateTime));
    DebugLnEnter;
    DeleteOldFiles{(Dir, MaxDateTime, True, ImgExts.ToStringArray, True, @UpdateUI)};
  finally
    //ImgExts.Free;
    DebugLnExit;
    DebugLn('Old files cleaning finished');
  end;
end;

procedure TOldScreenshotCleaner.UpdateUI;
begin
  Application.ProcessMessages;
end;

function TOldScreenshotCleaner.GetMaxDateTime: TDateTime;
begin
  Result := Now - MaxAge;
end;
  
procedure TOldScreenshotCleaner.DeleteOldFiles;
var
  Res: Boolean;
  CreatedBefore: TDateTime; // Needs for prevent other time in second call to MaxDateTime property
begin
  DebugLn('Start clearing old screenshots until %s (%s ago)',
         [DateTimeToStr(MaxDateTime), String(MaxAge)]);

  CreatedBefore := MaxDateTime;

  with MainForm.SQLQuery1 do
  begin
    SQL.Clear;
    SQL.Add('SELECT `filename`, `created` FROM `files` WHERE `created` < :created_before;');
    ParamByName('created_before').{AsDateTime}AsFloat := CreatedBefore;
    Open;
    First;
    while not EOF do
    begin
      DebugLn('Try to delete "%s" with date %s ...',
              [FieldByName('filename').AsString,
               DateTimeToStr(FieldByName('created').{AsDateTime}AsFloat, True)]);
{$IfDef SIMULATE_OLD_FILES_DELETION}
      DebugLn('[ Simulation! ]');
      Res := True;
{$Else}
      Res := DeleteFile(FullName);
{$EndIf}
      DebugLn(IfThen(Res, 'Ok', 'Failed!'));
      Next;
    end;
    Close;

{$IfNDef SIMULATE_OLD_FILES_DELETION}
    SQL.Clear;
    SQL.Add('DELETE FROM `files` WHERE `created` < :created_before;');
    ParamByName('created_before').{AsDateTime}AsFloat := CreatedBefore;
    ExecSQL;
    SQLTransaction1.Commit;
    Close;
{$EndIf}
  end;
end;

constructor TOldScreenshotCleaner.Create;
begin
  Timer := TTimer.Create(Nil);
  Timer.Enabled := False;
  Timer.OnTimer := @DoOnTimer;
end;

destructor TOldScreenshotCleaner.Destroy;
begin
  Stop;
  Timer.Free;

  inherited Destroy;
end;

procedure TOldScreenshotCleaner.Start;
begin
  if not Timer.Enabled then
  begin
    Timer.Interval := 1 * MSecsPerSec; // To start immediately, will be
                                       // increased later after first run
    Timer.Enabled := True;
    DebugLn('Old screenshot cleaner started');
  end;
end;

procedure TOldScreenshotCleaner.Stop;
begin
  if Timer.Enabled then
  begin
    Timer.Enabled := False;
    DebugLn('Old screenshot cleaner stopped');
  end;
end;

end.

