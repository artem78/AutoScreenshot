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

uses LazLoggerBase, uUtils, ScreenGrabber, DateUtils,
  ///////////
  uAutoScreen, Forms
  ///////////
  ;

const
  RunInterval: Integer =
  {$IFOPT D+}
    5 * MSecsPerSec;
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
var
  MaxDateTime: TDateTime;
  ImgExts: TStringList;
  ImgFmt: TImageFormat;
  Dir: String;
begin
  // Set normal timer interval at first run
  if Timer.Interval <> RunInterval then
    Timer.Interval := RunInterval;

  MaxDateTime := Now - MaxAge;
  //DebugLn('MaxDateTime=', DateTimeToStr(MaxDateTime));

  ImgExts := TStringList.Create;
  try
    for ImgFmt in TImageFormat do
      ImgExts.Append(ImageFormatInfoArray[ImgFmt].Extension);
    //DebugLn('ImgExts=', ImgExts.CommaText);

    Dir := MainForm.OutputDirEdit.Directory {TODO: remake this!};
    Assert(not Dir.IsEmpty, 'Wrong path!');
    Assert(Dir <> '/', 'Wrong path!');

    DebugLn('Start clearing old screenshots until ', DateTimeToStr(MaxDateTime));
    DebugLnEnter;
    DeleteOldFiles(Dir, MaxDateTime, True, ImgExts.ToStringArray, True, @UpdateUI);
  finally
    ImgExts.Free;
    DebugLnExit;
    DebugLn('Old files cleaning finished');
  end;
end;

procedure TOldScreenshotCleaner.UpdateUI;
begin
  Application.ProcessMessages;
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
    Timer.Interval := 1 * MSecsPerSec; // For the first run only, will be increased later
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

