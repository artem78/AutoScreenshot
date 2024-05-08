unit OldScreenshotCleaner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, SQLite3DS, SQLite3Conn, SQLDB, DB;

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

  { TFileJournal }

  TFileJournal = class
  private
    SQLite3Connection: TSQLite3Connection;
    Sqlite3Dataset: TSqlite3Dataset;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    DataSource: TDataSource;

    procedure CreateTables;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const AFileName: String);
    procedure Remove(AMaxDateTime: TDateTime);
    function GetFiles(AMaxDateTime: TDateTime): TStringList;
    function GetDirs(AMaxDateTime: TDateTime): TStringList;
  end;


  // Operator overloads

  operator explicit (const AInterval: TInterval): String;
  operator explicit (const AStr: String): TInterval;
  operator - (ADateTime: TDateTime; AInterval: TInterval): TDateTime;



implementation

uses LazLoggerBase, FileUtil, uUtils, DateUtils, StrUtils, SQLite3Dyn, ctypes,
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

procedure DirCallback(ACtx: psqlite3_context; AArgc: longint;
  AArgv: ppsqlite3_value); cdecl;
var
  Dir: String;
begin
  if (AArgc <> 1) or (sqlite3_value_type(AArgv[0]) <> SQLITE_TEXT) then
  begin
    sqlite3_result_null(ACtx);
    Exit;
  end;

  Dir := ExtractFilePath(sqlite3_value_text(AArgv[0]));

  sqlite3_result_text(ACtx, PAnsiChar(Dir), -1, sqlite3_destructor_type(SQLITE_TRANSIENT));
end;

{ TFileJournal }

procedure TFileJournal.CreateTables;
begin
  with SQLQuery do
  begin
    SQL.Clear;
    SQL.Add('CREATE TABLE IF NOT EXISTS `' + Sqlite3Dataset.TableName + '` (');
    SQL.Add('  `filename` TEXT,');
    SQL.Add('  `created` REAL');
    SQL.Add(');');
    ExecSQL;
    SQLTransaction.Commit;
    Close;
  end;
end;

constructor TFileJournal.Create;
var
  DBFileName: String;
  RC: cint;
begin
  if IsPortable then
    DBFileName := ProgramDirectory
  else
    DBFileName := GetAppConfigDir(False);
  DBFileName := ConcatPaths([DBFileName, 'journal.dat']);


  Sqlite3Dataset := TSqlite3Dataset.Create(Nil);
  with Sqlite3Dataset do
  begin
    FileName :=  DBFileName;
    TableName := 'files';
  end;

  SQLite3Connection := TSQLite3Connection.Create(Nil);
  with SQLite3Connection do
  begin
    DatabaseName := DBFileName;
    CharSet := 'UTF8';
  end;

  SQLTransaction := TSQLTransaction.Create(Nil);
  SQLTransaction.DataBase := SQLite3Connection;
  SQLite3Connection.Transaction := SQLTransaction;

  SQLQuery := TSQLQuery.Create(Nil);
  with SQLQuery do
  begin
    Database := SQLite3Connection;
    Transaction := SQLTransaction;
  end;

  DataSource := TDataSource.Create(Nil);
  DataSource.DataSet := Sqlite3Dataset;

  CreateTables;

  Sqlite3Dataset.Open;
  SQLite3Connection.Connected := True;


  //Register custom sqlite3 function
  RC := sqlite3_create_function(SQLite3Connection.Handle, PAnsiChar('DIR'), 1,
     SQLITE_UTF8 or SQLITE_DETERMINISTIC, Nil, @DirCallback, Nil, Nil);
  if RC <> SQLITE_OK then
    raise Exception.Create('Failed to create sqlite3 function');
end;

destructor TFileJournal.Destroy;
begin
  inherited Destroy;

  SQLite3Connection.Connected := False;
  Sqlite3Dataset.Close;

  DataSource.Free;
  SQLQuery.Free;
  SQLTransaction.Free;
  SQLite3Connection.Free;
  Sqlite3Dataset.Free;
end;

procedure TFileJournal.Add(const AFileName: String);
begin
  with SQLQuery do
  begin
    SQL.Clear;
    SQL.Add('INSERT INTO `' + Sqlite3Dataset.TableName + '` (`filename`, `created`) VALUES (:filename, :created);');
    ParamByName('filename').AsString := AFileName;
    ParamByName('created').{AsDateTime}AsFloat := Now;
    ExecSQL;
    SQLTransaction.Commit;
    Close;
  end;
end;

procedure TFileJournal.Remove(AMaxDateTime: TDateTime);
begin
  with SQLQuery do
  begin
    SQL.Clear;
    SQL.Add('DELETE FROM `' + Sqlite3Dataset.TableName + '` WHERE `created` < :created_before;');
    ParamByName('created_before').{AsDateTime}AsFloat := AMaxDateTime;
    ExecSQL;
    SQLTransaction.Commit;
    Close;
  end;
end;

function TFileJournal.GetFiles(AMaxDateTime: TDateTime): TStringList;
begin
  Result := TStringList.Create;

  with SQLQuery do
  begin
    SQL.Clear;
    SQL.Add('SELECT `filename`, `created` FROM `' + Sqlite3Dataset.TableName + '` WHERE `created` < :created_before;');
    ParamByName('created_before').{AsDateTime}AsFloat := {CreatedBefore} AMaxDateTime;
    Open;
    First;
    while not EOF do
    begin
      Result.Add(FieldByName('filename').AsString + #9 + FloatToStr(FieldByName('created').{AsDateTime}AsFloat));


     //// UpdateUI; // To prevent form freezes if too many files to delete

      Next;
    end;
    Close;
  end;
end;

function TFileJournal.GetDirs(AMaxDateTime: TDateTime): TStringList;
begin
  Result := TStringList.Create;

  with SQLQuery do
  begin
    SQL.Clear;
    SQL.Add('SELECT DISTINCT DIR(`filename`) AS `directory` FROM `' + Sqlite3Dataset.TableName + '` WHERE `created` < :created_before ORDER BY `directory` ASC;');
    ParamByName('created_before').{AsDateTime}AsFloat := {CreatedBefore} AMaxDateTime;
    Open;
    First;
    while not EOF do
    begin
      Result.Add(FieldByName('directory').AsString (*+ #9 + FloatToStr(FieldByName('created').{AsDateTime}AsFloat)*));
      //debugln('directory: ', FieldByName('directory').AsString);

     //// UpdateUI; // To prevent form freezes if too many files to delete

      Next;
    end;
    Close;
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
begin
  // Set normal timer interval at first run
  if Timer.Interval <> RunInterval then
    Timer.Interval := RunInterval;

    DeleteOldFiles;
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

  sl: TStringList;
  s, FileName, Created, Dir: String;
  Dirs: TStringList;
begin
  CreatedBefore := MaxDateTime;

  DebugLn('Start clearing old screenshots until %s (%s ago)',
         [DateTimeToStr(CreatedBefore), String(MaxAge)]);


  sl := MainForm.FileJournal.GetFiles(CreatedBefore);
  DebugLn('%d old screenshots found', [sl.Count]);
  for s in sl do
  begin
    FileName:=ExtractDelimited(1, s, [#9]);
    Created:=DateTimeToStr(StrToFloat(ExtractDelimited(2, s, [#9])));

    DebugLn('Try to delete "%s" created at %s ...',
            [FileName,
             created]);
{$IfDef SIMULATE_OLD_FILES_DELETION}
    DebugLn('[ Simulation! ]');
    Res := True;
{$Else}
    Res := DeleteFile(FileName);
{$EndIf}
    DebugLn(IfThen(Res, 'Ok', 'Failed!'));

    UpdateUI; // To prevent form freezes if too many files to delete
  end;

  Dirs := MainForm.FileJournal.GetDirs(CreatedBefore);
  for s in Dirs do
  begin
    Dir := s;
    //DebugLn('dir=', Dir);
    while not Dir.IsEmpty do
    begin
      DebugLn('dir=', Dir);
      if DirectoryExists(Dir) then
      begin
        if DirectoryIsEmpty(Dir) then
        begin
          DebugLn('Try to delete empty directory "%s" ...', [Dir]);
{$IfDef SIMULATE_OLD_FILES_DELETION}
          DebugLn('[ Simulation! ]');
          Res := True;
{$Else}
          Res := DeleteDirectory(Dir, False);
{$EndIf}
          DebugLn(IfThen(Res, 'Ok', 'Failed!'));
        end
        else
        begin
          DebugLn('Skip deletion of not empty directory "%s"', [Dir]);
          Break;
        end;
      end;

      Dir := ParentDirectory(Dir);

      UpdateUI; // To prevent form freezes if too many folders to delete
    end;
  end;
  Dirs.Free;

{$IfNDef SIMULATE_OLD_FILES_DELETION}
  MainForm.FileJournal.RemoveBefore(CreatedBefore);
{$EndIf}

  DebugLn('Old files cleaning finished');

  sl.free;
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

