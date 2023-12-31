unit uUtils;

{$MODE objfpc}

{ Various general usefull functions }


interface

uses
  {$IfDef Windows}
  Windows,
  {$EndIf}
  uLocalization;


{ Formats given string Str. If DateTime not provided, use result of Now().

  Format characters list:
     %D    day (2 digits)
     %M    month (2 digits)
     %Y    year (4 digits)
     %H    hour (2 digits)
     %N    minute (2 digits)
     %S    second (2 digits)

  Usage example:
    Str := FormatDateTime2('%Y/%M/%D %H:%N:%S', EncodeDateTime(2020, 4, 19, 12, 34, 56, 789));

    Result string will be '2020/04/19 12:34:56' }

function FormatDateTime2(const Str: String; const DateTime: TDateTime): String; overload;
function FormatDateTime2(const Str: String): String; overload;

// Decodes control characters (like \r, \n, \t and etc.) from given string.
function DecodeControlCharacters(const Str: WideString): WideString;

//{ Returns string with program version. If ShortFormat is True prints release
//  and build values only if they are not equal zero.
//
//  Examples:
//      GetProgramVersionStr(True);
//
//      Version   | Result
//      ----------------------
//      2.1.0.0   | '2.1'
//      1.0.0.0   | '1.0'
//      0.0.0.0   | '0.0'
//      3.0.1.0   | '3.0.1'
//      5.6.7.8   | '5.6.7.8'
//      0.0.0.9   | '0.0.0.9'
// }
//
//function GetProgramVersionStr({HideRealeaseAndBuildIfZero} ShortFormat: Boolean = False): string;
function GetProgramVersionStr: string;

// Returns program build date and time
function GetBuildDateTime: TDateTime;

function GetLocalComputerName: string;

function GetCurrentUserName: string;

//function GetSystemLanguageID: Integer;

{ Returns two-letter language code from ISO 639 standard. }
function GetSystemLanguageCode: String{[2]};
function GetAlternativeLanguage(const ALangs: TLanguagesArray;
    ALangCode: TLanguageCode): TLanguageCode;

procedure AutoRun(const FileName: String; const AppTitle: String;
    Enabled: Boolean = True);
function CheckAutoRun(const FileName: String; const AppTitle: String): Boolean;
procedure RunCmdInbackground(ACmd: String);
function IsPortable: Boolean;
function GetUserPicturesDir: WideString;

type
  TDeleteOldFilesCallback = procedure of Object;

procedure DeleteOldFiles(const ADir: string; AMaxDateTime: TDateTime;
  AIncludeSubdirs: Boolean; const AAllowedExtensions: array of {String} AnsiString;
  ADeleteEmptyDirs: Boolean; ACallback: TDeleteOldFilesCallback = Nil);

implementation

uses
  {$IfDef Windows}
  WinDirs {???}, Registry,
  {$EndIf}
  {$IfDef Linux}
  Unix, LazUTF8, LazFileUtils,
  {$EndIf}
  SysUtils, Classes, DateUtils, StrUtils, uLanguages, Forms {???}, FileInfo, process,
  FileUtil, LazLogger;

{$IfDef Windows}
const
  RegistryAutorunKey = 'Software\Microsoft\Windows\CurrentVersion\Run';
{$EndIf}

function FormatDateTime2(const Str: String; const DateTime: TDateTime): String;
  function DecToNum(N: Longint; Len: byte): string;
  begin
    Result := Dec2Numb(N, Len, 10);
  end;

const
  TmplVarsChar = '%';
begin
  Result := StringReplace(Str,    TmplVarsChar + 'D', DecToNum(DayOf(DateTime), 2),    [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'M', DecToNum(MonthOf(DateTime), 2),  [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'Y', DecToNum(YearOf(DateTime), 4),   [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'H', DecToNum(HourOf(DateTime), 2),   [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'N', DecToNum(MinuteOf(DateTime), 2), [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'S', DecToNum(SecondOf(DateTime), 2), [rfReplaceAll]);
end;

function FormatDateTime2(const Str: String): String;
begin
  Result := FormatDateTime2(Str, Now());
end;

function DecodeControlCharacters(const Str: WideString): WideString;
begin
  Result := StringReplace(Str,    '\r', #13, [rfReplaceAll]);
  Result := StringReplace(Result, '\n', #10, [rfReplaceAll]);
  Result := StringReplace(Result, '\t', #9,  [rfReplaceAll]);
  Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);
end;

//function GetProgramVersionStr({HideRealeaseAndBuildIfZero} ShortFormat: Boolean): String;
//var
//  FileName: String;
//  VerInfoSize: Cardinal;
//  VerValueSize: Cardinal;
//  Dummy: Cardinal;
//  PVerInfo: Pointer;
//  PVerValue: PVSFixedFileInfo;
//
//  Major, Minor, Release, Build: Cardinal;
//begin
//  // Note: Also we can get version string from FileVersion
//  // or ProductVersion section
//
//  FileName := ParamStr(0);
//  Result := '';
//  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
//  GetMem(PVerInfo, VerInfoSize);
//  try
//    if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, PVerInfo) then
//      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
//        with PVerValue^ do
//          {Result := Format('v%d.%d.%d build %d', [
//            HiWord(dwFileVersionMS), //Major
//            LoWord(dwFileVersionMS), //Minor
//            HiWord(dwFileVersionLS), //Release
//            LoWord(dwFileVersionLS)]); //Build}
//        begin
//          Major := HiWord(dwFileVersionMS);
//          Minor := LoWord(dwFileVersionMS);
//          Release := HiWord(dwFileVersionLS);
//          Build := LoWord(dwFileVersionLS);
//
//          if not {HideRealeaseAndBuildIfZero} ShortFormat then
//            Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build])
//          else
//          begin
//            // Writes minor and major parts in any case
//            Result := Format('%d.%d', [Major, Minor]);
//
//            // Writes realease and build only if they exist
//            if (Release > 0) or (Build > 0) then
//            begin
//              Result := Result + '.' + IntToStr(Release);
//
//              if Build > 0 then
//                Result := Result + '.' + IntToStr(Build);
//            end;
//          end;
//        end;
//  finally
//    FreeMem(PVerInfo, VerInfoSize);
//  end;
//end;

function GetProgramVersionStr: String;
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo := TFileVersionInfo.Create(Nil);
  try
    FileVerInfo.ReadFileInfo;
    Result := FileVerInfo.VersionStrings.Values[{'FileVersion'} 'ProductVersion'];
  finally
    FileVerInfo.Free;
  end;
end;

function GetBuildDateTime: TDateTime;
begin
  Result := EncodeDateTime({$I %dateYear%}, {$I %dateMonth%}, {$I %dateDay%},
            {$I %timeHour%}, {$I %timeMinute%}, {$I %timeSecond%}, 0);
end;

function GetLocalComputerName: string;
{$IfDef Windows}
var
  Size: dword;
  Buf: array [0..MAX_COMPUTERNAME_LENGTH + 1] of char;
  Res: Boolean;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  Res := GetComputerName(@Buf, Size);
  if Res and (Size > 0) then
    Result := Buf
  else
    Result := '';
end;
{$EndIf}
{$IfDef Linux}
begin
  //Result := GetEnvironmentVariable('COMPUTERNAME');
  Result := GetHostName;
end;
{$EndIf}

function GetCurrentUserName: string;
{$IfDef Windows}
const
  UNLEN = 256; // Not defined in windows.pas
var
  Size: dword;
  Buf: array [0..UNLEN + 1] of char;
  Res: Boolean;
begin
  Size := UNLEN + 1;
  Res := GetUserName(@Buf, Size);
  if Res and (Size > 0) then
    //Result := Buf
    SetString(Result, {Buf} PChar(@Buf[0]), {Length(Buf)} Size - 1)
  else
    Result := '';
end;
{$EndIf}
{$IfDef Linux}
const
  BufSize = 256;
var
  S: TProcess;
  Count, I: Integer;
  Buffer: array[1..BufSize] of {byte} char;
  SL: TStringList;
begin
  //Result := GetEnvironmentVariable({'USERNAME'} {'USER'} 'LOGNAME');
  S:=TProcess.Create(Nil);
  S.Commandline:='whoami';
  S.Options:=[poUsePipes,poNoConsole];
  S.execute;
  {Repeat
    Count:=s.output.read(Buffer,BufSize);
    // reverse print for fun.
    For I:=1 to count do
      Result:=Result + Buffer[i];
  until Count=0;}
  sl:=TStringList.Create;
  sl.LoadFromStream(s.Output);
  Result:=sl[0];
  sl.Free;

  s.Free;
end;
{$EndIf}

{function GetSystemLanguageID: Integer;
begin
  Result := GetSystemDefaultLCID;
end;}

function GetSystemLanguageCode: String{[2]};
begin
  {$IfDef Windows}
  Result := Iso6391FromLcid(GetUserDefaultLCID);
  {$EndIf}
  {$IfDef Linux}
  Result := GetEnvironmentVariable('LANGUAGE');
  {$EndIf}
end;

function GetAlternativeLanguage(const ALangs: TLanguagesArray;
    ALangCode: TLanguageCode): TLanguageCode;
var
  LangIdx, AltIdx: Integer;
begin
  for LangIdx := 0 to Length(ALangs) - 1 do
  begin
    for AltIdx := 0 to Length(ALangs[LangIdx].AlternativeFor) - 1 do
    begin
      if ALangs[LangIdx].AlternativeFor[AltIdx] = ALangCode then
      begin
        Result := ALangs[LangIdx].Code;
        Exit;
      end;
    end;
  end;

  Result := ''; // Not found
end;

{$IfDef Linux}
function GetAutostartFileName(const AFileName: String): String;
begin
  Result := ConcatPaths([GetEnvironmentVariableUTF8('HOME'), '.config',
            'autostart', DelSpace(ExtractFileNameOnly(AFileName)) + '.desktop']);
end;
{$EndIf}

procedure SetAutoRun(const FileName: String; const AppTitle: String);
const
  Args = '-autorun';
var
  {$IfDef Windows}
  Reg: TRegistry;
  {$EndIf}
  {$IfDef Linux}
  AutostartFileContent: TStringList;
  {$EndIf}
  Cmd, AutostartFile: String;
begin
  Cmd := '"' + FileName + '" ' + Args;

  {$IfDef Windows}
  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(RegistryAutorunKey, True) then
      Reg.WriteString(AppTitle, Cmd);
  finally
    Reg.Free;
  end;
  {$EndIf}

  {$IfDef Linux}
  AutostartFile := GetAutostartFileName(FileName);
  ForceDirectories(ExtractFileDir(AutostartFile));
  AutostartFileContent := TStringList.Create;
  try
    with AutostartFileContent do
    begin
      Add('[Desktop Entry]');
      Add('Type=Application');
      Add('Exec=' + Cmd);
      Add('Hidden=false');
      Add('Name=' + AppTitle);
      SaveToFile(AutostartFile);
    end;
  finally
    AutostartFileContent.Free;
  end;
  {$EndIf}
end;

procedure RemoveAutoRun(const FileName: String; const AppTitle: String);
{$IfDef Windows}
var
  Reg: TRegistry;
{$EndIf}
begin
  {$IfDef Windows}
  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(RegistryAutorunKey, False) then
      Reg.DeleteValue(AppTitle);
  finally
    Reg.Free;
  end;
  {$EndIf}

  {$IfDef Linux}
  DeleteFile(GetAutostartFileName(FileName));
  {$EndIf}
end;

procedure AutoRun(const FileName: String; const AppTitle: String;
    Enabled: Boolean = True);
begin
  if Enabled then
    SetAutoRun(FileName, AppTitle)
  else
    RemoveAutoRun(FileName, AppTitle);
end;

function CheckAutoRun(const FileName: String; const AppTitle: String): Boolean;
{$IfDef Windows}
var
  Reg: TRegistry;
{$EndIf}
begin
  {$IfDef Windows}
  Result := False;
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(RegistryAutorunKey) then
      Result := Reg.ReadString(AppTitle) <> '';
  finally
    Reg.Free;
  end;
  {$EndIf}

  {$IfDef Linux}
  Result := FileExists(GetAutostartFileName(FileName));
  {$EndIf}
end;

procedure RunCmdInbackground(ACmd: String);
var
  proc: TProcess;
begin
  proc := TProcess.Create(nil);
  try
    {$IfDef Windows}
    proc.Executable := 'cmd.exe';
    proc.Parameters.Add('/c');
    proc.Parameters.Add(ACmd);
    {$EndIf}
    {$IfDef Linux}
    proc.Executable := FindDefaultExecutablePath('bash');
    proc.Parameters.Add('-c');
    proc.Parameters.Add(ACmd);
    {$EndIf}
    proc.Options := proc.Options + [poNoConsole];
    proc.Execute;
  finally
    proc.Free;
  end;
end;

function IsPortable: Boolean;
{$IfDef Windows}
var
  UninstallerFileName: String;
{$EndIf}
begin
  {$IfDef Windows}
  UninstallerFileName := ExtractFilePath(Application.ExeName) + 'unins000.exe';
  Result := not FileExists(UninstallerFileName);
  {$EndIf}
  {$IfDef Linux}
  Result := not Application.ExeName.StartsWith('/usr/bin/');
  {$EndIf}
end;

function GetUserPicturesDir: WideString;
{$IfDef Linux}
var
  CmdRes: AnsiString;
{$EndIf}
begin
  Result := '';

  {$IfDef Windows}
  //Result := GetUserDir + 'Pictures';
  Result := GetWindowsSpecialDirUnicode(CSIDL_MYPICTURES);
  {$EndIf}
  {$IfDef Linux}
  try
    if RunCommand('xdg-user-dir PICTURES', CmdRes) then
      Result := Trim(CmdRes);
  except
  end;

  if (Result = '') or (not DirectoryExists(Result)) then
    // As fallback - not 100% guarantee, but most likely
    Result := ConcatPaths([GetEnvironmentVariable('HOME'), 'Pictures']);
  {$EndIf}

  Result := IncludeTrailingPathDelimiter(Result);
end;

{$IfDef Windows}
function FileCreatedTime(const ASearchRec: TSearchRec): TDateTime;
// Source: https://www.cyberforum.ru/post10364301.html+}
var
  t1:TFILETIME;
  t2:TSYSTEMTIME;
begin
  FileTimeToLocalFileTime(ASearchRec.FindData.ftCreationTime,t1);
  FileTimeToSystemTime(t1,t2);
  Result := SystemTimeToDateTime(t2);
end;
{$EndIf}

function FileModifiedTime(const ASearchRec: TSearchRec): TDateTime; inline;
begin
  Result := ASearchRec.TimeStamp;
end;

procedure DeleteOldFiles(const ADir: string; AMaxDateTime: TDateTime;
  AIncludeSubdirs: Boolean; const AAllowedExtensions: array of {String} AnsiString;
  ADeleteEmptyDirs: Boolean; ACallback: TDeleteOldFilesCallback);

  function IsEmptyDirectory(ADir: String): Boolean;
  var
    SearchRecResult: TSearchRec;
  begin
    Result := True;

    if FindFirst(ConcatPaths([ADir, '*']), faAnyFile, SearchRecResult) = 0 then
    begin
      repeat
        if (SearchRecResult.Name = '.') or (SearchRecResult.Name = '..') then
          Continue;

        Result := False;
        Break;
      until FindNext(SearchRecResult) <> 0;
      FindClose(SearchRecResult);
    end;
  end;
var
  SearchRec: TSearchRec;
  ExtList: TStringList;
  Ext: String;
  FullName: String;
  Res: Boolean;
  FileTime: TDateTime;
begin
  ExtList := TStringList.Create;
  try
    ExtList.CaseSensitive := False;
    ExtList.Duplicates := dupIgnore;
    ExtList.Sorted := True;

    for Ext in AAllowedExtensions do
      ExtList.Add(Ext);

    {DebugLn}DebugLnEnter('Prepare for clean "%s" directory from old files', [ADir]);
    //DebugLn('Allowed extensions: ', ExtList.CommaText);

    if FindFirst(ConcatPaths([ADir, '*']), faAnyFile, SearchRec) = 0 then
    begin
      try
        repeat
          if Assigned(ACallback) then
            ACallback;

          FullName := ConcatPaths([ADir, SearchRec.Name]);

          if (SearchRec.Attr and faDirectory) = faDirectory then
          begin
            if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
              Continue;

            if AIncludeSubdirs then
              DeleteOldFiles(ConcatPaths([ADir, SearchRec.Name]), AMaxDateTime,
                             AIncludeSubdirs, AAllowedExtensions, ADeleteEmptyDirs);

            Continue;
          end;

          if ExtList.Count > 0 then
          begin
            Ext := ExtractFileExt(SearchRec.Name);
            if (Length(Ext) > 0) and (Ext[1] = '.') then
              Delete(Ext, 1, 1); // Delete dot before extension
            if ExtList.IndexOf(Ext) = -1 then
            begin
              DebugLn('Skip "%s" with excluded extension', [FullName]);
              Continue;
            end;
          end;

          {$IfDef Windows}
          FileTime := FileCreatedTime(SearchRec);
          {$EndIf}
          {$IfDef Linux}
          // On Linux getting creation time is not always possible and not easy,
          // therefore modification time used instead.
          FileTime := FileModifiedTime(SearchRec);
          {$EndIf}

          if FileTime >= AMaxDateTime then
          begin
            DebugLn('Skip file "%s" with date %s', [FullName, DateTimeToStr(FileTime)]);
            Continue;
          end;

          DebugLn('Try to delete "%s" with date %s ...', [FullName, DateTimeToStr(FileTime)]);
{$IfDef SIMULATE_OLD_FILES_DELETION}
          DebugLn('[ Simulation! ]');
          Res := True;
{$Else}
          Res := DeleteFile(FullName);
{$EndIf}
          DebugLn(IfThen(Res, 'Ok', 'Failed!'));
        until FindNext(SearchRec) <> 0;
      finally
        FindClose(SearchRec);
      end;
    end;

    if ADeleteEmptyDirs and IsEmptyDirectory(ADir) then
    begin
      DebugLn('Try to delete empty directory "%s" ...', [ADir]);
{$IfDef SIMULATE_OLD_FILES_DELETION}
        DebugLn('[ Simulation! ]');
        Res := True;
{$Else}
        Res := DeleteDirectory(ADir, False);
{$EndIf}
      DebugLn(IfThen(Res, 'Ok', 'Failed!'));
    end;

  finally
    ExtList.Free;
  end;

  DebugLn('Old files cleaning in directory "%s" finished',  [ADir]);
  DebugLnExit;
end;

end.
