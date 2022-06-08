unit uUtils;

{$MODE Delphi}

{ Various general usefull functions }


interface

uses
  Windows, uLocalization;

// Retrieves the time (in ms) of the last input event (mouse moved or key pressed).
// Also works if current application window has no focus (hidden or minimized).
function LastInput: DWord;

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

{ Removes duplicated slashes from given path.
  Example:
      RemoveExtraPathDelimiters('c:\dir1\\dir2\\\file.txt');
      Result: c:\dir1\dir2\file.txt
}
function RemoveExtraPathDelimiters(const Path: String): String;

function JoinPath(const Base: String; const Path: String): String;

function GetLocalComputerName: string;

function GetCurrentUserName: string;

//function GetSystemLanguageID: Integer;

{ Returns two-letter language code from ISO 639 standard. }
function GetSystemLanguageCode: String{[2]};
function GetAlternativeLanguage(const ALangs: TLanguagesArray;
    ALangCode: TLanguageCode): TLanguageCode;

procedure AutoRun(const FileName: String; const AppTitle: String;
    Enabled: Boolean = True);
function CheckAutoRun(const AppTitle: String): Boolean;
procedure RunCmdInbackground(ACmd: String);

implementation

uses
  SysUtils, DateUtils, StrUtils, Registry, uLanguages, FileInfo, process;

type
  tagLASTINPUTINFO = record
    cbSize: UINT;
    dwTime: DWORD;
  end;
  LASTINPUTINFO = tagLASTINPUTINFO;
  TLastInputInfo = LASTINPUTINFO;

const
  RegistryAutorunKey = 'Software\Microsoft\Windows\CurrentVersion\Run';

function GetLastInputInfo(var plii: TLastInputInfo): BOOL;stdcall; external 'user32' name 'GetLastInputInfo';

function LastInput: DWord;
var
  LInput: TLastInputInfo;
begin
  LInput.cbSize := SizeOf(TLastInputInfo);
  GetLastInputInfo(LInput);
  Result := GetTickCount - LInput.dwTime;
end;

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

function RemoveExtraPathDelimiters(const Path: String): String;
var
  I: Integer;
  Ch: Char;
  IsPrevDelim: Boolean;
begin
  // Result := StringReplace(Path, PathDelim + PathDelim, PathDelim, [rfReplaceAll]);

  Result := '';
  IsPrevDelim := False;
  for I := 1 to Length(Path) do
  begin
     Ch := Path[I];
     if Ch = PathDelim then
     begin
       if not IsPrevDelim then
       begin
         Result := Result + Ch;
         IsPrevDelim := True;
       end;
     end
     else
     begin
       Result := Result + Ch;
       IsPrevDelim := False;
     end;
  end;
end;

function JoinPath(const Base: String; const Path: String): String;
begin
  Result := IncludeTrailingPathDelimiter(Base) + Path;
  Result := RemoveExtraPathDelimiters(Result);
end;

function GetLocalComputerName: string;
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

function GetCurrentUserName: string;
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
    Result := Buf
  else
    Result := '';
end;

{function GetSystemLanguageID: Integer;
begin
  Result := GetSystemDefaultLCID;
end;}

function GetSystemLanguageCode: String{[2]};
begin
  Result := Iso6391FromLcid(GetUserDefaultLCID);
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

procedure SetAutoRun(const FileName: String; const AppTitle: String);
const
  Args = '-autorun';
var
  Reg: TRegistry;
  Cmd: String;
begin
  Cmd := '"' + FileName + '" ' + Args;

  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(RegistryAutorunKey, True) then
      Reg.WriteString(AppTitle, Cmd);
  finally
    Reg.Free;
  end;
end;

procedure RemoveAutoRun(const AppTitle: String);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(RegistryAutorunKey, False) then
      Reg.DeleteValue(AppTitle);
  finally
    Reg.Free;
  end;
end;

procedure AutoRun(const FileName: String; const AppTitle: String;
    Enabled: Boolean = True);
begin
  if Enabled then
    SetAutoRun(FileName, AppTitle)
  else
    RemoveAutoRun(AppTitle);
end;

function CheckAutoRun(const AppTitle: String): Boolean;
var
  Reg: TRegistry;
begin
  Result := False;

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(RegistryAutorunKey) then
      Result := Reg.ReadString(AppTitle) <> '';
  finally
    Reg.Free;
  end;
end;

procedure RunCmdInbackground(ACmd: String);
var
  proc: TProcess;
begin
  proc := TProcess.Create(nil);
  try
    proc.CommandLine := ACmd;
    //proc.Options := proc.Options + ...;
    proc.Execute;
  finally
    proc.Free;
  end;
end;

end.
