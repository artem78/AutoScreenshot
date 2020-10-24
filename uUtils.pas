unit uUtils;
{ Various general usefull functions }


interface

uses
  Windows;

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

// Same as IntToStr(), but adds leading zeros before number
function Int2Str(Val: Integer; LeadingZeros: Integer = 0): String;

// Decodes control characters (like \r, \n, \t and etc.) from given string.
function DecodeControlCharacters(const Str: WideString): WideString;

{ Returns string with program version. If ShortFormat is True prints release
  and build values only if they are not equal zero.

  Examples:
      GetProgramVersionStr(True);

      Version   | Result
      ----------------------
      2.1.0.0   | '2.1'
      1.0.0.0   | '1.0'
      0.0.0.0   | '0.0'
      3.0.1.0   | '3.0.1'
      5.6.7.8   | '5.6.7.8'
      0.0.0.9   | '0.0.0.9'
 }

function GetProgramVersionStr({HideRealeaseAndBuildIfZero} ShortFormat: Boolean = False): string;

// Returns build time from TimeDateStamp PE header
function GetLinkerTimeStamp: TDateTime{; overload};

{ Removes duplicated slashes from given path.
  Example:
      RemoveExtraPathDelimiters('c:\dir1\\dir2\\\file.txt');
      Result: c:\dir1\dir2\file.txt
}
function RemoveExtraPathDelimiters(const Path: String): String;

function JoinPath(const Base: String; const Path: String): String;

function GetLocalComputerName: string;

function GetCurrentUserName: string;

implementation

uses
  SysUtils, DateUtils, TntSysUtils;

function LastInput: DWord;
var
  LInput: TLastInputInfo;
begin
  LInput.cbSize := SizeOf(TLastInputInfo);
  GetLastInputInfo(LInput);
  Result := GetTickCount - LInput.dwTime;
end;

function FormatDateTime2(const Str: String; const DateTime: TDateTime): String;
const
  TmplVarsChar = '%';
begin
  Result := StringReplace(Str,    TmplVarsChar + 'D', Int2Str(DayOf(DateTime), 2),    [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'M', Int2Str(MonthOf(DateTime), 2),  [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'Y', Int2Str(YearOf(DateTime), 4),   [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'H', Int2Str(HourOf(DateTime), 2),   [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'N', Int2Str(MinuteOf(DateTime), 2), [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'S', Int2Str(SecondOf(DateTime), 2), [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'COMP', GetLocalComputerName, [rfReplaceAll]);
  Result := StringReplace(Result, TmplVarsChar + 'USER', GetCurrentUserName,   [rfReplaceAll]);
end;

function FormatDateTime2(const Str: String): String;
begin
  Result := FormatDateTime2(Str, Now());
end;

function Int2Str(Val: Integer; LeadingZeros: Integer): String;
var
  Tmp: String;
begin
  Result := '';

  Tmp := IntToStr(Abs(Val));

  if Val < 0 then
    Result := Result + '-';

  if (LeadingZeros > 0) and (LeadingZeros > Length(Tmp)) then
    Result := Result + StringOfChar('0', LeadingZeros - Length(Tmp));

  Result := Result + Tmp;
end;

function DecodeControlCharacters(const Str: WideString): WideString;
begin
  Result := Tnt_WideStringReplace(Str,    '\r', #13, [rfReplaceAll]);
  Result := Tnt_WideStringReplace(Result, '\n', #10, [rfReplaceAll]);
  Result := Tnt_WideStringReplace(Result, '\t', #9,  [rfReplaceAll]);
  Result := Tnt_WideStringReplace(Result, '\\', '\', [rfReplaceAll]);
end;

{function GetProgramVersionStr: string;
var
  Rec: LongRec;
begin
  Rec := LongRec(GetFileVersion(ParamStr(0)));
  Result := Format('%d.%d', [Rec.Hi, Rec.Lo])
end;}

function GetProgramVersionStr({HideRealeaseAndBuildIfZero} ShortFormat: Boolean): String;
var
  FileName: String;
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;

  Major, Minor, Release, Build: Cardinal;
begin
  // Note: Also we can get version string from FileVersion
  // or ProductVersion section

  FileName := ParamStr(0);
  Result := '';
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, PVerInfo) then
      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
        with PVerValue^ do
          {Result := Format('v%d.%d.%d build %d', [
            HiWord(dwFileVersionMS), //Major
            LoWord(dwFileVersionMS), //Minor
            HiWord(dwFileVersionLS), //Release
            LoWord(dwFileVersionLS)]); //Build}
        begin
          Major := HiWord(dwFileVersionMS);
          Minor := LoWord(dwFileVersionMS);
          Release := HiWord(dwFileVersionLS);
          Build := LoWord(dwFileVersionLS);

          if not {HideRealeaseAndBuildIfZero} ShortFormat then
            Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build])
          else
          begin
            // Writes minor and major parts in any case
            Result := Format('%d.%d', [Major, Minor]);

            // Writes realease and build only if they exist
            if (Release > 0) or (Build > 0) then
            begin
              Result := Result + '.' + IntToStr(Release);

              if Build > 0 then
                Result := Result + '.' + IntToStr(Build);
            end;
          end;
        end;
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;

{function LinkerTimeStamp(const FileName: string): TDateTime; overload;
var
  LI: TLoadedImage;
begin
  Win32Check(MapAndLoad(PChar(FileName), nil, @LI, False, True));
  Result := LI.FileHeader.FileHeader.TimeDateStamp / SecsPerDay + UnixDateDelta;
  UnMapAndLoad(@LI);
end;}

function GetLinkerTimeStamp: TDateTime{; overload};
begin
  Result := PImageNtHeaders(HInstance + Cardinal(PImageDosHeader(HInstance)^
        ._lfanew))^.FileHeader.TimeDateStamp / SecsPerDay + UnixDateDelta;
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

end.
