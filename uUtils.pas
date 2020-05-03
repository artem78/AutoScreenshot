unit uUtils;

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
function DecodeControlCharacters(const Str: String): String;


implementation

uses
  SysUtils, DateUtils;

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

function DecodeControlCharacters(const Str: String): String;
begin
  Result := StringReplace(Str,    '\r', #13, [rfReplaceAll]);
  Result := StringReplace(Result, '\n', #10, [rfReplaceAll]);
  Result := StringReplace(Result, '\t', #9,  [rfReplaceAll]);
  Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);
end;

end.
 