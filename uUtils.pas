unit uUtils;

interface

uses
  Windows;

// Retrieves the time (in ms) of the last input event (mouse moved or key pressed).
// Also works if current application window has no focus (hidden or minimized).
function LastInput: DWord;

// ToDo: add description
function FormatPath(Path: String; DateTime: TDateTime): String; overload;

// ToDo: add description
function FormatPath(Path: String): String; overload;

// ToDo: add description
function Int2Str(Val: Integer; LeadingZeros: Integer = 0): String;

// Decodes control characters (like \r, \n, \t and etc.) from given string.
function DecodeControlCharacters(Str: String): String;


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

function FormatPath(Path: String; DateTime: TDateTime): String;
const
  TmplVarsChar = '%';
var
  I: Integer;
  IsVariable: Boolean;
begin
  Result := '';

  IsVariable := False;
  for I := 1 to Length(Path) do
  begin
    if Path[I] = TmplVarsChar then
      IsVariable := True
    else
    begin
      if IsVariable then
      begin
        case Path[I] of
          'D': Result := Result + Int2Str(DayOf(DateTime), 2);
          'M': Result := Result + Int2Str(MonthOf(DateTime), 2);
          'Y': Result := Result + Int2Str(YearOf(DateTime), 4);
          'H': Result := Result + Int2Str(HourOf(DateTime), 2);
          'N': Result := Result + Int2Str(MinuteOf(DateTime), 2);
          'S': Result := Result + Int2Str(SecondOf(DateTime), 2);
        end;
        IsVariable := False;
      end
      else
      begin
        Result := Result + Path[I];
      end;
    end;
  end;
end;

function FormatPath(Path: String): String;
begin
  Result := FormatPath(Path, Now());
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

function DecodeControlCharacters(Str: String): String;
// https://programmersforum.ru/showthread.php?p=1813831#post1813831
// ToDo: So similar to FormatPath(). Need to reduce code duplication.
var
  I: Integer;
  IsSpecial: Boolean;
begin
  Result := '';

  IsSpecial := False;
  for I := 1 to Length(Str) do
  begin
    if IsSpecial then
    begin
      case Str[I] of
        'r': Result := Result + #13;
        'n': Result := Result + #10;
        't': Result := Result + #9;
        '\': Result := Result + '\';
        // ToDo: add more...
      end;
      IsSpecial := False;
    end
    else
    begin
      if Str[I] = '\' then
        IsSpecial := True
      else
        Result := Result + Str[I]; // ToDo: Looks not good in the loop
    end;
  end;
end;

end.
 