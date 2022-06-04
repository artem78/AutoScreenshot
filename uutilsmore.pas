unit uUtilsMore;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils;

type

  { TProgramVersion }

  TProgramVersion = record
    Major, Minor, Revision, Build: Cardinal;

    class function Create(AMajor: Cardinal = 0; AMinor: Cardinal = 0;
             ARevision: Cardinal = 0; ABuild: Cardinal = 0): TProgramVersion; static; overload;
    class function Create(const AStr: String): TProgramVersion; static; overload;
    function ToString(ADropTrailingZeros: Boolean = False): String;
  end;

operator = (AVer1, AVer2: TProgramVersion): Boolean;
operator <> (AVer1, AVer2: TProgramVersion): Boolean;
operator > (AVer1, AVer2: TProgramVersion): Boolean;
operator < (AVer1, AVer2: TProgramVersion): Boolean;

type

  { THotKey }

  THotKey = record
    ShiftState: TShiftState;
    Key: Word;

    function ToString: String;
    procedure Parse(const AString: String);
  end;

implementation

uses
  RegExpr, StrUtils, Menus {for ShortCutToKey}, LCLProc;

type
  { TJoinInteger }

  { Thanks to Zvoni!
    https://forum.lazarus.freepascal.org/index.php/topic,59519.msg443738.html#msg443738 }
  TJoinInteger=Type helper(TStringHelper) for AnsiString
    class function Join(const Separator: string; const Values: array of Integer): string; overload; static;
  end;

operator = (AVer1, AVer2: TProgramVersion): Boolean;
begin
  Result := (AVer1.Major = AVer2.Major) and (AVer1.Minor = AVer2.Minor)
        and (AVer1.Revision = AVer2.Revision) and (AVer1.Build = AVer2.Build);
end;

operator <> (AVer1, AVer2: TProgramVersion): Boolean;
begin
  Result := not (AVer1 = AVer2);
end;

operator>(AVer1, AVer2: TProgramVersion): Boolean;
begin
  Result := False;

  if AVer1.Major > AVer2.Major then
    Result := True
  else if AVer1.Major = AVer2.Major then
  begin
    if AVer1.Minor > AVer2.Minor then
      Result := True
    else if AVer1.Minor = AVer2.Minor then
    begin
      if AVer1.Revision > AVer2.Revision then
        Result := True
      else if AVer1.Revision = AVer2.Revision then
      begin
        if AVer1.Build > AVer2.Build then
          Result := True;
      end;
    end;
  end;
end;

operator<(AVer1, AVer2: TProgramVersion): Boolean;
begin
  Result := (not (AVer1 > AVer2)) and (AVer1 <> AVer2);
end;

{ THotKey }

function THotKey.ToString: String;
begin
  Result := '';

  if ssAlt in ShiftState then
    Result := Result + 'Alt+';

  if ssShift in ShiftState then
    Result := Result + 'Shift+';

  if ssCtrl in ShiftState then
    Result := Result + 'Ctrl+';

  // Other possible values are not implemented yet

  //Result := Result + IntToStr(Key);
  Result := Result + KeyAndShiftStateToKeyString(Key, []);
end;

procedure THotKey.Parse(const AString: String);
var
  KeyNumPos: Integer;
  KeyName: String;
  ShortCut: TShortCut;
  UnusedShiftState: TShiftState;
begin
  ShiftState := [];

  if ContainsText(AString, 'alt') then
    Include(ShiftState, ssAlt);

  if ContainsText(AString, 'shift') then
    Include(ShiftState, ssShift);

  if ContainsText(AString, 'ctrl') then
    Include(ShiftState, ssCtrl);

  // Other possible values are not implemented yet

  KeyNumPos := RPos('+', AString) + 1;
  KeyName := Copy(AString, KeyNumPos, Length(AString) - KeyNumPos + 1);
  //Key := StrToInt(Copy(AString, KeyNumPos, Length(AString) - KeyNumPos + 1));
  ShortCut := TextToShortCut(KeyName);
  ShortCutToKey(ShortCut, Key, UnusedShiftState);
end;

{ TJoinInteger }

class function TJoinInteger.Join(const Separator: string;
  const Values: array of Integer): string;
Var
  SValues:Array Of String;
  i:SizeInt;
begin
  SetLength(SValues,System.Length(Values));
  For i:=Low(SValues) To High(SValues) Do SValues[i]:=Values[i].ToString;
  Result:=String.Join(Separator,SValues);
end;

{ TProgramVersion }

class function TProgramVersion.Create(AMajor: Cardinal; AMinor: Cardinal;
  ARevision: Cardinal; ABuild: Cardinal): TProgramVersion; static;
begin
  with Result do
  begin
    Major := AMajor;
    Minor := AMinor;
    Revision := ARevision;
    Build := ABuild;
  end;
end;

class function TProgramVersion.Create(const AStr: String): TProgramVersion;
var
  Re: TRegExpr;
begin
  // RegEx test: https://regex101.com/r/sEeva5/1

  Result := TProgramVersion.Create(0, 0, 0, 0);

  Re := TRegExpr.Create('^v?(\d+)(?:\.(\d+))?(?:\.(\d+))?(?:\.(\d+))?$');
  Re.ModifierI := True;
  if Re.Exec(AStr) then
  begin
    if Re.Match[1] <> '' then
      Result.Major := StrToInt(Re.Match[1]);

    if Re.Match[2] <> '' then
      Result.Minor := StrToInt(Re.Match[2]);

    if Re.Match[3] <> '' then
      Result.Revision := StrToInt(Re.Match[3]);

    if Re.Match[4] <> '' then
      Result.Build := StrToInt(Re.Match[4]);
  end;
  Re.Free;
end;

function TProgramVersion.ToString(ADropTrailingZeros: Boolean): String;
type
  TVersionNumbers = Array of {Cardinal} Integer;
var
  Numbers: TVersionNumbers;
  I: Integer;
begin
  Numbers := TVersionNumbers.Create(Major, Minor, Revision, Build);

  if ADropTrailingZeros then
  begin
    for I := High(Numbers) downto Low(Numbers) do
    begin
      if (Numbers[I] = 0) then
        Delete(Numbers, I, 1)
      else
        Break;
    end;
  end;

  Result := String.Join('.', Numbers);
end;

end.

