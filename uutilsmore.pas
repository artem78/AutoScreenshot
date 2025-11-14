unit uUtilsMore;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, StdCtrls;

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
    function IsEmpty: boolean;
  end;

operator = (HotKey1, Hotkey2: THotKey): Boolean;
operator <> (HotKey1, Hotkey2: THotKey): Boolean;

type

  { TComboBoxHelper }

  TComboBoxHelper = class helper for TComboBox
    procedure AutoWidth;
  end;

  
function {CompareImages} ImagesEqual(const AImgFilename1, AImgFilename2: String;
              APercentThreshold: Integer = 100): Boolean;

implementation

uses
  RegExpr, StrUtils, Menus  {for ShortCutToKey}, LCLProc, LCLType, Graphics, LCLIntf, Math, BGRABitmap, BGRABitmapTypes;

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

operator=(HotKey1, Hotkey2: THotKey): Boolean;
begin
  Result := (HotKey1.ShiftState = Hotkey2.ShiftState)
       and (HotKey1.Key = Hotkey2.Key);
end;

operator<>(HotKey1, Hotkey2: THotKey): Boolean;
begin
  Result := not (HotKey1 = Hotkey2);
end;

{ TComboBoxHelper }

procedure TComboBoxHelper.AutoWidth;
const
  SPACING = {12} 20;
var
  I, TextMaxWidth, Metr: integer;
  Bmp: TBitmap;
begin
  TextMaxWidth := 0;
  Bmp := TBitmap.Create;
  try
    Bmp.Canvas.Font.Assign(Self.Font);

    for I := 0 to Self.Items.Count - 1 do
    begin
      TextMaxWidth := Max(TextMaxWidth, {Self}Bmp.Canvas.font.GetTextWidth(self.Items[I]));
    end;

    Metr := GetSystemMetrics(SM_CXVSCROLL);
    Self{.Width}.Constraints.MinWidth := TextMaxWidth + Metr + SPACING;
  finally
    Bmp.Free;
  end;
end;

function ImagesEqual(const AImgFilename1, AImgFilename2: String; APercentThreshold: Integer): Boolean;
var
  Bmp1, Bmp2: TBGRABitmap {= nil};
  p1, p2: PBGRAPixel;
  n: integer;
  EqualPixelCount: Integer = 0;
  R: Boolean;
begin
  if (not FileExists(AImgFilename1)) or (not FileExists(AImgFilename2)) then
    Exit(False);

  Bmp1 := TBGRABitmap.Create();
  Bmp2 := TBGRABitmap.Create();
  try
    try
      Bmp1.LoadFromFile(AImgFilename1);
      Bmp2.LoadFromFile(AImgFilename2);

      if (Bmp1.Width <> Bmp2.Width) or (Bmp1.Height <> Bmp2.Height) then
        Exit(False);

      p1 := bmp1.Data;
      p2 := bmp2.Data;
      for n := bmp1.NbPixels-1 downto 0 do
      begin
        //if p1 <> p2 then
        //if p1^ = p2^ then
        if (p1^.red = p2^.red) and (p1^.green = p2^.green) and (p1^.blue = p2^.blue) then
          Inc(EqualPixelCount);

        inc(p1);
        inc(p2);
      end;

      R := EqualPixelCount / bmp1.NbPixels >= APercentThreshold / 100;
      DebugLn('pixel matches=%d/%d (%.2f%%) Threshold=%d%% result=%s', [EqualPixelCount,
              bmp1.NbPixels, (EqualPixelCount / bmp1.NbPixels) * 100,
              APercentThreshold, BoolToStr(r, 'TRUE', 'FALSE')]);
      Exit(R);
    finally
      Bmp2.Free;
      Bmp1.Free;
    end;
  except
    Exit(False);
  end;
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

function THotKey.IsEmpty: boolean;
begin
  Result := Key = VK_UNKNOWN;
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

