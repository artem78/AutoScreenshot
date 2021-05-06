unit uLocalization;

{$MODE Delphi}

interface

uses
  {TntIniFiles} IniFiles, SysUtils;

type
  TLanguageCode = String[2];

  TLanguageInfo = record
    Code: TLanguageCode;
    Name, NativeName: WideString;
    FileName: String;
    AlternativeFor: array of TLanguageCode;
    Author: WideString;
  end;

  TLanguagesArray = array of TLanguageInfo;

  ELocalizerException = class(Exception);

  TLocalizer = class
  private
    Ini: {TTntMemIniFile} TMemIniFile;
    LangsDir: String;

    function GetLanguageInfo: TLanguageInfo;
    class function GetLanguageInfoFromIni(const AnIni: TMemIniFile): TLanguageInfo;
  public
    constructor Create(ALangsDir: String);
    destructor Destroy; override;

    //procedure LoadByCode(ALang: TLanguageCode);
    procedure LoadFromFile(AFileName: String);
    function I18N(Str: String): WideString;
    procedure GetLanguages(var LangsArr: TLanguagesArray);

    property LanguageInfo: TLanguageInfo read GetLanguageInfo;
  end;

var
  Localizer: TLocalizer;

implementation

uses uUtils, Classes;

{ TLocalizer }

constructor TLocalizer.Create(ALangsDir: String);
begin
  Ini := nil;
  LangsDir := IncludeTrailingBackslash(ALangsDir);
end;

destructor TLocalizer.Destroy;
begin
  FreeAndNil(Ini);

  inherited;
end;

function TLocalizer.GetLanguageInfo: TLanguageInfo;
begin
  Result := TLocalizer.GetLanguageInfoFromIni(Ini);
end;

class function TLocalizer.GetLanguageInfoFromIni(
  const AnIni: TMemIniFile): TLanguageInfo;
const
  IniSection = 'info';
var
  AlternativeForStr: TStringList;
  I: integer;
begin
  Result.Code := {Wide}LowerCase(AnIni.ReadString(IniSection, 'LangCode', ''));
  Result.Name := AnIni.ReadString(IniSection, 'LangName', '');
  Result.NativeName := AnIni.ReadString(IniSection, 'LangNativeName', '');
  Result.FileName := AnIni.FileName;

  AlternativeForStr := TStringList.Create;
  AlternativeForStr.CommaText := AnIni.ReadString(IniSection, 'AlternativeFor', '');
  SetLength(Result.AlternativeFor, AlternativeForStr.Count);
  for I := 0 to AlternativeForStr.Count - 1 do
    Result.AlternativeFor[I] := AlternativeForStr.Strings[I];
  AlternativeForStr.Free;

  Result.Author := AnIni.ReadString(IniSection, 'Author', '');
end;

procedure TLocalizer.GetLanguages(var LangsArr: TLanguagesArray);
var
  SearchRes: TSearchRec;
  LangsCount: integer;
  Idx: integer;
  Ini: TMemIniFile;
begin
  // Get amount of available languages
  LangsCount := 0;
  if FindFirst(LangsDir + '*.ini', faAnyFile, SearchRes) = 0 then
  begin
    repeat
      Inc(LangsCount);
    until FindNext(SearchRes) <> 0;

    FindClose(SearchRes);
  end;

  // Allocate memory for array of languages
  SetLength(LangsArr, LangsCount);

  // Write language info in array
  Idx := -1;
  if FindFirst(LangsDir + '*.ini', faAnyFile, SearchRes) = 0 then
  begin
    repeat
      Ini := TMemIniFile.Create(LangsDir + SearchRes.Name);
      try
        try
          Inc(Idx);

          LangsArr[Idx] := TLocalizer.GetLanguageInfoFromIni(Ini);
        finally
          //Ini.Free;
          FreeAndNil(Ini);
        end;
      except
      end;
    until FindNext(SearchRes) <> 0;

    FindClose(SearchRes);
  end;
end;

function TLocalizer.I18N(Str: String): WideString;
begin
  if not Assigned(Ini) then
    raise ELocalizerException.Create('No localization loaded');

  //Result := '[' + Lang + ']' + Str + '';
  Result := Ini.ReadString('translation', Str, {Str}'<unknown>');

  Result := DecodeControlCharacters(Result);
end;

procedure TLocalizer.LoadFromFile(AFileName: String);
var
  FileName: String;
begin
  FreeAndNil(Ini);

  // Load ini file with strings for selected language
  if not FileExists(AFileName) then
    raise ELocalizerException.CreateFmt('Can`t open localization file "%s"', [FileName]);
  Ini := TMemIniFile.Create(AFileName);
end;

initialization
begin
  Localizer := TLocalizer.Create(ExtractFilePath(ParamStr(0)) + 'lang' + PathDelim);
end;

finalization
begin
  FreeAndNil(Localizer);
end;

end.
 
