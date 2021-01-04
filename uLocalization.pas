unit uLocalization;

interface

uses
  TntIniFiles, SysUtils;

type
  TLanguageCode = String[2];

  TLanguageInfo = record
    Code: TLanguageCode;
    Name, NativeName: WideString;
    FileName: String;
  end;

  TLanguagesArray = array of TLanguageInfo;

  ELocalizerException = class(Exception);

  TLocalizer = class
  private
    Ini: TTntMemIniFile;
    LangsDir: String;
  public
    constructor Create(ALangsDir: String);
    destructor Destroy; override;

    //procedure LoadByCode(ALang: TLanguageCode);
    procedure LoadFromFile(AFileName: String);
    function I18N(Str: String): WideString;
    procedure GetLanguages(var LangsArr: TLanguagesArray);
  end;

var
  Localizer: TLocalizer;

implementation

uses uUtils;

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

procedure TLocalizer.GetLanguages(var LangsArr: TLanguagesArray);
const
  IniSection = 'info';
var
  SearchRes: TSearchRec;
  LangsCount: integer;
  Idx: integer;
  Ini: TTntMemIniFile;
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
      Ini := TTntMemIniFile.Create(LangsDir + SearchRes.Name);
      try
        try
          Inc(Idx);

          LangsArr[Idx].Code := {Wide}LowerCase(Ini.ReadString(IniSection, 'LangCode', ''));
          LangsArr[Idx].Name := Ini.ReadString(IniSection, 'LangName', '');
          LangsArr[Idx].NativeName := Ini.ReadString(IniSection, 'LangNativeName', '');
          LangsArr[Idx].FileName := LangsDir + SearchRes.Name;
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
  Ini := TTntMemIniFile.Create(AFileName);
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
 