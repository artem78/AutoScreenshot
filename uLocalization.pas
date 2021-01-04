unit uLocalization;

interface

uses
  TntIniFiles;

type
  TLanguageCode = String[2];

  TLanguageInfo = record
    Code: TLanguageCode;
    Name, NativeName: WideString;
    FileName: String;
  end;

  TLanguagesArray = array of TLanguageInfo;

  TLocalizer = class
  private
    Lang: TLanguageCode;
    Ini: TTntMemIniFile;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetLang(ALang: TLanguageCode);
    function I18N(Str: String): WideString;
    procedure GetLanguages(var LangsArr: TLanguagesArray);
  end;

var
  Localizer: TLocalizer;

implementation

uses SysUtils, uUtils;

const
  LangSubDir = 'lang';

{ TLocalizer }

constructor TLocalizer.Create;
begin
  Lang := 'en';
  Ini := nil;
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
  LangDir: String;
  SearchRes: TSearchRec;
  LangsCount: integer;
  Idx: integer;
  Ini: TTntMemIniFile;
begin
  LangDir := ExtractFilePath(ParamStr(0)) + LangSubDir + PathDelim;

  // Get amount of available languages
  LangsCount := 0;
  if FindFirst(LangDir + '*.ini', faAnyFile, SearchRes) = 0 then
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
  if FindFirst(LangDir + '*.ini', faAnyFile, SearchRes) = 0 then
  begin
    repeat
      Ini := TTntMemIniFile.Create(LangDir + SearchRes.Name);
      try
        try
          Inc(Idx);

          LangsArr[Idx].Code := {Wide}LowerCase(Ini.ReadString(IniSection, 'LangCode', ''));
          LangsArr[Idx].Name := Ini.ReadString(IniSection, 'LangName', '');
          LangsArr[Idx].NativeName := Ini.ReadString(IniSection, 'LangNativeName', '');
          LangsArr[Idx].FileName := LangDir + SearchRes.Name;
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
  //Result := '[' + Lang + ']' + Str + '';
  Result := Ini.ReadString('translation', Str, {Str}'<unknown>');

  Result := DecodeControlCharacters(Result);
end;

procedure TLocalizer.SetLang(ALang: TLanguageCode);
var
  LangDir: String;
begin
  Lang := ALang;

  LangDir := ExtractFilePath(ParamStr(0)) + LangSubDir + PathDelim;

  // Load ini file with strings for selected language
  FreeAndNil(Ini);
  Ini := TTntMemIniFile.Create(LangDir + Lang + '.ini');
end;

initialization
begin
  Localizer := TLocalizer.Create;
end;

finalization
begin
  FreeAndNil(Localizer);
end;

end.
 