unit uLocalization;

interface

uses
  TntIniFiles;

type
  TLocalizer = class
  private
    Lang: String;
    Ini: TTntMemIniFile;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetLang(ALang: String);
    function I18N(Str: String): WideString;
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

function TLocalizer.I18N(Str: String): WideString;
begin
  //Result := '[' + Lang + ']' + Str + '';
  Result := Ini.ReadString('translation', Str, {Str}'<unknown>');

  Result := DecodeControlCharacters(Result);
end;

procedure TLocalizer.SetLang(ALang: String);
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
 