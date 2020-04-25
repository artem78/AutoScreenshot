unit uLocalization;

interface

uses
  IniFiles;

procedure I18NSetLang(ALang: String);
function I18N(Str: String): String;

implementation

uses SysUtils, uUtils;

var
  Lang: String;
  Ini: TIniFile;

const
  LangDir = 'lang/';

procedure I18NSetLang(ALang: String);
begin
  Lang := ALang;

  // Load ini file with strings for selected language
  FreeAndNil(Ini);
  Ini := TIniFile.Create(LangDir + Lang + '.ini');
end;

function I18N(Str: String): String;
begin
  //Result := '[' + Lang + ']' + Str + '';
  Result := ini.ReadString('translation', Str, Str);

  Result := DecodeControlCharacters(Result);
end;

initialization
begin
  // Sets default language
  Lang := 'en';
  Ini := nil;
end;

finalization
begin
  FreeAndNil(Ini);
end;

end.
 