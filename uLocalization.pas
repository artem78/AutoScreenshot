unit uLocalization;

interface

uses
  TntIniFiles;

procedure I18NSetLang(ALang: String);
function I18N(Str: String): WideString;

implementation

uses SysUtils, uUtils;

var
  Lang: String;
  Ini: TTntMemIniFile;

const
  LangDir = 'lang/';

procedure I18NSetLang(ALang: String);
begin
  Lang := ALang;

  // Load ini file with strings for selected language
  FreeAndNil(Ini);
  Ini := TTntMemIniFile.Create(LangDir + Lang + '.ini');
end;

function I18N(Str: String): WideString;
begin
  //Result := '[' + Lang + ']' + Str + '';
  Result := ini.ReadString('translation', Str, {Str}'<unknown>');

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
 