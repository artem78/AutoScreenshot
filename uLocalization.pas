unit uLocalization;

{$MODE objfpc}

interface

uses
  IniFiles, SysUtils, Classes;

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

  { TLocalizer }

  TLocalizer = class
  private
    LangInfo: TLanguageInfo;
    Strings: TStringList;
    LangsDir: String;
    UseAltsForMissedStrings: Boolean; // Try or not read missed strings from default or alternative languages

    function GetLanguageInfo: TLanguageInfo;
    class function GetLanguageInfoFromIni(const AnIni: TMemIniFile): TLanguageInfo;
    procedure ClearLangInfoAndStrings;
  public
    constructor Create(ALangsDir: String; AnUseAltsForMissedStrings: Boolean = True);
    destructor Destroy; override;

    //procedure LoadByCode(ALang: TLanguageCode);
    procedure LoadFromFile(AFileName: String);
    function I18N(StrID: String; ADefaultVal: string = ''): WideString;
    function I18NFromGroup(const AGroupName: string; const StrID: String;
      const ADefaultVal: string = ''): WideString;
    procedure GetLanguages(var LangsArr: TLanguagesArray);

    property LanguageInfo: TLanguageInfo read GetLanguageInfo;
  end;

var
  Localizer: TLocalizer;

implementation

uses uUtils, FileUtil, LazFileUtils, {Classes,} StrUtils;

{ TLocalizer }

procedure TLocalizer.ClearLangInfoAndStrings;
begin
  with LangInfo do
  begin
    Code       := '';
    Name       := '';
    NativeName := '';
    FileName   := '';
    SetLength(AlternativeFor, 0);
    Author     := '';
  end;

  Strings.Clear;
end;

constructor TLocalizer.Create(ALangsDir: String; AnUseAltsForMissedStrings: Boolean);
begin
  Strings := TStringList.Create;
  Strings.NameValueSeparator := '=';
  LangInfo.AlternativeFor := nil;
  ClearLangInfoAndStrings;
  LangsDir := IncludeTrailingBackslash(ALangsDir);
  UseAltsForMissedStrings := AnUseAltsForMissedStrings;
end;

destructor TLocalizer.Destroy;
begin
  //FreeAndNil(LangInfo.AlternativeFor);
  LangInfo.AlternativeFor := nil;
  FreeAndNil(Strings);

  inherited;
end;

function TLocalizer.GetLanguageInfo: TLanguageInfo;
begin
  Result := LangInfo;
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

function TLocalizer.I18N(StrID: String; ADefaultVal: string): WideString;
begin
  if LangInfo.Code = '' then
    raise ELocalizerException.Create('No localization loaded');

  Result := Strings.Values[StrID];
  if Result = '' then
  begin
    if ADefaultVal <> '' then
      Result := ADefaultVal
    else
      Result := '<unknown>';
  end;

  Result := DecodeControlCharacters(Result);
end;

function TLocalizer.I18NFromGroup(const AGroupName: string; const StrID: String;
  const ADefaultVal: string): WideString;
begin
  Result := I18N(AGroupName + '___' + StrID, ADefaultVal);
end;

procedure TLocalizer.LoadFromFile(AFileName: String);
// ToDo: Optimization needed - reduce the number of file reading operations

const
  TRANSLATION_INI_SECTION = 'translation';

  procedure ReadAllTranslations(AIni: TMemIniFile; AStrs: TStrings);
  var
    Section, GroupName, StrID: string; //мржно shortstring
    StrVal:{string}{AnsiString}WideString;    //почему { $ H + } не включено?
    Sections, TmpSL: TStringList;
    I:integer;
  begin
    AStrs.Clear;

    Sections := TStringList.Create;
    TmpSL := TStringList.Create;
    try
      AIni.ReadSections(Sections);
      for Section in Sections do
      begin
        if Section = TRANSLATION_INI_SECTION then
          GroupName := ''
        //else if Section.StartsWith(TRANSLATION_INI_SECTION + '.') then // группа переводов
        else if StartsStr(TRANSLATION_INI_SECTION + '.', Section) then // группа переводов
          GroupName := ExtractWord(2, Section, ['.'])
        else
          Continue;

        TmpSL.Clear;

        AIni.ReadSectionValues(Section, TmpSL);
        for I := 0 to TmpSL.Count - 1 do
        begin
          StrID := TmpSL.Names[I];
          StrVal := TmpSL.Values[StrID];

          if {not GroupName.IsEmpty} GroupName <> '' then
            StrID := GroupName + '___' + StrID;

          AStrs.AddPair(StrID, StrVal);
          //writeln('sect=', Section, #9, 'group=', GroupName, #9, 'strid=', StrID, #9, 'strval="', StrVal,'"');
        end;
      end;
    finally
      TmpSL.Free;
      Sections.Free;
    end;

  end;

var
  Ini: TMemIniFile;
  TmpStr: TStringList;
  AllLangs: TLanguagesArray;
  AltLang: TLanguageCode;

  procedure CombineValues(L1: TStrings; const L2: TStrings);
  var
    Idx: integer;
  begin
    for Idx := 0 to L2.Count - 1 do
      L1.Values[L2.Names[Idx]] := L2.ValueFromIndex[Idx];
  end;
begin
  ClearLangInfoAndStrings;

  { Check if selected translation file exists }
  if not FileExists(AFileName) then
    raise ELocalizerException.CreateFmt('Can`t open localization file "%s"', [AFileName]);

  { Read language info }
  Ini := TMemIniFile.Create(AFileName);
  try
    LangInfo := GetLanguageInfoFromIni(Ini);
  finally
    FreeAndNil(Ini);
  end;

  if UseAltsForMissedStrings then
  begin
    { Read strings from default (English) translation }
    if not AnsiEndsStr('en.ini', AFileName) then // Skip for English
    begin
      Ini := TMemIniFile.Create(LangsDir + 'en.ini');
      try
        ReadAllTranslations(Ini, Strings);
      finally
        FreeAndNil(Ini);
      end;
    end;

    { Read and update strings from alternative of specified translation }
    GetLanguages(AllLangs);
    AltLang := GetAlternativeLanguage(AllLangs, LangInfo.Code);
    if AltLang <> '' then
    begin
      Ini := TMemIniFile.Create(LangsDir + AltLang + '.ini');
      TmpStr := TStringList.Create;
      try
        ReadAllTranslations(ini, TmpStr);
        CombineValues(Strings, TmpStr);
      finally
        FreeAndNil(TmpStr);
      end;
    end;
  end;

  { Read and update strings from specified translation }
  Ini := TMemIniFile.Create(AFileName);
  try
    // Combine translation strings with defaults
    TmpStr := TStringList.Create;
    try
      ReadAllTranslations(ini, TmpStr);
      CombineValues(Strings, TmpStr);
    finally
      FreeAndNil(TmpStr);
    end;
  finally
    FreeAndNil(Ini);
  end;
end;

var
  LangDir: String;

initialization
begin
  {$IfDef Windows}
  LangDir := AppendPathDelim(ConcatPaths([ProgramDirectory, 'lang']));
  {$EndIf}
  {$IfDef Linux}
  if IsPortable then
    LangDir := AppendPathDelim(ConcatPaths([ProgramDirectory, 'lang']))
  else
    LangDir := '/usr/share/autoscreenshot/lang/';
  {$EndIf}

  // ToDo: Use alternatives only in Release build
  Localizer := TLocalizer.Create(LangDir, True);
end;

finalization
begin
  FreeAndNil(Localizer);
end;

end.
 
