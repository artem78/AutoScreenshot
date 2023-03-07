unit uLanguages;

{$MODE objfpc}

// Wikipedia: https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes

interface

type
  TIso6391Code = String[2]; // ISO 639-1 Code
  TIso6393Code = String[3]; // ISO 639-3 Code
  TLcid = Cardinal; // Windows Language Code Identifier

function Iso6391FromLcid(ALcid: TLcid): TIso6391Code;
function Iso6393FromLcid(ALcid: TLcid): TIso6393Code;

implementation

uses SysUtils;

type
  TLangCodesRecord = record
    Iso6391Code: TIso6391Code;
    Iso6393Code: TIso6393Code;
    Lcid: TLcid;
  end;

{$I languagecodes.inc}
  

function FindByLcid(ALcid: TLcid): TLangCodesRecord;
var
  LangInfo: TLangCodesRecord;
begin
  for LangInfo in CodesArr do
  begin
    if LangInfo.Lcid = ALcid then
      Exit(LangInfo);
  end;

  raise Exception.Create('LCID not found');
end;

function Iso6391FromLcid(ALcid: TLcid): TIso6391Code;
begin
  Result := '';
  try
    Result := FindByLcid(ALcid).Iso6391Code;
  except
  end;
end;

function Iso6393FromLcid(ALcid: TLcid): TIso6393Code;
begin
  Result := '';
  try
    Result := FindByLcid(ALcid).Iso6393Code;
  except
  end;
end;

end.
 