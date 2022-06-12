unit uIniHelper;

{$mode objfpc}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, IniFiles, uUtilsMore;

type

  { TIniFilesHelper }

  TIniFilesHelper = class helper for TIniFile
  public
    function ReadHotKey(const Section, Ident: string; const Default: THotKey): THotKey;
    procedure WriteHotKey(const Section, Ident: String; const Value: THotKey);
  end;

implementation

{ TIniFilesHelper }

function TIniFilesHelper.ReadHotKey(const Section, Ident: string;
  const Default: THotKey): THotKey;
var
  ResultStr: String;
begin
  ResultStr := ReadString(Section, Ident, Default.ToString);

  try
    Result.Parse(ResultStr);
  except
    Result := Default;
  end;
end;

procedure TIniFilesHelper.WriteHotKey(const Section, Ident: String;
  const Value: THotKey);
begin
  WriteString(Section, Ident, Value.ToString);
end;

end.

