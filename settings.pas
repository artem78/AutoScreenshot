unit Settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type

  { TSettings }

  TSettings = class
  private
    Ini: TIniFile;


  public
    constructor Create;
    destructor Destroy; override;

    // todo: переместить сюда остальные настройки...
    //...

    // debug, только чтение
    function Debug_ProgramVersion: string;
    function Debug_SimulateOldFilesDeletion: Boolean;
    function Debug_WalletsApiUrl: string;

  end;

var
  Cfg: TSettings;

implementation

uses uUtils, FileUtil;

const
  DEFAULT_INI_SECTION = 'main';
  DEBUG_INI_SECTION = 'debug';

{ TSettings }

constructor TSettings.Create;
var
  IniFileName:string;
begin
  if IsPortable then
    IniFileName := ConcatPaths([ProgramDirectory, 'config.ini'])
  else
    IniFileName := ConcatPaths([GetAppConfigDir(False), 'config.ini']);

  Ini := TIniFile.Create(IniFileName);
end;

destructor TSettings.Destroy;
begin
  Ini.free;

  inherited Destroy;
end;

function TSettings.Debug_ProgramVersion: string;
begin
  Result := ini.ReadString(DEBUG_INI_SECTION,'ProgramVersion','');
end;

function TSettings.Debug_SimulateOldFilesDeletion: Boolean;
begin
  Result := ini.ReadBool(DEBUG_INI_SECTION, 'SimulateOldFilesDeletion', False);
end;

function TSettings.Debug_WalletsApiUrl: string;
begin
  Result := Ini.ReadString(DEBUG_INI_SECTION, 'WalletsApiUrl', '');
end;

initialization
  Cfg := TSettings.Create;

finalization
  FreeAndNil(Cfg);

end.

