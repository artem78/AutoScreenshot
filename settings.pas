unit Settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type

  { TSettings }

  TSettings = class
  //private
  public // todo потом сделать опять private
    Ini: TIniFile;
  private
    procedure SetLastCheckForUpdates(AVal: TDateTime);
    function GetLastCheckForUpdates: TdateTime;
    procedure SetIncludeCursor(AVal: boolean);
    function GetIncludeCursor: boolean;


  public
    constructor Create;
    destructor Destroy; override;

    // todo: переместить сюда остальные настройки...
    //...

    property LastCheckForUpdates: tdatetime read GetLastCheckForUpdates write SetLastCheckForUpdates;

    property IncludeCursor: boolean read GetIncludeCursor write SetIncludeCursor;

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

procedure TSettings.SetLastCheckForUpdates(AVal: TDateTime);
begin
  ini.WriteDateTime(DEFAULT_INI_SECTION, 'LastCheckForUpdates', AVal);
end;

function TSettings.GetLastCheckForUpdates: TdateTime;
begin
  ini.ReadDateTime(DEFAULT_INI_SECTION, 'LastCheckForUpdates', 0);
end;

procedure TSettings.SetIncludeCursor(AVal: boolean);
begin
  ini.WriteBool(DEFAULT_INI_SECTION, 'IncludeCursor', aval);
end;

function TSettings.GetIncludeCursor: boolean;
begin
  ini.ReadBool(DEFAULT_INI_SECTION, 'IncludeCursor', False);
end;

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

