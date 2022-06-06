unit GlobalKeyHook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, uUtilsMore, fgl;

type

  EGlobalKeyHookException = class(Exception);

  TKeysMap = specialize TFPGMap<string, THotKey>;

  { TGlobalKeyHook }

  TGlobalKeyHook = class
  private
    WndHandle: HWND;
    UniqueIdentifier: String;
    KeysMap: TKeysMap;

    procedure RegisterKey(AHotKeyId: Integer; const AHotKey: THotKey); overload;
    procedure UnregisterKey(AHotKeyId: Integer); overload;
    procedure UnregisterAllKeys;
    function GetFullStringId(AStringId: string): String;
  public
    constructor Create(AWndHandle: HWND; const AUniqueIdentifier: string);
    destructor Destroy; override;

    procedure RegisterKey(const AStringId: String; const AHotKey: THotKey); overload;
    procedure UnregisterKey(const AStringId: String); overload;
    function FindHotKey(const AStringId: String): THotKey;
    function HotKeyId(const AStringId: String): integer;
  end;

implementation

{$IFOPT D+}
procedure DebugMsg(const Msg: String);
begin
  OutputDebugString(PChar(Msg))
end;
{$ELSE}
procedure DebugMsg(const Msg: String);
begin

end;
{$ENDIF}

{ TGlobalKeyHook }

procedure TGlobalKeyHook.RegisterKey(AHotKeyId: Integer; const AHotKey: THotKey
  );
var
  Modifiers: UINT = 0;
begin
  if AHotKeyId = 0 then
    raise EGlobalKeyHookException.Create('AHotKeyId must not be zero');

  if ssAlt in AHotKey.ShiftState then
    Modifiers := Modifiers + MOD_ALT;

  if ssShift in AHotKey.ShiftState then
    Modifiers := Modifiers + MOD_SHIFT;

  if ssCtrl in AHotKey.ShiftState then
    Modifiers := Modifiers + MOD_CONTROL;

  // Delete previous hotkey with current ID (if exists)
  UnregisterKey(AHotKeyId);

  if not RegisterHotKey(WndHandle, AHotkeyId, Modifiers, AHotKey.Key) then
    raise EGlobalKeyHookException.CreateFmt('Failed to register hot key (RegisterHotKey, error %d)',
                              [GetLastError]);
end;

procedure TGlobalKeyHook.UnregisterKey(AHotKeyId: Integer);
begin
  //if AHotKeyId = 0 then
  //  raise EGlobalKeyHookException.Create('AHotKeyId must not be zero');

  {if not} UnregisterHotKey(WndHandle, AHotKeyId) {then} ;
    // ToDo: check result
end;

procedure TGlobalKeyHook.UnregisterAllKeys;
var
  I: Integer;
begin
  for I := KeysMap.Count - 1 downto 0 do
  begin
    UnregisterKey(KeysMap.Keys[I]);
  end;
end;

function TGlobalKeyHook.GetFullStringId(AStringId: string): String;
begin
  Result := UniqueIdentifier + '_' + AStringId;
end;

constructor TGlobalKeyHook.Create(AWndHandle: HWND;
  const AUniqueIdentifier: string);
begin
  WndHandle := AWndHandle;
  UniqueIdentifier := AUniqueIdentifier;

  KeysMap := TKeysMap.Create;
end;

destructor TGlobalKeyHook.Destroy;
begin
  UnregisterAllKeys;
  KeysMap.Free;

  inherited Destroy;
end;

procedure TGlobalKeyHook.RegisterKey(const AStringId: String;
  const AHotKey: THotKey);
var
  FullStrId: String;
  Id: Integer;
begin
  DebugMsg(Format('Start to register hotkey %s for %s',
      [AHotKey.ToString, AStringId]));

  // Find atom id
  FullStrId := GetFullStringId(AStringId);
  if KeysMap.IndexOf(AStringId) = -1 then
    Id := GlobalAddAtom(PChar(FullStrId))
  else
  begin
    if KeysMap.KeyData[AStringId] = AHotKey then
    begin
      DebugMsg(Format('Hotkey %s for %s not changed', [AHotKey.ToString, AStringId]));

      Exit;
    end;

    Id := GlobalFindAtom(PChar(FullStrId));
  end;

  RegisterKey(Id, AHotKey);
  KeysMap.AddOrSetData(AStringId, AHotKey);

  DebugMsg(Format('Hot key %s for %s registered with id=%d',
       [AHotKey.ToString, AStringId, id]));
end;

procedure TGlobalKeyHook.UnregisterKey(const AStringId: String);
var
  FullStrId: String;
  Id: Integer;
begin
  DebugMsg(Format('Start to unregister hotkey for %s', [AStringId]));

  if KeysMap.IndexOf(AStringId) = -1 then
  begin // Not registered yet
    DebugMsg(Format('Warning: Hotkey for %s not found', [AStringId]));
    Exit;
  end;

  FullStrId := GetFullStringId(AStringId);
  Id := GlobalFindAtom(PChar(FullStrId));
  UnregisterKey(Id);

  KeysMap.Remove(AStringId);

  DebugMsg(Format('Hot key for %s with id=%d unregistered',  [AStringId, id]));
end;

function TGlobalKeyHook.FindHotKey(const AStringId: String): THotKey;
begin
  Result := KeysMap.KeyData[AStringId];
end;

function TGlobalKeyHook.HotKeyId(const AStringId: String): integer;
var
  FullStrId: String;
begin
  FullStrId := GetFullStringId(AStringId);
  Result := GlobalFindAtom(PChar(FullStrId));

  DebugMsg(Format('%s => id %d', [AStringId, Result]));
end;



end.

