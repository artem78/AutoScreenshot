unit GlobalKeyHook;

// ToDo: Not implemented for Linux

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType,
  {$IfDef Windows}
  Windows,
  {$EndIf}
  uUtilsMore, fgl, LazLoggerBase;

type

  EGlobalKeyHookException = class(Exception);

  TKeysMap = specialize TFPGMap<string, THotKey>;

  { TGlobalKeyHook }

  TGlobalKeyHook = class
  {$IfDef Windows}
  private
    WndHandle: HWND;
    UniqueIdentifier: String;
    KeysMap: TKeysMap;

    procedure RegisterKey(AHotKeyId: Integer; const AHotKey: THotKey); overload;
    procedure UnregisterKey(AHotKeyId: Integer); overload;
    procedure UnregisterAllKeys;
    function GetFullStringId(AStringId: string): String;
    function GetShortStringId(AFullStrId: String): String;
  {$EndIf}
  public
    constructor Create(AWndHandle: HWND; const AUniqueIdentifier: string);
    destructor Destroy; override;

    procedure RegisterKey(const AStringId: String; const AHotKey: THotKey);{$IfDef Windows}  overload;{$EndIf}
    procedure UnregisterKey(const AStringId: String);{$IfDef Windows} overload;{$EndIf}
    function FindHotKey(const AStringId: String): THotKey;
    function HotKeyId(const AStringId: String): integer;

    {$IfDef Windows}
    function IdToStrId(AnId: Integer): String;
    {$EndIf}
  end;

implementation

uses {LCLType,} StrUtils;

{ TGlobalKeyHook }

{$IfDef Windows}
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

function TGlobalKeyHook.GetShortStringId(AFullStrId: String): String;
begin
  Result := AFullStrId;
  if StartsStr(UniqueIdentifier + '_', Result) then
    Delete(Result, 1, Length(UniqueIdentifier + '_'));
  {else raise ...}
end;
{$EndIf}

constructor TGlobalKeyHook.Create(AWndHandle: HWND;
  const AUniqueIdentifier: string);
begin
  {$IfDef Windows}
  WndHandle := AWndHandle;
  UniqueIdentifier := AUniqueIdentifier;

  KeysMap := TKeysMap.Create;
  {$EndIf}
end;

destructor TGlobalKeyHook.Destroy;
begin
  {$IfDef Windows}
  UnregisterAllKeys;
  KeysMap.Free;
  {$EndIf}

  inherited Destroy;
end;

procedure TGlobalKeyHook.RegisterKey(const AStringId: String;
  const AHotKey: THotKey);
{$IfDef Windows}
var
  FullStrId: String;
  Id: Integer;
begin
  if AHotKey.IsEmpty then
  begin // No key set
    UnregisterKey(AStringId);
    Exit;
  end;

  DebugLn('Start to register hotkey %s for %s',
      [AHotKey.ToString, AStringId]);

  // Find atom id
  FullStrId := GetFullStringId(AStringId);
  if KeysMap.IndexOf(AStringId) = -1 then
    Id := GlobalAddAtom(PChar(FullStrId))
  else
  begin
    if KeysMap.KeyData[AStringId] = AHotKey then
    begin
      DebugLn('Hotkey %s for %s not changed', [AHotKey.ToString, AStringId]);

      Exit;
    end;

    Id := GlobalFindAtom(PChar(FullStrId));
  end;

  RegisterKey(Id, AHotKey);
  KeysMap.AddOrSetData(AStringId, AHotKey);

  DebugLn('Hot key %s for %s registered with id=%d',
       [AHotKey.ToString, AStringId, id]);
  DebugLn('Total registered hot keys: %d', [KeysMap.Count]);
end;
{$EndIf}
{$IfDef Linux}
begin

end;
{$EndIf}

procedure TGlobalKeyHook.UnregisterKey(const AStringId: String);
{$IfDef Windows}
var
  FullStrId: String;
  Id: Integer;
begin
  DebugLn('Start to unregister hotkey for %s', [AStringId]);

  if KeysMap.IndexOf(AStringId) = -1 then
  begin // Not registered yet
    DebugLn('Warning: Hotkey for %s not found', [AStringId]);
    Exit;
  end;

  FullStrId := GetFullStringId(AStringId);
  Id := GlobalFindAtom(PChar(FullStrId));
  UnregisterKey(Id);

  KeysMap.Remove(AStringId);

  DebugLn('Hot key for %s with id=%d unregistered',  [AStringId, id]);
  DebugLn('Total registered hot keys: %d', [KeysMap.Count]);
end;
{$EndIf}
{$IfDef Linux}
begin

end;
{$EndIf}

function TGlobalKeyHook.FindHotKey(const AStringId: String): THotKey;
begin
  {$IfDef Windows}
  // Returns VK_UNKNOWN if hot key not found
  KeysMap.TryGetData(AStringId, Result);
  {$EndIf}
  {$IfDef Linux}
  Result.Key:=VK_UNKNOWN;
  {$EndIf}
end;

function TGlobalKeyHook.HotKeyId(const AStringId: String): integer;
{$IfDef Windows}
var
  FullStrId: String;
begin
  FullStrId := GetFullStringId(AStringId);
  Result := GlobalFindAtom(PChar(FullStrId));

  DebugLn('%s => id %d', [AStringId, Result]);
end;
{$EndIf}
{$IfDef Linux}
begin
  result:=0;
end;
{$EndIf}

{$IfDef Windows}
function TGlobalKeyHook.IdToStrId(AnId: Integer): String;
const
  MaxLength = 255;
var
  Buffer: array[0..MaxLength-1] of char;
  Len: UINT;
begin
  Len := GlobalGetAtomName(AnId, Buffer, MaxLength);

  if (Len = 0) then
    raise EGlobalKeyHookException.CreateFmt('Not found string id for id=%d', [AnId]);

  Result := Buffer;
  Result := GetShortStringId(Result);
end;
{$EndIf}

end.

