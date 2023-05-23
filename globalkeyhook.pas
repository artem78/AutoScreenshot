unit GlobalKeyHook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType,
  {$IfDef Windows}
  Windows,
  {$EndIf}
  {$IfDef Linux}
  Codebot.Input.HotKeys,
  {$EndIf}
  uUtilsMore, fgl, LazLoggerBase;

type

  EGlobalKeyHookException = class(Exception);

  TKeysMap = specialize TFPGMap<string, THotKey>;

  {$IfDef Linux}
  THotKeyEvent = procedure(const AId: String) of object;
  {$EndIf}

  { TGlobalKeyHook }

  TGlobalKeyHook = class
  private
    KeysMap: TKeysMap;
    {$IfDef Windows}
    WndHandle: HWND;
    UniqueIdentifier: String;
    {$EndIf}
    {$IfDef Linux}
    HKCapture: THotkeyCapture;
    Callback: THotKeyEvent;
    {$EndIf}

    procedure UnregisterAllKeys;
    {$IfDef Windows}
    procedure RegisterKey(AHotKeyId: Integer; const AHotKey: THotKey); overload;
    procedure UnregisterKey(AHotKeyId: Integer); overload;
    function GetFullStringId(AStringId: string): String;
    function GetShortStringId(AFullStrId: String): String;
    {$EndIf}
    {$IfDef Linux}
    procedure OnHotKeyEvent(Sender: TObject; Key: Word; Shift: TShiftState);
    function FindId(const AHotKey: THotKey): String;
    {$EndIf}
  public
    constructor Create({$IfDef Windows}AWndHandle: HWND;const AUniqueIdentifier: string{$EndIf}
                       {$IfDef Linux}ACallback: THotKeyEvent {= Nil}{$EndIf});
    destructor Destroy; override;

    procedure RegisterKey(const AStringId: String; const AHotKey: THotKey);{$IfDef Windows}  overload;{$EndIf}
    procedure UnregisterKey(const AStringId: String);{$IfDef Windows} overload;{$EndIf}
    function FindHotKey(const AStringId: String): THotKey;

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
{$EndIf}

procedure TGlobalKeyHook.UnregisterAllKeys;
var
  I: Integer;
begin
  for I := KeysMap.Count - 1 downto 0 do
  begin
    UnregisterKey(KeysMap.Keys[I]);
  end;
end;

{$IfDef Linux}
procedure TGlobalKeyHook.OnHotKeyEvent(Sender: TObject; Key: Word;
  Shift: TShiftState);
var
  HotKey: THotKey;
  Id: String;
begin
  HotKey.Key := Key;
  HotKey.ShiftState := Shift;
  Id := FindId(HotKey);

  DebugLn('Hot key %s event recieved (id=%s)', [HotKey.ToString, Id]);

  if Assigned(Callback) then
    Callback(Id);
end;

function TGlobalKeyHook.FindId(const AHotKey: THotKey): String;
var
  Idx: Integer;
begin
  Result := '';

  Idx := KeysMap.IndexOfData(AHotKey);
  if Idx <> -1 then
    Result := KeysMap.Keys[Idx];
end;
{$EndIf}

{$IfDef Windows}
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

constructor TGlobalKeyHook.Create(
  {$IfDef Windows}
  AWndHandle: HWND;
  const AUniqueIdentifier: string
  {$EndIf}
  {$IfDef Linux}
  ACallback: THotKeyEvent
  {$EndIf}
  );
begin
  {$IfDef Windows}
  WndHandle := AWndHandle;
  UniqueIdentifier := AUniqueIdentifier;
  {$EndIf}

  KeysMap := TKeysMap.Create;

  {$IfDef Linux}
  Callback := ACallback;
  HKCapture := HotkeyCapture();
  {$EndIf}
end;

destructor TGlobalKeyHook.Destroy;
begin
  UnregisterAllKeys;
  KeysMap.Free;


  (*{$IfDef Linux}
  //HKCapture.Free;
  //FreeAndNil(HKCapture);
  {$EndIf}*)

  inherited Destroy;
end;

procedure TGlobalKeyHook.RegisterKey(const AStringId: String;
  const AHotKey: THotKey);
{$IfDef Windows}
var
  FullStrId: String;
  Id: Integer;
{$EndIf}
begin
  if AHotKey.IsEmpty then
  begin // No key set
    UnregisterKey(AStringId);
    Exit;
  end;

  DebugLn('Start to register hotkey %s for %s',
      [AHotKey.ToString, AStringId]);

  {$IfDef Windows}
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
  {$EndIf}
  {$IfDef Linux}
  if (KeysMap.IndexOf(AStringId) <> -1) and (KeysMap.KeyData[AStringId] = AHotKey) then
  begin
    DebugLn('Hotkey %s for %s not changed', [AHotKey.ToString, AStringId]);

    Exit;
  end;
  HKCapture.RegisterNotify(AHotKey.Key, AHotKey.ShiftState, @OnHotKeyEvent);
  {$EndIf}

  KeysMap.AddOrSetData(AStringId, AHotKey);

  {$IfDef Windows}
  DebugLn('Hot key %s for %s registered with id=%d',
       [AHotKey.ToString, AStringId, id]);
  {$EndIf}
  {$IfDef Linux}
  DebugLn('Hot key %s for %s registered',
       [AHotKey.ToString, AStringId]);
  {$EndIf}

  DebugLn('Total registered hot keys: %d', [KeysMap.Count]);
end;

procedure TGlobalKeyHook.UnregisterKey(const AStringId: String);
var
{$IfDef Windows}
  FullStrId: String;
  Id: Integer;
{$EndIf}
{$IfDef Linux}
  HotKey: THotKey;
{$EndIf}
begin
  DebugLn('Start to unregister hotkey for %s', [AStringId]);

  if KeysMap.IndexOf(AStringId) = -1 then
  begin // Not registered yet
    DebugLn('Warning: Hotkey for %s not found', [AStringId]);
    Exit;
  end;

  {$IfDef Windows}
  FullStrId := GetFullStringId(AStringId);
  Id := GlobalFindAtom(PChar(FullStrId));
  UnregisterKey(Id);
  {$EndIf}
  {$IfDef Linux}
  HotKey := KeysMap.KeyData[AStringId];
  HKCapture.UnregisterNotify(HotKey.Key, HotKey.ShiftState);
  {$EndIf}

  KeysMap.Remove(AStringId);

  {$IfDef Windows}
  DebugLn('Hot key for %s with id=%d unregistered',  [AStringId, id]);
  {$EndIf}
  {$IfDef Linux}
  DebugLn('Hot key for %s unregistered',  [AStringId]);
  {$EndIf}

  DebugLn('Total registered hot keys: %d', [KeysMap.Count]);
end;

function TGlobalKeyHook.FindHotKey(const AStringId: String): THotKey;
begin
  // Returns VK_UNKNOWN if hot key not found
  KeysMap.TryGetData(AStringId, Result);
end;

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

