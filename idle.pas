unit Idle;

// FixMe: In Linux relies on X11. What about Wayland and others?

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{ Returns how much time passed (in milliseconds) since last user activity
  (mouse or keyboard events). Also works if current application window has
  no focus (hidden or minimized). }
function UserIdleTime: Cardinal;

implementation

uses
  {$IfDef Windows}
  Windows
  {$EndIf}
  {$IfDef Linux}
  x, xlib, dynlibs, ctypes
  {$EndIf}
  ;

{$IfDef Windows}
type
  TLastInputInfo = record
    cbSize: UINT;
    dwTime: DWORD;
  end;

function GetLastInputInfo(var plii: TLastInputInfo): BOOL; stdcall; external 'user32' name 'GetLastInputInfo';
{$EndIf}

{$IfDef Linux}
type
  TXScreenSaverInfo = record
    Window: culong;
    State: cint;
    Kind: cint;
    TilOrSince: culong;
    Idle: culong;
    EventMask: culong;
  end;
  PXScreenSaverInfo = ^TXScreenSaverInfo;

  TXScreenSaverAllocInfoFunc = function: Pointer {PXScreenSaverInfo}; cdecl;
  TXScreenSaverQueryInfoFunc = function(ADisplay: PXDisplay; ADrawable: TDrawable;
      AScreenSaverInfo: PXScreenSaverInfo): TStatus; cdecl;

var
  XScreenSaverLib: TLibHandle;
  XScreenSaverAllocInfo: TXScreenSaverAllocInfoFunc;
  XScreenSaverQueryInfo: TXScreenSaverQueryInfoFunc;

{$EndIf}

function UserIdleTime: Cardinal;
{$IfDef Windows}
var
  LastInputInfo: TLastInputInfo;
begin
  LastInputInfo.cbSize := SizeOf(TLastInputInfo);
  if GetLastInputInfo(LastInputInfo) then
    Result := GetTickCount - LastInputInfo.dwTime
  else
    Result := 0;
end;
{$EndIf}
{$IfDef Linux}
var
  DisplayName: String;
  Display: PDisplay;
  RootWnd: TWindow;
  XScreenSaverInfo: PXScreenSaverInfo;
begin
  Result := 0;

  if XScreenSaverLib = dynlibs.NilHandle then
    Exit;

  XScreenSaverInfo := XScreenSaverAllocInfo();
  if Assigned(XScreenSaverInfo) then
  begin
    DisplayName := GetEnvironmentVariable('DISPLAY');
    Display := XOpenDisplay(PChar(DisplayName));
    RootWnd := RootWindow(Display, DefaultScreen(Display));

    if XScreenSaverQueryInfo(Display, RootWnd, XScreenSaverInfo) <> 0 then
    begin
      Result := XScreenSaverInfo^.Idle;
    end;

    if (Assigned(Display)) then
      XCloseDisplay(Display);

    XFree(XScreenSaverInfo);
  end;
end;
{$EndIf}


{$IfDef Linux}
initialization
  XScreenSaverLib := LoadLibrary('libXss.so.1');
  if XScreenSaverLib = dynlibs.NilHandle then
    raise Exception.Create('Could not load library libXss');

  XScreenSaverAllocInfo := TXScreenSaverAllocInfoFunc(GetProcedureAddress(XScreenSaverLib, 'XScreenSaverAllocInfo'));
  if not Assigned(XScreenSaverAllocInfo) then
    raise Exception.Create('Could not find XScreenSaverAllocInfo function in libXss');

  XScreenSaverQueryInfo := TXScreenSaverQueryInfoFunc(GetProcedureAddress(XScreenSaverLib, 'XScreenSaverQueryInfo'));
  if not Assigned(XScreenSaverQueryInfo) then
    raise Exception.Create('Could not find XScreenSaverQueryInfo function in libXss');

finalization;
  if XScreenSaverLib <> dynlibs.NilHandle then
    UnloadLibrary(XScreenSaverLib);
{$EndIf}

end.

