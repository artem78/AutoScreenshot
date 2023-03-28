unit XRandREventWatcher;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, x, xlib, xrandr, ctypes;

type
  TNotifyProc = procedure(const AEvent: TXEvent) of object;

  { TXRandREventWatcherThread }

  TXRandREventWatcherThread = class(TThread)
  private
    FNotifier: TNotifyProc;
    FEventMask: cint;
    FLastEvent: TXEvent;
    procedure Notify;
  protected
    procedure Execute; override;
  public
    Constructor Create({ACreateSuspended: boolean;} AEventMask: cint;
        ANotifier: TNotifyProc);
    destructor Destroy; override;
  end;

implementation

{ TXRandREventWatcherThread }

procedure TXRandREventWatcherThread.Notify;
begin
  if Assigned(FNotifier) then
    FNotifier(FLastEvent);
end;

procedure TXRandREventWatcherThread.Execute;
var
  //DisplayName: String;
  Display: PDisplay;
  RootWnd: TWindow;
begin
  //DisplayName := GetEnvironmentVariable('DISPLAY');
  Display := XOpenDisplay({PChar(DisplayName)} Nil);
  RootWnd := RootWindow(Display, DefaultScreen(Display));

  XRRSelectInput(Display, RootWnd, FEventMask);

  while True do
  begin
    if XPending(Display) > 0 then
    begin
      XNextEvent(Display, @FLastEvent);
      Synchronize(@Notify);
    end;

    if Terminated then
      Break;

    Sleep(200);
  end;

  XCloseDisplay(Display);
end;

constructor TXRandREventWatcherThread.Create(AEventMask: cint;
  ANotifier: TNotifyProc);
begin
  FNotifier := ANotifier;
  FEventMask := AEventMask;
  FreeOnTerminate := {True} False;

  inherited Create(False);
end;

destructor TXRandREventWatcherThread.Destroy;
begin
  Terminate;
  WaitFor;
  inherited Destroy;
end;

end.

