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

uses LazLoggerBase;

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
  DebugLn(ClassName, '::Execute() started');

  //DisplayName := GetEnvironmentVariable('DISPLAY');
  Display := XOpenDisplay({PChar(DisplayName)} Nil);
  RootWnd := RootWindow(Display, DefaultScreen(Display));

  XRRSelectInput(Display, RootWnd, FEventMask);

  while True do
  begin
    if XPending(Display) > 0 then
    begin
      XNextEvent(Display, @FLastEvent);
      DebugLn(['Recieved event with type ', FLastEvent._type]);
      Synchronize(@Notify);
    end;

    if Terminated then
      Break;

    Sleep(200);
  end;

  XCloseDisplay(Display);

  DebugLn(ClassName, '::Execute() ended');
end;

constructor TXRandREventWatcherThread.Create(AEventMask: cint;
  ANotifier: TNotifyProc);
begin
  FNotifier := ANotifier;
  FEventMask := AEventMask;
  FreeOnTerminate := {True} False;

  inherited Create(False);

  NameThreadForDebugging(ClassName, ThreadID);
  DebugLn('Thread ', ClassName ,' started');
end;

destructor TXRandREventWatcherThread.Destroy;
begin
  DebugLn('Prepare to destroy thread ', ClassName);

  Terminate;
  WaitFor;
  inherited Destroy;

  DebugLn('Thread ', ClassName, 'destroyed');
end;

end.

