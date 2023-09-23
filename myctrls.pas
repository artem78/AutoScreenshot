unit MyCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Menus;

type

  TMyTrayIconState = (tisDefault, tisBlackWhite, tisFlashAnimation);

  { TMyTrayIcon }

  TMyTrayIcon = class
    private
      IconIdx: 1..7;
      FIconState: TMyTrayIconState;
      TrayIcon: TTrayIcon;
      AnimationTimer: TTimer;

      procedure SetIconState(AnIconState: TMyTrayIconState);
      procedure SetNextAnimationIcon();
      procedure OnTimer(Sender: TObject);
      procedure SetPopupMenu(APopupMenu: TPopupMenu);
      procedure SetOnDblClickEvent(AnEvent: TNotifyEvent);
      procedure SetHint(const AText: String);

      property IconState: TMyTrayIconState write SetIconState;
    public
      constructor Create;
      destructor Destroy; override;

      property PopupMenu: TPopupMenu write SetPopupMenu;
      property OnDblClick: TNotifyEvent write SetOnDblClickEvent;
      property Hint: String write SetHint;

      procedure SetDefaultIcon;
      procedure SetInactiveIcon {SetBlackWhiteIcon};
      procedure StartFlashAnimation;
      procedure Show;
      procedure Hide;
  end;

implementation

{ TMyTrayIcon }

procedure TMyTrayIcon.SetIconState(AnIconState: TMyTrayIconState);
var
  ResName: String;
begin
  if AnIconState <> tisFlashAnimation then
    FIconState := AnIconState;

  case AnIconState of
    tisBlackWhite: ResName := '_CAMERA_BW';
    tisFlashAnimation:
      begin
        IconIdx := Low(IconIdx);
        AnimationTimer.Enabled := True;
        ResName := Format('_CAMERA_FLASH_%d', [IconIdx]);
      end
    //tisDefault:
    else ResName := '_CAMERA';
  end;

  TrayIcon.Icon.LoadFromResourceName(HInstance, ResName);
end;

procedure TMyTrayIcon.SetNextAnimationIcon();
var
  ResName: String;
begin
  if (IconIdx < High(IconIdx)) then
  begin
    Inc(IconIdx);
    ResName := Format('_CAMERA_FLASH_%d', [IconIdx]);
    TrayIcon.Icon.LoadFromResourceName(HInstance, ResName);
  end
  else
  begin
    IconIdx := Low(IconIdx);
    AnimationTimer.Enabled := False; // Stop animation

    // Restore previous tray icon
    IconState := FIconState;
  end;
end;

procedure TMyTrayIcon.OnTimer(Sender: TObject);
begin
  SetNextAnimationIcon();
end;

procedure TMyTrayIcon.SetPopupMenu(APopupMenu: TPopupMenu);
begin
  TrayIcon.PopUpMenu := APopupMenu;
end;

procedure TMyTrayIcon.SetOnDblClickEvent(AnEvent: TNotifyEvent);
begin
  TrayIcon.OnDblClick := AnEvent;
end;

procedure TMyTrayIcon.SetHint(const AText: String);
begin
  TrayIcon.Hint := AText;
end;

constructor TMyTrayIcon.Create;
begin
  inherited;

  TrayIcon := TTrayIcon.Create(Nil);
  SetDefaultIcon;
  PopupMenu := Nil;
  AnimationTimer := TTimer.Create(Nil);
  AnimationTimer.OnTimer := @OnTimer;
  AnimationTimer.Interval := 160;
  AnimationTimer.Enabled := False;
end;

destructor TMyTrayIcon.Destroy;
begin
  AnimationTimer.Enabled := False;
  AnimationTimer.Free;
  TrayIcon.Free;

  inherited Destroy;
end;



procedure TMyTrayIcon.SetDefaultIcon;
begin
  IconState := tisDefault;
end;

procedure TMyTrayIcon.SetInactiveIcon;
begin
  IconState := tisBlackWhite;
end;

procedure TMyTrayIcon.StartFlashAnimation;
begin
  IconState := tisFlashAnimation;
end;

procedure TMyTrayIcon.Show;
begin
  TrayIcon.Show;
end;

procedure TMyTrayIcon.Hide;
begin
  TrayIcon.Hide;
end;

end.


