unit MyCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Menus;

type

  { TMyTrayIcon }

  TMyTrayIcon = class(TTrayIcon)
    private
      type
        TState = (tisDefault, tisBlackWhite, tisFlashAnimation);

    private
      IconIdx: 1..7;
      FIconState: TState;
      AnimationTimer: TTimer;

      procedure SetIconState(AnIconState: TState);
      procedure SetNextAnimationIcon();
      procedure OnTimer(Sender: TObject);

      property IconState: TState write SetIconState;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      procedure SetDefaultIcon;
      procedure SetInactiveIcon {SetBlackWhiteIcon};
      procedure StartFlashAnimation;
  end;

implementation

{ TMyTrayIcon }

procedure TMyTrayIcon.SetIconState(AnIconState: TState);
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

  Icon.LoadFromResourceName(HInstance, ResName);
end;

procedure TMyTrayIcon.SetNextAnimationIcon();
var
  ResName: String;
begin
  if (IconIdx < High(IconIdx)) then
  begin
    Inc(IconIdx);
    ResName := Format('_CAMERA_FLASH_%d', [IconIdx]);
    Icon.LoadFromResourceName(HInstance, ResName);
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

constructor TMyTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetDefaultIcon;
  AnimationTimer := TTimer.Create(Nil);
  AnimationTimer.OnTimer := @OnTimer;
  AnimationTimer.Interval := 160;
  AnimationTimer.Enabled := False;
end;

destructor TMyTrayIcon.Destroy;
begin
  AnimationTimer.Enabled := False;
  AnimationTimer.Free;

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

end.
