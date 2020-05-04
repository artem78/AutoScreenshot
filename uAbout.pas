unit uAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellApi, ExtCtrls;

type
  TAboutForm = class(TForm)
    ProgramNameLabel: TLabel;
    VersionLabel: TLabel;
    AuthorLabel: TLabel;
    CloseButton: TButton;
    LinkLabel: TLabel;
    Logo: TImage;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LinkLabelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses uLocalization, uUtils;

{$R *.dfm}

const
  ProjectGitHubURL = 'https://github.com/artem78/AutoScreenshot';

procedure TAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Caption := I18N('About');

  Logo.Picture.Icon.Handle := LoadImage(HInstance, 'MAINICON', IMAGE_ICON,
      64, 64, LR_DEFAULTCOLOR);

  ProgramNameLabel.Caption := Application.Title;
  VersionLabel.Caption := I18N('Version') + ': ' + GetProgramVersionStr(True);
  AuthorLabel.Caption := I18N('Author') + ': ' + 'artem78      e-mail: megabyte1024@ya.ru';
  LinkLabel.Caption := ProjectGitHubURL;

  CloseButton.Caption := I18N('Close');
end;

procedure TAboutForm.LinkLabelClick(Sender: TObject);
begin
  ShellExecute(handle, 'open', ProjectGitHubURL, nil, nil, SW_SHOW);
end;

end.
