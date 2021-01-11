unit uAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellApi, ExtCtrls, TntForms, TntStdCtrls, TntExtCtrls;

type
  TAboutForm = class(TTntForm)
    ProgramNameLabel: TTntLabel;
    VersionLabel: TTntLabel;
    AuthorLabel: TTntLabel;
    CloseButton: TTntButton;
    LinkLabel: TTntLabel;
    Logo: TTntImage;
    BuildDateLabel: TTntLabel;
    LocalizationAuthorLabel: TTntLabel;
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

uses uLocalization, uUtils, DateUtils;

{$R *.dfm}

const
  ProjectGitHubURL{: WideString} = 'https://github.com/artem78/AutoScreenshot#readme';

procedure TAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
var
  BuildDate: TDateTime;
  BuildDateStr: WideString;
begin
  Caption := Localizer.I18N('About');

  Logo.Picture.Icon.Handle := LoadImage(HInstance, 'MAINICON', IMAGE_ICON,
      64, 64, LR_DEFAULTCOLOR);

  ProgramNameLabel.Caption := TntApplication.Title;
  VersionLabel.Caption := Localizer.I18N('Version') + ': ' + GetProgramVersionStr(True);
  AuthorLabel.Caption := Localizer.I18N('Author') + ': ' + 'Artem Demin (artem78) <megabyte1024@ya.ru>';
  with Localizer.LanguageInfo do
  begin
    if (Code <> 'en') and (Author <> '') then
    begin
      LocalizationAuthorLabel.Caption := Localizer.I18N('LocalizationAuthor') + ': ' + Author;
      LocalizationAuthorLabel.Show;
    end
    else
      LocalizationAuthorLabel.Hide;
  end;
  LinkLabel.Caption := ProjectGitHubURL;

  BuildDate := GetLinkerTimeStamp;
  { Check if date is correct
    (Older versions of Delphi may put incorrect TimeDateStamp in exe
    without this patch - http://cc.embarcadero.com/Item/19823) }
  if YearOf(BuildDate) < 2000 then
    BuildDateStr := 'unknown'
  else
    BuildDateStr := FormatDateTime({'dddddd tt'} 'dddddd', BuildDate);
  BuildDateLabel.Caption := Localizer.I18N('BuildDate') + ': ' + BuildDateStr;

  CloseButton.Caption := Localizer.I18N('Close');
end;

procedure TAboutForm.LinkLabelClick(Sender: TObject);
begin
  ShellExecute(handle, 'open', ProjectGitHubURL, nil, nil, SW_SHOW);
end;

end.
