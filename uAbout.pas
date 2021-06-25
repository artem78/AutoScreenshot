unit uAbout;

{$MODE Delphi}

interface

uses
  {Windows, Messages,} SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, {ShellApi,} LCLIntf, ExtCtrls{, TntForms, TntStdCtrls, TntExtCtrls};

type

  { TAboutForm }

  TAboutForm = class(TForm)
    ProgramNameLabel: TLabel;
    VersionLabel: TLabel;
    AuthorLabel: TLabel;
    CloseButton: TButton;
    LinkLabel: TLabel;
    Logo: TImage;
    BuildDateLabel: TLabel;
    LocalizationAuthorLabel: TLabel;
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

{$R *.lfm}

const
  ProjectGitHubURL{: WideString} = 'https://github.com/artem78/AutoScreenshot#readme';

procedure TAboutForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
var
  BuildDateTime: TDateTime;
  BuildStr: WideString;
  Png: TPortableNetworkGraphic;
begin
  Caption := Localizer.I18N('About');

  //Logo.Picture.Icon.Handle := LoadImage(HInstance, 'MAINICON', IMAGE_ICON,
  //    64, 64, LR_DEFAULTCOLOR);
  //Logo.Picture.LoadFromLazarusResource('_LOGO');
  png := TPortableNetworkGraphic.Create;
  try
     Png.LoadFromResourceName(Hinstance, '_LOGO');
     Logo.Picture.Graphic := Png;
  finally
     Png.Free;
  end;

  ProgramNameLabel.Caption := Application.Title;
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

  BuildDateTime := GetBuildDateTime;
  BuildStr := FormatDateTime({'dddddd tt'} 'dddddd', BuildDateTime);
  BuildDateLabel.Caption := Localizer.I18N('BuildDate') + ': ' + BuildStr;

  CloseButton.Caption := Localizer.I18N('Close');
end;

procedure TAboutForm.LinkLabelClick(Sender: TObject);
begin
   //OpenDocument(ProjectGitHubURL); { *Преобразовано из ShellExecute* }
  OpenURL(ProjectGitHubURL);
end;

end.
