unit uAbout;

{$MODE Delphi}

interface

uses
  {Windows, Messages,} SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, {ShellApi,} LCLIntf, ExtCtrls, ButtonPanel;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    ButtonPanel: TButtonPanel;
    BuildDateValueLabel: TLabel;
    AuthorValueLabel: TLabel;
    LinkTitleLabel: TLabel;
    LicenseValueLabel: TLabel;
    LocalizationAuthorValueLabel: TLabel;
    VersionValueLabel: TLabel;
    LicenseTitleLabel: TLabel;
    Panel: TPanel;
    ProgramNameLabel: TLabel;
    VersionTitleLabel: TLabel;
    AuthorTitleLabel: TLabel;
    LinkValueLabel: TLabel;
    Logo: TImage;
    BuildDateTitleLabel: TLabel;
    LocalizationAuthorTitleLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LinkValueLabelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

uses uLocalization, uUtils, DateUtils, StrUtils;

{$R *.lfm}

const
  ProjectURLTitle = 'https://github.com/artem78/AutoScreenshot';
  ProjectURL = ProjectURLTitle + '#readme';

procedure TAboutForm.FormCreate(Sender: TObject);
const
  Bitness =
  {$IF defined(CPU64)}
    '64-bit'
  {$ElseIf defined(CPU32)}
    '32-bit'
  {$Else}
    '' // Unknown
  {$ENDIF}
  ;
  License = {'GNU General Public License v3.0'} 'GNU GPLv3';
var
  Png: TPortableNetworkGraphic;
  ResourceName: String;
begin
  Caption := Localizer.I18N('About');

  //Logo.Picture.Icon.Handle := LoadImage(HInstance, 'MAINICON', IMAGE_ICON,
  //    64, 64, LR_DEFAULTCOLOR);
  //Logo.Picture.LoadFromLazarusResource('_LOGO');
  if Screen.PixelsPerInch <= 120 then // Scale = 100% or 125%
    ResourceName := '_LOGO' // Default size with 64px width
  else // Scale = 150% or 200%
    ResourceName := '_LOGO_HIGH_DPI'; // High resolution with 128px width

  png := TPortableNetworkGraphic.Create;
  try
     Png.LoadFromResourceName(Hinstance, ResourceName);
     Logo.Picture.Graphic := Png;
  finally
     Png.Free;
  end;

  ProgramNameLabel.Caption := Application.Title;

  VersionTitleLabel.Caption := Localizer.I18N('Version') + ':';
  VersionValueLabel.Caption := Format('%s (%s) %s', [
          GetProgramVersionStr(), Bitness,
          IfThen(IsPortable, Localizer.I18N('Portable'))
  ]);
  {$IFOPT D+}
    VersionValueLabel.Caption := VersionValueLabel.Caption + '    [DEBUG BUILD]';
  {$ENDIF}

  AuthorTitleLabel.Caption := Localizer.I18N('Author') + ':';
  AuthorValueLabel.Caption := 'Artem Demin (artem78) <megabyte1024@ya.ru>';
  with Localizer.LanguageInfo do
  begin
    if (Code <> 'en') and (Author <> '') then
    begin
      LocalizationAuthorTitleLabel.Caption := Localizer.I18N('LocalizationAuthor') + ':';
      LocalizationAuthorTitleLabel.Show;

      LocalizationAuthorValueLabel.Caption := Author;
      LocalizationAuthorValueLabel.Show;
    end
    else
    begin
      LocalizationAuthorTitleLabel.Hide;
      LocalizationAuthorValueLabel.Hide;
    end;
  end;

  LinkTitleLabel.Caption := Localizer.I18N('HomePage') + ':';
  LinkValueLabel.Caption := ProjectURLTitle;

  BuildDateTitleLabel.Caption := Localizer.I18N('BuildDate') + ':';
  BuildDateValueLabel.Caption := FormatDateTime('dddddd', GetBuildDateTime);

  LicenseTitleLabel.Caption := Localizer.I18N('License') + ':';
  LicenseValueLabel.Caption := License;

  ButtonPanel.CloseButton.Caption := Localizer.I18N('Close');

  // FixMe: Close button icon not hidden
end;

procedure TAboutForm.LinkValueLabelClick(Sender: TObject);
begin
  OpenURL(ProjectURL);
end;

end.
