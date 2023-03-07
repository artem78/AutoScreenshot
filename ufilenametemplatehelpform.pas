unit uFileNameTemplateHelpForm;

{This dialog is used as a replacement of standard ShowMessage() because it can`t
 show tabs on Windows (possible WinApi bug). Discussion:
    https://forum.lazarus.freepascal.org/index.php/topic,55804.0.html
    http://www.freepascal.ru/forum/viewtopic.php?f=13&t=43290&p=162762#p162762
}

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type

  { TFileNameTemplateHelpForm }

  TFileNameTemplateHelpForm = class(TForm)
    ButtonPanel: TButtonPanel;
    TextLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FileNameTemplateHelpForm: TFileNameTemplateHelpForm;

implementation

uses
  uLocalization;

{$R *.lfm}

{ TFileNameTemplateHelpForm }

procedure TFileNameTemplateHelpForm.FormCreate(Sender: TObject);
begin
  TextLabel.Caption := Localizer.I18N('FileNameTemplateHelpText');
  ButtonPanel.CloseButton.Caption := Localizer.I18N('Close');
end;

end.

