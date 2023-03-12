unit CheckForUpdatesFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TCheckForUpdatesForm }

  TCheckForUpdatesForm = class(TForm)
    StatusLabel: TLabel;
    ProgressBar: TProgressBar;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  CheckForUpdatesForm: TCheckForUpdatesForm;

implementation

uses uLocalization;

{$R *.lfm}

{ TCheckForUpdatesForm }

procedure TCheckForUpdatesForm.FormCreate(Sender: TObject);
begin
  StatusLabel.Caption := Localizer.I18N('CheckingForUpdates');
end;

end.

