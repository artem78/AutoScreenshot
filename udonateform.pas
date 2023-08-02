unit uDonateForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TDonateForm }

  TDonateForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    procedure CopyWalletToClipboard(ASender: TObject);
  public

  end;

var
  DonateForm: TDonateForm;

implementation

uses StdCtrls, Clipbrd, uLocalization;

{$R *.lfm}

{ TDonateForm }

procedure TDonateForm.FormCreate(Sender: TObject);
var
  Wallets: TStringList;
  I: Integer;
begin
  Caption := Localizer.I18N('Donate');

  Wallets := TStringList.Create;
  try
    with Wallets do
    begin;
      AddPair('PayPal', 'megabyte1024@yandex.com');
      AddPair('ETH Ethereum / Tether USDT', '0xB14C877b2eAF7E3b4b49df25039122C0545edA74');
      AddPair('Webmoney WMZ', 'Z598881055273');
    end;

    for I := 0 to Wallets.Count - 1 do
    begin
      with TLabel.Create(Self) do
      begin
        Caption := Wallets.Names[I] + ':';
        BorderSpacing.CellAlignVertical := ccaCenter;
        BorderSpacing.CellAlignHorizontal := ccaRightBottom;
        Parent := Self;
      end;

      with TEdit.Create(Self) do
      begin
        Width := 300;
        Constraints.MinWidth := Width;
        Text := Wallets.ValueFromIndex[I];
        ReadOnly := True;
        BorderSpacing.CellAlignVertical := ccaCenter;
        Parent := Self;
      end;

      with TButton.Create(Self) do
      begin
        Caption := Localizer.I18N('Copy');
        OnClick := @CopyWalletToClipboard;
        BorderSpacing.CellAlignVertical := ccaCenter;
        Parent := Self;
      end;
    end;
  finally
    Wallets.Free;
  end;
end;

procedure TDonateForm.CopyWalletToClipboard(ASender: TObject);
  function FindPrevComponent(AComponent: TComponent): TComponent;
  begin
    if not AComponent.HasParent then
      Result := Nil
    else
    begin
      if AComponent.ComponentIndex > 0 then
        Result := AComponent.GetParentComponent.Components[AComponent.ComponentIndex - 1]
      else
        Result := Nil;
    end;
  end;

var
  Component: TEdit;
begin
  Component := TEdit(FindPrevComponent(TComponent(ASender)));
  //Component.SelectAll;
  //Component.CopyToClipboard;
  Clipboard.AsText := Component.Text;
end;

end.

