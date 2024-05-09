unit uDonateForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TDonateForm }

  TDonateForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure CopyWalletToClipboard(ASender: TObject);
    class procedure LoadWalletsData(ASL: TStringList); static;
    class procedure OpenWebPage; static;
  public

  end;

var
  DonateForm: TDonateForm;

implementation

uses StdCtrls, Clipbrd, LCLIntf, ExtCtrls, uLocalization, fpjson,
  opensslsockets, base64, StrUtils, fphttpclient;

{$R *.lfm}

type

  { TPictureHelper }

  TPictureHelper = class helper for TPicture
    procedure LoadFromBase64(const AStr: String);
  end;

{ TPictureHelper }

procedure TPictureHelper.LoadFromBase64(const AStr: String);
  function Base64ToStream(const ABase64: String; var AStream: TMemoryStream): Boolean;
  var
    Str: String;
  begin
    Result := False;
    if Length(Trim(ABase64)) = 0 then
      Exit;

    Str := DecodeStringBase64(ABase64);
    AStream.Write(Pointer(Str)^, Length(Str) div SizeOf(Char));
    AStream.Position := 0;
    Result := True;
  end;

var
  IconStrm: TMemoryStream;
begin
  IconStrm := TMemoryStream.Create;
  try
    if not Base64ToStream(AStr, IconStrm) then
      raise Exception.Create('Can''t load picture from base64 string');

    Self.LoadFromStream(IconStrm);
  finally
    IconStrm.Free;
  end;
end;

{ TDonateForm }

procedure TDonateForm.FormCreate(Sender: TObject);
var
  Wallets: TStringList;
  I: Integer;
  IconBase64: String;
begin
  Caption := Localizer.I18N('Donate');

  Wallets := TStringList.Create;
  try
    try
      LoadWalletsData(Wallets);

      for I := 0 to Wallets.Count - 1 do
      begin
        IconBase64 := ExtractWord(2, Wallets.ValueFromIndex[I], [#9]);
        if not IconBase64.IsEmpty then
        begin
          with TImage.Create(Self) do
          begin
            Picture.LoadFromBase64(IconBase64);
            BorderSpacing.CellAlignVertical := ccaCenter;
            BorderSpacing.CellAlignHorizontal := {ccaCenter} ccaRightBottom;
            Parent := Self;
          end;
        end
        else
        begin
          // Create any dummy empty control to prevent layout broken when no icon
          with TLabel.Create(Self) do
          begin
            Text := '';
            AutoSize := True;
            Parent := Self;
          end;
        end;

        with TLabel.Create(Self) do
        begin
          Caption := Wallets.Names[I] + ':';
          BorderSpacing.CellAlignVertical := ccaCenter;
          //BorderSpacing.CellAlignHorizontal := ccaRightBottom;
          Parent := Self;
        end;

        with TEdit.Create(Self) do
        begin
          Width := 300;
          Constraints.MinWidth := Width;
          Text := ExtractWord(1, Wallets.ValueFromIndex[I], [#9]);
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
  except
    // No action needed there
  end;
end;

procedure TDonateForm.FormShow(Sender: TObject);
begin
  if ComponentCount = 0 then
  begin
    // Open Donate url in web browser as fallback if something goes wrong
    OpenWebPage;
    Close;
  end
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
  Clipboard.AsText := Component.Text;
end;

class procedure TDonateForm.LoadWalletsData(ASL: TStringList);
const
  ApiUrl = 'https://api.github.com/gists/6c79ab382865da9b598927194c52eb09';
var
  Http: TFPHTTPClient;
  Json: TJSONData;
  Str: String;
  Enumerator: TBaseJSONEnumerator;
  PaymentMethod, WalletID, IconBase64: String;
begin
  ASL.Clear;

  Http := TFPHttpClient.Create(Nil);
  try
    Http.AllowRedirect := True;
    Http.AddHeader('Accept', 'application/vnd.github+json');
    Http.AddHeader('User-Agent', 'Auto Screenshot');
    Json := GetJSON(Http.Get(ApiUrl));
    try
      Str := TJSONObject(Json).Objects['files'].Objects['donate_wallets.json'].Strings['content'];
    finally
      Json.Free;
    end;

    Json := GetJSON(Str);
    try
      Enumerator := TJSONObject(Json).GetEnumerator;
      try
        while Enumerator.MoveNext do
        begin
          with TJSONArray(Enumerator.Current.Value) do
          begin
            PaymentMethod := Items[0].AsString;
            WalletID      := Items[1].AsString;
            IconBase64 := '';
            try
              if Count > 2 then
                IconBase64 := TJSONObject(Items[2]).Get('icon', '');
            except
            end;
          end;
          ASL.AddPair(PaymentMethod, WalletID + #9 + IconBase64);
        end;
      finally
        Enumerator.Free;
      end;
    finally
      Json.Free;
    end;

  finally
    Http.Free;
  end;
end;

class procedure TDonateForm.OpenWebPage;
var
  Url: String;
begin
  case Localizer.LanguageInfo.Code of
    'fr':
      Url := 'https://github.com/artem78/AutoScreenshot/blob/master/docs/README-fr.md#faire-un-don';
    'ru', 'uk':
      Url := 'https://github.com/artem78/AutoScreenshot/blob/master/docs/README-ru.md#%D0%B2%D0%BE%D0%B7%D0%BD%D0%B0%D0%B3%D1%80%D0%B0%D0%B4%D0%B8%D1%82%D1%8C-%D0%B0%D0%B2%D1%82%D0%BE%D1%80%D0%B0-%D0%BC%D0%B0%D1%82%D0%B5%D1%80%D0%B8%D0%B0%D0%BB%D1%8C%D0%BD%D0%BE';
    else
      Url := 'https://github.com/artem78/AutoScreenshot/tree/master#donate';
  end;

  OpenURL(Url);
end;

end.

