object AboutForm: TAboutForm
  Left = 192
  Top = 124
  BorderStyle = bsDialog
  Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
  ClientHeight = 167
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 176
    Height = 37
    Caption = 'Auto Screen'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -32
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 70
    Height = 13
    Caption = #1042#1077#1088#1089#1080#1103': X.X.X'
  end
  object Label3: TLabel
    Left = 16
    Top = 88
    Width = 133
    Height = 13
    Caption = #1044#1072#1090#1072' '#1089#1073#1086#1088#1082#1080': XX.XX.XXXX'
  end
  object btnClose: TButton
    Left = 128
    Top = 128
    Width = 97
    Height = 25
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 0
    OnClick = btnCloseClick
  end
end
