object AboutForm: TAboutForm
  Left = 192
  Top = 124
  BorderStyle = bsDialog
  Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
  ClientHeight = 191
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ProgramNameLabel: TLabel
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
  object VersionLabel: TLabel
    Left = 16
    Top = 64
    Width = 70
    Height = 13
    Caption = #1042#1077#1088#1089#1080#1103': X.X.X'
  end
  object AutorLabel: TLabel
    Left = 16
    Top = 88
    Width = 229
    Height = 13
    Caption = #1040#1074#1090#1086#1088': artem78      e-mail: megabyte1024@ya.ru'
  end
  object LinkLabel: TLabel
    Left = 16
    Top = 112
    Width = 22
    Height = 13
    Cursor = crHandPoint
    Caption = 'Url...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = LinkLabelClick
  end
  object CloseButton: TButton
    Left = 112
    Top = 152
    Width = 97
    Height = 25
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 0
    OnClick = CloseButtonClick
  end
end
