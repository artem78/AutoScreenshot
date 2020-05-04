object AboutForm: TAboutForm
  Left = 192
  Top = 124
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 191
  ClientWidth = 405
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
    Left = 104
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
    Left = 104
    Top = 64
    Width = 68
    Height = 13
    Caption = 'Version: X.X.X'
  end
  object AuthorLabel: TLabel
    Left = 104
    Top = 88
    Width = 46
    Height = 13
    Caption = 'Author: ...'
  end
  object LinkLabel: TLabel
    Left = 104
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
  object Logo: TImage
    Left = 16
    Top = 16
    Width = 64
    Height = 64
  end
  object CloseButton: TButton
    Left = 160
    Top = 152
    Width = 97
    Height = 25
    Caption = 'Close'
    TabOrder = 0
    OnClick = CloseButtonClick
  end
end
