object AboutForm: TAboutForm
  Left = 192
  Top = 124
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 209
  ClientWidth = 481
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  DesignSize = (
    481
    209)
  PixelsPerInch = 96
  TextHeight = 15
  object ProgramNameLabel: TTntLabel
    Left = 104
    Top = 16
    Width = 174
    Height = 36
    Caption = 'Auto Screen'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8611623
    Font.Height = -32
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object VersionLabel: TTntLabel
    Left = 104
    Top = 64
    Width = 74
    Height = 15
    Caption = 'Version: X.X.X'
  end
  object AuthorLabel: TTntLabel
    Left = 104
    Top = 88
    Width = 50
    Height = 15
    Caption = 'Author: ...'
  end
  object LinkLabel: TTntLabel
    Left = 104
    Top = 136
    Width = 25
    Height = 15
    Cursor = crHandPoint
    Caption = 'Url...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = LinkLabelClick
  end
  object Logo: TTntImage
    Left = 16
    Top = 16
    Width = 64
    Height = 64
  end
  object BuildDateLabel: TTntLabel
    Left = 216
    Top = 64
    Width = 123
    Height = 15
    Caption = 'Build date: XX.XX.XXXX'
  end
  object LocalizationAuthorLabel: TTntLabel
    Left = 104
    Top = 112
    Width = 80
    Height = 15
    Caption = 'Localization: ...'
  end
  object CloseButton: TTntButton
    Left = 192
    Top = 170
    Width = 97
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Close'
    TabOrder = 0
    OnClick = CloseButtonClick
  end
end
