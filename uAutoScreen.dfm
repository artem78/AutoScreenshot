object MainForm: TMainForm
  Left = 192
  Top = 124
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Auto Screenshot'
  ClientHeight = 314
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object OutputDirLabel: TLabel
    Left = 122
    Top = 24
    Width = 79
    Height = 13
    Alignment = taRightJustify
    Caption = 'Saving directory:'
  end
  object CaptureIntervalLabel: TLabel
    Left = 78
    Top = 77
    Width = 123
    Height = 13
    Alignment = taRightJustify
    Caption = 'Saving interval in minutes:'
  end
  object ImageFormatLabel: TLabel
    Left = 166
    Top = 108
    Width = 35
    Height = 13
    Alignment = taRightJustify
    Caption = 'Format:'
  end
  object JPEGQualityLabel: TLabel
    Left = 306
    Top = 109
    Width = 63
    Height = 13
    Alignment = taRightJustify
    Caption = 'Compression:'
  end
  object JPEGQualityPercentLabel: TLabel
    Left = 440
    Top = 109
    Width = 8
    Height = 13
    Caption = '%'
  end
  object FileNameTemplateLabel: TLabel
    Left = 113
    Top = 50
    Width = 88
    Height = 13
    Alignment = taRightJustify
    Caption = 'Filename template:'
  end
  object OutputDirEdit: TEdit
    Left = 208
    Top = 20
    Width = 265
    Height = 21
    TabOrder = 0
    OnChange = OutputDirEditChange
  end
  object ChooseOutputDirButton: TButton
    Left = 480
    Top = 16
    Width = 33
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = ChooseOutputDirButtonClick
  end
  object CaptureInterval: TSpinEdit
    Left = 208
    Top = 72
    Width = 57
    Height = 22
    MaxValue = 99999
    MinValue = 1
    TabOrder = 2
    Value = 1
    OnChange = CaptureIntervalChange
  end
  object TakeScreenshotButton: TButton
    Left = 48
    Top = 260
    Width = 99
    Height = 25
    Caption = 'Take screenshot'
    TabOrder = 3
    OnClick = TakeScreenshotButtonClick
  end
  object JPEGQualitySpinEdit: TSpinEdit
    Left = 376
    Top = 104
    Width = 57
    Height = 22
    MaxValue = 100
    MinValue = 0
    TabOrder = 4
    Value = 0
    OnChange = JPEGQualitySpinEditChange
  end
  object OpenOutputDirButton: TButton
    Left = 520
    Top = 16
    Width = 97
    Height = 25
    Caption = 'Open directory'
    TabOrder = 5
    OnClick = OpenOutputDirButtonClick
  end
  object StopWhenInactiveCheckBox: TCheckBox
    Left = 208
    Top = 136
    Width = 329
    Height = 17
    Caption = 'Pause taking screenshots while idle'
    TabOrder = 6
    OnClick = StopWhenInactiveCheckBoxClick
  end
  object LanguageRadioGroup: TRadioGroup
    Left = 520
    Top = 160
    Width = 97
    Height = 57
    Caption = 'Language'
    Items.Strings = (
      'English'
      #1056#1091#1089#1089#1082#1080#1081)
    TabOrder = 10
    OnClick = LanguageRadioGroupClick
  end
  object ImageFormatComboBox: TComboBox
    Left = 208
    Top = 104
    Width = 57
    Height = 21
    AutoComplete = False
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 13
    TabOrder = 7
    OnChange = ImageFormatComboBoxChange
  end
  object AutoCaptureControlGroup: TGroupBox
    Left = 208
    Top = 232
    Width = 233
    Height = 65
    Caption = 'Automatic capture'
    TabOrder = 8
    object StartAutoCaptureButton: TButton
      Left = 24
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Enable'
      TabOrder = 0
      OnClick = StartAutoCaptureButtonClick
    end
    object StopAutoCaptureButton: TButton
      Left = 136
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Disable'
      TabOrder = 1
      OnClick = StopAutoCaptureButtonClick
    end
  end
  object AboutButton: TButton
    Left = 520
    Top = 256
    Width = 97
    Height = 25
    Caption = 'About'
    TabOrder = 9
    OnClick = AboutButtonClick
  end
  object StartCaptureOnStartUpCheckBox: TCheckBox
    Left = 208
    Top = 160
    Width = 265
    Height = 17
    Caption = 'Start auto capture when program starts'
    TabOrder = 11
    OnClick = StartCaptureOnStartUpCheckBoxClick
  end
  object StartMinimizedCheckBox: TCheckBox
    Left = 208
    Top = 184
    Width = 265
    Height = 17
    Caption = 'Start minimized to tray'
    TabOrder = 12
    OnClick = StartMinimizedCheckBoxClick
  end
  object FileNameTemplateComboBox: TComboBox
    Left = 208
    Top = 46
    Width = 265
    Height = 21
    ItemHeight = 13
    TabOrder = 13
    OnChange = FileNameTemplateComboBoxChange
    Items.Strings = (
      '%Y-%M-%D\%Y-%M-%D %H.%N.%S'
      '%Y-%M\%D\%Y-%M-%D_%H.%N.%S'
      '%Y\%M\%D\%H_%N_%S'
      'screenshot %Y-%M-%D %H%N%S')
  end
  object FileNameTemplateHelpButton: TButton
    Left = 480
    Top = 44
    Width = 33
    Height = 25
    Caption = '?'
    TabOrder = 14
    OnClick = FileNameTemplateHelpButtonClick
  end
  object GrayscaleCheckBox: TCheckBox
    Left = 476
    Top = 106
    Width = 97
    Height = 17
    Caption = 'Grayscale'
    TabOrder = 15
    OnClick = GrayscaleCheckBoxClick
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 16
    Top = 124
  end
  object TrayIcon: TTrayIcon
    PopupMenu = TrayIconPopupMenu
    IconVisible = False
    FormVisible = False
    AppVisible = False
    Left = 96
    Top = 124
  end
  object XPManifest: TXPManifest
    Left = 56
    Top = 124
  end
  object TrayIconPopupMenu: TPopupMenu
    Left = 136
    Top = 120
    object RestoreWindowTrayMenuItem: TMenuItem
      Caption = 'Restore'
      Default = True
      OnClick = RestoreWindowTrayMenuItemClick
    end
    object Separator1TrayMenuItem: TMenuItem
      Caption = '-'
    end
    object ToggleAutoCaptureTrayMenuItem: TMenuItem
      Caption = 'Enable automatic capture'
      OnClick = ToggleAutoCaptureTrayMenuItemClick
    end
    object TakeScreenshotTrayMenuItem: TMenuItem
      Caption = 'Take screenshot'
      OnClick = TakeScreenshotTrayMenuItemClick
    end
    object Separator2TrayMenuItem: TMenuItem
      Caption = '-'
    end
    object ExitTrayMenuItem: TMenuItem
      Caption = 'Quit'
      OnClick = ExitTrayMenuItemClick
    end
  end
end
