object MainForm: TMainForm
  Left = 192
  Top = 124
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Auto Screenshot'
  ClientHeight = 474
  ClientWidth = 656
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    656
    474)
  PixelsPerInch = 96
  TextHeight = 15
  object OutputDirLabel: TTntLabel
    Left = 112
    Top = 24
    Width = 89
    Height = 15
    Alignment = taRightJustify
    Caption = 'Saving directory:'
  end
  object CaptureIntervalLabel: TTntLabel
    Left = 77
    Top = 197
    Width = 124
    Height = 15
    Alignment = taRightJustify
    Caption = 'Saving interval (h:m:s):'
  end
  object ImageFormatLabel: TTntLabel
    Left = 159
    Top = 228
    Width = 42
    Height = 15
    Alignment = taRightJustify
    Caption = 'Format:'
  end
  object JPEGQualityLabel: TTntLabel
    Left = 290
    Top = 229
    Width = 79
    Height = 15
    Alignment = taRightJustify
    Caption = 'Compression:'
  end
  object JPEGQualityPercentLabel: TTntLabel
    Left = 440
    Top = 229
    Width = 11
    Height = 15
    Caption = '%'
  end
  object FileNameTemplateLabel: TTntLabel
    Left = 95
    Top = 50
    Width = 106
    Height = 15
    Alignment = taRightJustify
    Caption = 'Filename template:'
  end
  object ColorDepthLabel: TTntLabel
    Left = 374
    Top = 228
    Width = 67
    Height = 15
    Alignment = taRightJustify
    Caption = 'Color depth:'
  end
  object MonitorLabel: TTntLabel
    Left = 123
    Top = 356
    Width = 78
    Height = 15
    Alignment = taRightJustify
    Caption = 'Used monitor:'
  end
  object OutputDirEdit: TTntEdit
    Left = 208
    Top = 20
    Width = 265
    Height = 23
    TabOrder = 0
    OnChange = OutputDirEditChange
  end
  object ChooseOutputDirButton: TTntButton
    Left = 480
    Top = 16
    Width = 33
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = ChooseOutputDirButtonClick
  end
  object TakeScreenshotButton: TTntButton
    Left = 32
    Top = 422
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Take screenshot'
    TabOrder = 16
    OnClick = TakeScreenshotButtonClick
  end
  object JPEGQualitySpinEdit: TSpinEdit
    Left = 376
    Top = 224
    Width = 57
    Height = 24
    MaxValue = 100
    MinValue = 0
    TabOrder = 9
    Value = 0
    OnChange = JPEGQualitySpinEditChange
  end
  object OpenOutputDirButton: TTntButton
    Left = 520
    Top = 16
    Width = 121
    Height = 25
    Caption = 'Open directory'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = OpenOutputDirButtonClick
  end
  object StopWhenInactiveCheckBox: TTntCheckBox
    Left = 208
    Top = 256
    Width = 329
    Height = 17
    Caption = 'Pause taking screenshots while idle'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 11
    OnClick = StopWhenInactiveCheckBoxClick
  end
  object ImageFormatComboBox: TTntComboBox
    Left = 208
    Top = 224
    Width = 57
    Height = 23
    AutoComplete = False
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 15
    TabOrder = 7
    OnChange = ImageFormatComboBoxChange
  end
  object AutoCaptureControlGroup: TTntGroupBox
    Left = 208
    Top = 394
    Width = 281
    Height = 65
    Anchors = [akLeft, akBottom]
    Caption = 'Automatic capture'
    TabOrder = 17
    object StartAutoCaptureButton: TTntBitBtn
      Left = 24
      Top = 24
      Width = 105
      Height = 25
      Caption = 'Enable'
      TabOrder = 0
      OnClick = StartAutoCaptureButtonClick
      NumGlyphs = 2
      Spacing = 8
    end
    object StopAutoCaptureButton: TTntBitBtn
      Left = 152
      Top = 24
      Width = 105
      Height = 25
      Caption = 'Disable'
      TabOrder = 1
      OnClick = StopAutoCaptureButtonClick
      NumGlyphs = 2
      Spacing = 8
    end
  end
  object StartCaptureOnStartUpCheckBox: TTntCheckBox
    Left = 208
    Top = 280
    Width = 305
    Height = 17
    Caption = 'Start auto capture when program starts'
    TabOrder = 12
    OnClick = StartCaptureOnStartUpCheckBoxClick
  end
  object StartMinimizedCheckBox: TTntCheckBox
    Left = 208
    Top = 304
    Width = 305
    Height = 17
    Caption = 'Start minimized to tray'
    TabOrder = 13
    OnClick = StartMinimizedCheckBoxClick
  end
  object FileNameTemplateComboBox: TTntComboBox
    Left = 208
    Top = 46
    Width = 265
    Height = 23
    ItemHeight = 15
    TabOrder = 3
    OnChange = FileNameTemplateComboBoxChange
    Items.Strings = (
      '%Y-%M-%D\%Y-%M-%D %H.%N.%S'
      '%Y-%M\%D\%Y-%M-%D_%H.%N.%S'
      '%Y\%M\%D\%H_%N_%S'
      'screenshot %Y-%M-%D %H%N%S'
      'screenshot %NUM')
  end
  object FileNameTemplateHelpButton: TTntButton
    Left = 480
    Top = 44
    Width = 33
    Height = 25
    Caption = '?'
    TabOrder = 4
    OnClick = FileNameTemplateHelpButtonClick
  end
  object GrayscaleCheckBox: TTntCheckBox
    Left = 476
    Top = 226
    Width = 141
    Height = 17
    Caption = 'Grayscale'
    TabOrder = 10
    OnClick = GrayscaleCheckBoxClick
  end
  object ColorDepthComboBox: TTntComboBox
    Left = 448
    Top = 224
    Width = 65
    Height = 23
    AutoComplete = False
    Style = csDropDownList
    DropDownCount = 10
    ItemHeight = 15
    TabOrder = 8
    OnChange = ColorDepthComboBoxChange
  end
  object CaptureInterval: TTntDateTimePicker
    Left = 208
    Top = 192
    Width = 73
    Height = 23
    Date = 44009.000000000000000000
    Format = 'H:mm:ss'
    Time = 44009.000000000000000000
    Kind = dtkTime
    TabOrder = 6
    OnChange = CaptureIntervalChange
  end
  object AutoRunCheckBox: TTntCheckBox
    Left = 208
    Top = 328
    Width = 265
    Height = 17
    Caption = 'Run application at system startup'
    TabOrder = 14
    OnClick = AutoRunCheckBoxClick
  end
  object MonitorComboBox: TTntComboBox
    Left = 208
    Top = 352
    Width = 265
    Height = 23
    Style = csDropDownList
    ItemHeight = 15
    TabOrder = 15
    OnChange = MonitorComboBoxChange
    Items.Strings = (
      ''
      '')
  end
  object SeqNumberGroup: TTntGroupBox
    Left = 208
    Top = 80
    Width = 305
    Height = 97
    Caption = 'Sequential number'
    TabOrder = 5
    object SeqNumberValueLabel: TTntLabel
      Left = 96
      Top = 32
      Width = 59
      Height = 15
      Alignment = taRightJustify
      Caption = 'Next value:'
    end
    object SeqNumberDigitsCountLabel: TTntLabel
      Left = 120
      Top = 64
      Width = 35
      Height = 15
      Alignment = taRightJustify
      Caption = 'Digits:'
    end
    object SeqNumberValueSpinEdit: TSpinEdit
      Left = 160
      Top = 27
      Width = 73
      Height = 24
      MaxValue = 999999
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = SeqNumberValueSpinEditChange
    end
    object SeqNumberDigitsCountSpinEdit: TSpinEdit
      Left = 160
      Top = 59
      Width = 49
      Height = 24
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = SeqNumberDigitsCountSpinEditChange
    end
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
  object TrayIconPopupMenu: TTntPopupMenu
    Left = 136
    Top = 240
    object RestoreWindowTrayMenuItem: TTntMenuItem
      Caption = 'Restore'
      Default = True
      OnClick = RestoreWindowTrayMenuItemClick
    end
    object Separator1TrayMenuItem: TTntMenuItem
      Caption = '-'
    end
    object ToggleAutoCaptureTrayMenuItem: TTntMenuItem
      Caption = 'Enable automatic capture'
      OnClick = ToggleAutoCaptureTrayMenuItemClick
    end
    object TakeScreenshotTrayMenuItem: TTntMenuItem
      Caption = 'Take screenshot'
      OnClick = TakeScreenshotTrayMenuItemClick
    end
    object Separator2TrayMenuItem: TTntMenuItem
      Caption = '-'
    end
    object ExitTrayMenuItem: TTntMenuItem
      Caption = 'Quit'
      OnClick = ExitTrayMenuItemClick
    end
  end
  object TrayIconAnimationTimer: TTimer
    Enabled = False
    Interval = 160
    OnTimer = TrayIconAnimationTimerTimer
    Left = 16
    Top = 168
  end
  object MainMenu: TTntMainMenu
    Left = 40
    Top = 40
    object OptionsSubMenu: TTntMenuItem
      Caption = 'Options'
      object LanguageSubMenu: TTntMenuItem
        Caption = 'Language'
      end
    end
    object HelpSubMenu: TTntMenuItem
      Caption = 'Help'
      object AboutMenuItem: TTntMenuItem
        Caption = 'About...'
        OnClick = AboutMenuItemClick
      end
    end
  end
end
