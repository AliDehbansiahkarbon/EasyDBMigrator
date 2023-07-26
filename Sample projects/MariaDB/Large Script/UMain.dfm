object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 682
  ClientWidth = 958
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    958
    682)
  TextHeight = 15
  object btnUpgradeDatabase: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Upgrade Database'
    TabOrder = 0
    OnClick = btnUpgradeDatabaseClick
  end
  object pbTotal: TProgressBar
    Left = 8
    Top = 48
    Width = 937
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
    ExplicitTop = 47
    ExplicitWidth = 933
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 65
    Width = 937
    Height = 612
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    HideScrollBars = False
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    ExplicitTop = 64
    ExplicitWidth = 933
  end
  object btnClear: TButton
    Left = 151
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 3
  end
  object rb_LogAllExecutions: TRadioButton
    Left = 241
    Top = 12
    Width = 133
    Height = 17
    Caption = 'Log each execution'
    Checked = True
    TabOrder = 4
    TabStop = True
  end
  object RadioButton1: TRadioButton
    Left = 382
    Top = 12
    Width = 107
    Height = 17
    Caption = 'Just Log errors'
    TabOrder = 5
  end
end
