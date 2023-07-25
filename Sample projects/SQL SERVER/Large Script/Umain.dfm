object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Large Script Execution'
  ClientHeight = 608
  ClientWidth = 925
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    925
    608)
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
    Top = 57
    Width = 901
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
    ExplicitTop = 45
    ExplicitWidth = 893
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 74
    Width = 901
    Height = 529
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
    ExplicitTop = 62
    ExplicitWidth = 893
  end
  object btnClear: TButton
    Left = 151
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 3
    OnClick = btnClearClick
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
