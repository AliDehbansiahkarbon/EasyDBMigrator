object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Large Script Execution'
  ClientHeight = 594
  ClientWidth = 940
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  DesignSize = (
    940
    594)
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
    Top = 46
    Width = 928
    Height = 17
    TabOrder = 1
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 63
    Width = 908
    Height = 521
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
    ExplicitWidth = 904
  end
  object chkLogExecutions: TCheckBox
    Left = 234
    Top = 13
    Width = 97
    Height = 17
    Caption = 'Log Executions'
    TabOrder = 3
  end
  object btnClear: TButton
    Left = 151
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 4
    OnClick = btnClearClick
  end
end
