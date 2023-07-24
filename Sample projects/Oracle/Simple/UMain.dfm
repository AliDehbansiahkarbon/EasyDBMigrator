object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Oracle Simple'
  ClientHeight = 480
  ClientWidth = 461
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Label1: TLabel
    Left = 155
    Top = 43
    Width = 58
    Height = 15
    Caption = 'to version: '
  end
  object btnDowngradeDatabase: TButton
    Left = 8
    Top = 39
    Width = 137
    Height = 25
    Caption = 'Downgrade Database'
    TabOrder = 0
  end
  object btnUpgradeDatabase: TButton
    Left = 151
    Top = 8
    Width = 140
    Height = 25
    Caption = 'Upgrade Database'
    TabOrder = 1
  end
  object btnAddMigrations: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Add Migrations'
    TabOrder = 2
  end
  object edtVersion: TEdit
    Left = 211
    Top = 39
    Width = 80
    Height = 23
    NumbersOnly = True
    TabOrder = 3
    Text = '202301010001'
  end
  object mmoLog: TMemo
    Left = 8
    Top = 80
    Width = 445
    Height = 360
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object pbTotal: TProgressBar
    Left = 303
    Top = 8
    Width = 150
    Height = 17
    TabOrder = 5
  end
end
