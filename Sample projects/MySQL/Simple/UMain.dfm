object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 493
  ClientWidth = 500
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
    Left = 151
    Top = 42
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
    OnClick = btnDowngradeDatabaseClick
  end
  object btnUpgradeDatabase: TButton
    Left = 151
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Upgrade Database'
    TabOrder = 1
    OnClick = btnUpgradeDatabaseClick
  end
  object btnAddMigrations: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Add Migrations'
    TabOrder = 2
    OnClick = btnAddMigrationsClick
  end
  object edtVersion: TEdit
    Left = 209
    Top = 39
    Width = 82
    Height = 23
    NumbersOnly = True
    TabOrder = 3
    Text = '202301010001'
  end
  object mmoLog: TMemo
    Left = 16
    Top = 74
    Width = 465
    Height = 403
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object pbTotal: TProgressBar
    Left = 312
    Top = 8
    Width = 169
    Height = 17
    TabOrder = 5
  end
end
