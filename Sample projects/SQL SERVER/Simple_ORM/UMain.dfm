object Form1: TForm1
  Left = 585
  Top = 310
  BorderStyle = bsToolWindow
  Caption = 'Form1'
  ClientHeight = 474
  ClientWidth = 483
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  OnDestroy = FormDestroy
  TextHeight = 15
  object Label1: TLabel
    Left = 294
    Top = 42
    Width = 58
    Height = 15
    Caption = 'to version: '
  end
  object btnDowngradeDatabase: TButton
    Left = 151
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
    Top = 39
    Width = 137
    Height = 25
    Caption = 'Add Migrations'
    TabOrder = 2
    OnClick = btnAddMigrationsClick
  end
  object edtVersion: TEdit
    Left = 352
    Top = 39
    Width = 82
    Height = 23
    NumbersOnly = True
    TabOrder = 3
    Text = '202301010001'
  end
  object mmoLog: TMemo
    Left = 16
    Top = 70
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
  object btnCreateDB: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Create Database'
    TabOrder = 6
    OnClick = btnCreateDBClick
  end
end
