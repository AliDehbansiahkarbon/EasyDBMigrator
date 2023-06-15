object Form1: TForm1
  Left = 613
  Top = 332
  Caption = 'Form1'
  ClientHeight = 135
  ClientWidth = 211
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object btnDowngradeDatabase: TButton
    Left = 32
    Top = 87
    Width = 137
    Height = 25
    Caption = 'Downgrade Database'
    TabOrder = 0
    OnClick = btnDowngradeDatabaseClick
  end
  object btnUpgradeDatabase: TButton
    Left = 32
    Top = 56
    Width = 137
    Height = 25
    Caption = 'Upgrade Database'
    TabOrder = 1
    OnClick = btnUpgradeDatabaseClick
  end
  object btnAddMigrations: TButton
    Left = 32
    Top = 25
    Width = 137
    Height = 25
    Caption = 'Add Migrations'
    TabOrder = 2
    OnClick = btnAddMigrationsClick
  end
end
