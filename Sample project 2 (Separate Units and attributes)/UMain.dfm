object Form2: TForm2
  Left = 700
  Top = 414
  BorderStyle = bsToolWindow
  Caption = 'Advance usage with attributes.'
  ClientHeight = 306
  ClientWidth = 456
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
  object Label1: TLabel
    Left = 216
    Top = 67
    Width = 58
    Height = 15
    Caption = 'to version: '
  end
  object btnDowngradeDatabase: TButton
    Left = 64
    Top = 63
    Width = 137
    Height = 25
    Caption = 'Downgrade Database'
    TabOrder = 0
    OnClick = btnDowngradeDatabaseClick
  end
  object btnUpgradeDatabase: TButton
    Left = 224
    Top = 25
    Width = 137
    Height = 25
    Caption = 'Upgrade Database'
    TabOrder = 1
    OnClick = btnUpgradeDatabaseClick
  end
  object btnAddMigrations: TButton
    Left = 64
    Top = 25
    Width = 137
    Height = 25
    Caption = 'Add Migrations'
    TabOrder = 2
    OnClick = btnAddMigrationsClick
  end
  object edtVersion: TEdit
    Left = 274
    Top = 64
    Width = 82
    Height = 23
    NumbersOnly = True
    TabOrder = 3
    Text = '202301010001'
  end
  object mmoLog: TMemo
    Left = 8
    Top = 104
    Width = 430
    Height = 187
    ScrollBars = ssVertical
    TabOrder = 4
  end
end
