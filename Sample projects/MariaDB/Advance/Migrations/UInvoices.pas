{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
unit UInvoices;

interface
uses
  System.SysUtils,

  EasyDB.Core,
  EasyDB.MigrationX,
  EasyDB.Attribute,
  UHelper;

type

  [TCustomMigrationAttribute('TbInvoices', 202301010005, 'Created TbInvoices table', 'Alex')]
  TInvoicesMgr_202301010005 = class(TMigrationX)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

  [TCustomMigrationAttribute('TbInvoices', 202301010010, 'Altered TbInvoices table, added TotlaAmount as Decimal(10, 2)', 'Alex')]
  TInvoicesMgr_202301010010 = class(TMigrationX)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

implementation

{ TInvoicesMgr_202301010005 }

procedure TInvoicesMgr_202301010005.Downgrade;
begin
  try
    MariaDB.ExecuteAdHocQuery('Drop Table TbInvoices');
  except on E: Exception do
    Logger.Log(atDowngrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TInvoicesMgr_202301010005.Upgrade;
var
  LvScript: string;
begin
  LvScript := 'CREATE TABLE IF NOT EXISTS TbInvoices (  ' + #10
       + '    ID INT NOT NULL PRIMARY KEY, ' + #10
       + '    InvoiceID Int, ' + #10
       + '    CustomerID Int, ' + #10
       + '    InvoiceDate Datetime ' + #10
       + '    );';

  try
    MariaDB.ExecuteAdHocQuery(LvScript);
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

{ TInvoicesMgr_202301010010 }

procedure TInvoicesMgr_202301010010.Downgrade;
begin
  try
    MariaDB.ExecuteAdHocQuery('Alter table TbInvoices Drop Column TotlaAmount');
  except on E: Exception do
    Logger.Log(atDownGrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TInvoicesMgr_202301010010.Upgrade;
begin
  try
    MariaDB.ExecuteAdHocQuery('Alter table TbInvoices Add TotlaAmount Decimal(10, 2)');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

end.

