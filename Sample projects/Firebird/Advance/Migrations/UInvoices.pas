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
    Firebird.ExecuteAdHocQuery('DROP TABLE TbInvoices');
  except on E: Exception do
    Logger.Log(atDowngrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TInvoicesMgr_202301010005.Upgrade;
var
  LvScript: string;
begin
  if not Firebird.DoesTbExist('TbInvoices') then
  begin
    LvScript := 'CREATE TABLE TbInvoices (  ' + #10
         + ' ID INT NOT NULL PRIMARY KEY, ' + #10
         + ' InvoiceID Int, ' + #10
         + ' CustomerID Int, ' + #10
         + ' InvoiceDate TIMESTAMP ' + #10
         + ' );';

    try
      Firebird.ExecuteAdHocQuery(LvScript);
    except on E: Exception do
      Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
    end;
  end;
end;

{ TInvoicesMgr_202301010010 }

procedure TInvoicesMgr_202301010010.Downgrade;
begin
  try
    Firebird.ExecuteAdHocQuery('ALTER TABLE TbInvoices DROP TotlaAmount');
  except on E: Exception do
    Logger.Log(atDownGrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TInvoicesMgr_202301010010.Upgrade;
begin
  try
    Firebird.ExecuteAdHocQuery('ALTER TABLE TbInvoices ADD TotlaAmount Decimal(10, 2)');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

end.

