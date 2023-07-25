unit UCustomers;

interface
uses
  System.SysUtils,

  EasyDB.Core,
  EasyDB.MigrationX,
  EasyDB.Attribute,
  UHelper;

type

  [TCustomMigrationAttribute('TbCustomers', 202301010005, 'Created Customers table', 'Alex')]
  TCustomersMgr_202301010005 = class(TMigrationX)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

  [TCustomMigrationAttribute('TbCustomers', 202301010010, 'Altered TbCustomers , added Phone as varchar(10)', 'Alex')]
  TCustomersMgr_202301010010 = class(TMigrationX)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

implementation

{ TCustomersMgr_202301010005 }

procedure TCustomersMgr_202301010005.Downgrade;
begin
  try
    Oracle.ExecuteAdHocQuery('Drop Table TbCustomers;');
  except on E: Exception do
    Logger.Log(atDowngrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TCustomersMgr_202301010005.Upgrade;
var
  LvScript: string;
begin
  LvScript := 'CREATE TABLE TbCustomers' + #10
              + '(' + #10
              + 'ID NUMBER(10) NOT NULL PRIMARY KEY,' + #10
              + 'Name NVARCHAR2(100 CHAR),' + #10
              + 'Family NVARCHAR2(100 CHAR)' + #10
              + ');';

  try
    Oracle.ExecuteAdHocQuery(LvScript);
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

{ TCustomersMgr_202301010010 }

procedure TCustomersMgr_202301010010.Downgrade;
begin
  try
    Oracle.ExecuteAdHocQuery('ALTER TABLE TbCustomers DROP COLUMN Phone;');
  except on E: Exception do
    Logger.Log(atDownGrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TCustomersMgr_202301010010.Upgrade;
begin
  try
    Oracle.ExecuteAdHocQuery('ALTER TABLE TbCustomers ADD Phone VARCHAR2(10 CHAR);');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

end.
