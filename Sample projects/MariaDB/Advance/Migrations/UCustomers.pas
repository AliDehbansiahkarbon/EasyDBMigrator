{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
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
    MariaDB.ExecuteAdHocQuery('Drop Table TbCustomers');
  except on E: Exception do
    Logger.Log(atDowngrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TCustomersMgr_202301010005.Upgrade;
var
  LvScript: string;
begin
  LvScript := 'CREATE TABLE IF NOT EXISTS TbCustomers ( ' + #10
            + '    ID INT NOT NULL PRIMARY KEY, ' + #10
            + '    Name NVARCHAR(100), ' + #10
            + '    Family NVARCHAR(100) ' + #10
            + '    );';

  try
    MariaDB.ExecuteAdHocQuery(LvScript);
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

{ TCustomersMgr_202301010010 }

procedure TCustomersMgr_202301010010.Downgrade;
begin
  try
    MariaDB.ExecuteAdHocQuery('Alter table TbCustomers Drop Column Phone');
  except on E: Exception do
    Logger.Log(atDownGrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TCustomersMgr_202301010010.Upgrade;
begin
  try
    MariaDB.ExecuteAdHocQuery('Alter table TbCustomers Add Phone Varchar(10)');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

end.
