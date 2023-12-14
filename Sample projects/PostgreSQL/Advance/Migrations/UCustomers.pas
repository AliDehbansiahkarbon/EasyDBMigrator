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
    PG.ExecuteAdHocQuery('DROP TABLE IF EXISTS public.TbCustomers;');
  except on E: Exception do
    Logger.Log(atDowngrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TCustomersMgr_202301010005.Upgrade;
var
  LvScript: string;
begin
  LvScript := 'CREATE TABLE IF NOT EXISTS public.TbCustomers' + #10
              + '(' + #10
              + 'ID INT NOT NULL PRIMARY KEY,' + #10
              + 'Name VARCHAR(100),' + #10
              + 'Family VARCHAR(100)' + #10
              + ');';

  try
    PG.ExecuteAdHocQuery(LvScript);
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

{ TCustomersMgr_202301010010 }

procedure TCustomersMgr_202301010010.Downgrade;
begin
  try
    PG.ExecuteAdHocQuery('ALTER TABLE public.TbCustomers DROP COLUMN IF EXISTS Phone;');
  except on E: Exception do
    Logger.Log(atDownGrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TCustomersMgr_202301010010.Upgrade;
begin
  try
    PG.ExecuteAdHocQuery('ALTER TABLE public.TbCustomers ADD COLUMN Phone VARCHAR(10);');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

end.
