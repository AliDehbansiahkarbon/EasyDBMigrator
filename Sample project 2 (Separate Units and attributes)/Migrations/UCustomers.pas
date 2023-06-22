unit UCustomers;

interface
uses
  System.SysUtils,

  EasyDB.Migration.Attrib,
  EasyDB.ConnectionManager.SQL,
  EasyDB.Logger,
  EasyDB.Attribute,
  UHelper;

type

  [TCustomMigrationAttribute('TbCustomers', 202301010005, 'Created Customers table', 'Alex')]
  TCustomersMgr_202301010005 = class(TMigrationEx)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

  [TCustomMigrationAttribute('TbCustomers', 202301010010, 'Altered TbCustomers , added NewField1 as varchar(100)', 'Alex')]
  TCustomersMgr_202301010010 = class(TMigrationEx)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

implementation

{ TCustomersMgr_202301010005 }

procedure TCustomersMgr_202301010005.Downgrade;
var
  LvScript: string;
begin
  try
    SQL.ExecuteAdHocQuery('Drop Table TbCustomers');
  except on E: Exception do
    Logger.Log(atDowngrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TCustomersMgr_202301010005.Upgrade;
var
  LvScript: string;
begin
  LvScript := 'If Not Exists( Select * From sysobjects Where Name = ''TbCustomers'' And xtype = ''U'') ' + #10
       + '    Create Table TbCustomers( ' + #10
       + '    	ID Int Primary key Identity(1, 1) Not null, ' + #10
       + '    	Name Nvarchar(100), ' + #10
       + '    	Family Nvarchar(50) ' + #10
       + '    );';

  try
    SQL.ExecuteAdHocQuery(LvScript);
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

{ TCustomersMgr_202301010010 }

procedure TCustomersMgr_202301010010.Downgrade;
var
  LvScript: string;
begin
  try
    SQL.ExecuteAdHocQuery('Alter table TbCustomers Drop Column Phone');
  except on E: Exception do
    Logger.Log(atDownGrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TCustomersMgr_202301010010.Upgrade;
var
  LvScript: string;
begin
  try
    SQL.ExecuteAdHocQuery('Alter table TbCustomers Add Phone Varchar(10)');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

end.
