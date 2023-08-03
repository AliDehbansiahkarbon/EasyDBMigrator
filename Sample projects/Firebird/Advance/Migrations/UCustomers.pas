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
    Firebird.ExecuteAdHocQuery('DROP TABLE TbCustomers');
  except on E: Exception do
    Logger.Log(atDowngrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TCustomersMgr_202301010005.Upgrade;
var
  LvScript: string;
begin
  if not Firebird.DoesTbExist('TbCustomers') then
  begin
    LvScript := 'CREATE TABLE TbCustomers ( ' + #10
              + ' ID INT NOT NULL PRIMARY KEY, ' + #10
              + ' Name VARCHAR(100), ' + #10
              + ' Family VARCHAR(100) ' + #10
              + ' );';

    try
      Firebird.ExecuteAdHocQuery(LvScript);
    except on E: Exception do
      Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
    end;
  end;
end;

{ TCustomersMgr_202301010010 }

procedure TCustomersMgr_202301010010.Downgrade;
begin
  try
    Firebird.ExecuteAdHocQuery('ALTER TABLE TbCustomers DROP Phone');
  except on E: Exception do
    Logger.Log(atDownGrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TCustomersMgr_202301010010.Upgrade;
begin
  try
    Firebird.ExecuteAdHocQuery('ALTER TABLE TbCustomers ADD Phone VARCHAR(10)');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

end.
