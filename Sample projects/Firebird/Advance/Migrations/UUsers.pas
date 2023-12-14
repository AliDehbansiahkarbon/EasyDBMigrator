{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
unit UUsers;

interface
uses
  System.SysUtils,

  EasyDB.Core,
  EasyDB.MigrationX,
  EasyDB.Attribute,
  UHelper;

type

  [TCustomMigrationAttribute('TbUsers', 202301010001, 'Created users table', 'Alex')]
  TUsersMgr_202301010001 = class(TMigrationX)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

  [TCustomMigrationAttribute('TbUsers', 202301010002, 'Added newfielad1', 'Alex')]
  TUsersMgr_202301010002 = class(TMigrationX)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

  [TCustomMigrationAttribute('TbUsers', 202301010003, 'Added newfielad2', 'Alex')]
  TUsersMgr_202301010003 = class(TMigrationX)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

implementation

{ TUsersMgr_202301010001 }
procedure TUsersMgr_202301010001.Downgrade;
begin
  try
    Firebird.ExecuteAdHocQuery('DROP TABLE TbUsers');
  except on E: Exception do
    Logger.Log(atDowngrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TUsersMgr_202301010001.Upgrade;
var
  LvScript: string;
begin
  if not Firebird.DoesTbExist('TbUsers') then
  begin
    LvScript := 'CREATE TABLE TbUsers ( ' + #10
              + ' ID INT NOT NULL PRIMARY KEY, ' + #10
              + ' UserName VARCHAR(100), ' + #10
              + ' Pass VARCHAR(100) ' + #10
              + ' );';
    try
      Firebird.ExecuteAdHocQuery(LvScript);
    except on E: Exception do
      Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
    end;
  end;
end;

{ TUsersMgr_202301010002 }
procedure TUsersMgr_202301010002.Downgrade;
begin
  try
    Firebird.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP CreatedDate');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TUsersMgr_202301010002.Upgrade;
begin
  try
    Firebird.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD CreatedDate TIMESTAMP');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

{ TUsersMgr_202301010003 }

procedure TUsersMgr_202301010003.Downgrade;
begin
  try
    Firebird.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP ImageLink');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TUsersMgr_202301010003.Upgrade;
begin
  try
    Firebird.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD ImageLink Varchar(500)');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

end.
