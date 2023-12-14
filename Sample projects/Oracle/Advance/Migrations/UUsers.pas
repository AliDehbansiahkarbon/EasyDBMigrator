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
    Oracle.ExecuteAdHocQuery('Drop Table TbUsers;');
  except on E: Exception do
    Logger.Log(atDowngrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TUsersMgr_202301010001.Upgrade;
var
  LvScript: string;
begin
  LvScript := 'CREATE TABLE tbusers' + #10
              + '(' + #10
              + 'id NUMBER(10) NOT NULL,' + #10
              + 'username VARCHAR2(100 CHAR),' + #10
              + 'pass VARCHAR2(50 CHAR),' + #10
              + 'CONSTRAINT tbusers_pkey PRIMARY KEY (id)' + #10
              + ');';

  try
    Oracle.ExecuteAdHocQuery(LvScript);
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

{ TUsersMgr_202301010002 }
procedure TUsersMgr_202301010002.Downgrade;
begin
  try
    Oracle.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP COLUMN CreatedDate;');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TUsersMgr_202301010002.Upgrade;
begin
  try
    Oracle.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD CreatedDate DATE;');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

{ TUsersMgr_202301010003 }

procedure TUsersMgr_202301010003.Downgrade;
begin
  try
    Oracle.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP COLUMN ImageLink;');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TUsersMgr_202301010003.Upgrade;
begin
  try
    Oracle.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD ImageLink VARCHAR2(500 CHAR);');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

end.
