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
    PG.ExecuteAdHocQuery('DROP TABLE IF EXISTS public.tbusers;');
  except on E: Exception do
    Logger.Log(atDowngrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TUsersMgr_202301010001.Upgrade;
var
  LvScript: string;
begin
  LvScript := 'CREATE TABLE IF NOT EXISTS public.tbusers' + #10
              + '(' + #10
              + 'id SERIAL PRIMARY KEY,' + #10
              + 'username VARCHAR(100),' + #10
              + 'pass VARCHAR(50)' + #10
              + ');';

  try
    PG.ExecuteAdHocQuery(LvScript);
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

{ TUsersMgr_202301010002 }
procedure TUsersMgr_202301010002.Downgrade;
begin
  try
    PG.ExecuteAdHocQuery('ALTER TABLE public.tbusers DROP COLUMN IF EXISTS CreatedDate;');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TUsersMgr_202301010002.Upgrade;
begin
  try
    PG.ExecuteAdHocQuery('ALTER TABLE public.tbusers ADD COLUMN CreatedDate TIMESTAMP;');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

{ TUsersMgr_202301010003 }

procedure TUsersMgr_202301010003.Downgrade;
begin
  try
    PG.ExecuteAdHocQuery('ALTER TABLE public.tbusers DROP COLUMN IF EXISTS ImageLink;');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

procedure TUsersMgr_202301010003.Upgrade;
begin
  try
    PG.ExecuteAdHocQuery('ALTER TABLE public.tbusers ADD COLUMN ImageLink VARCHAR(500);');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;
end;

end.
