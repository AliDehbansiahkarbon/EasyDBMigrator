unit Users;

interface
uses
  System.SysUtils,

  EasyDB.Migration.Attrib,
  EasyDB.ConnectionManager.SQL,
  EasyDB.Logger,
  EasyDB.Attribute;

type

  [CustomMigrationAttribute('TbUsers', 202301010003, 'Add users table', 'Alex')]
  TUsersMgr_202301010001 = class(TMigrationEx)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

  function Logger: TLogger;

implementation

{ TUsersMgr_202301010001 }

function Logger: TLogger;
begin
  Result := TLogger.Instance;
end;

procedure TUsersMgr_202301010001.Downgrade;
begin
  try
    TSQLConnection.Instance.ExecuteAdHocQuery('');
  except on E: Exception do
    Logger.Log(atUpgrade, E.Message, AttribEntityName, AttribVersion);
  end;

  inherited;
end;

procedure TUsersMgr_202301010001.Upgrade;
begin

  inherited;
end;

end.
