unit Users;

interface
uses
  System.SysUtils, EasyDB.Migration.Base, EasyDB.Attribute, EasyDB.ConnectionManager.SQL;

type

  [CustomMigrationAttribute('TbUsers', 202301010001, 'Add users table')]
  TUsersMgr = class(TMigration)
  public
    procedure Upgrade; override;
    procedure Downgrade; override;
  end;

implementation

{ TUsersMgr }

procedure TUsersMgr.Downgrade;
begin
  try
    TSQLConnection.Instance.ExecuteAdHocQuery('');
  except on E: Exception do
    //Log(E.Message);
  end;

  inherited;
end;

procedure TUsersMgr.Upgrade;
begin

  inherited;
end;

end.
