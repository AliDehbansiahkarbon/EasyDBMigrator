{$M+}
unit EasyDB.Runner;

interface
uses
  System.Rtti, System.Generics.Collections, EasyDB.Migration.Base, Vcl.Dialogs,
  EasyDB.Attribute, System.SysUtils, TypInfo, Contnrs, System.Classes, EasyDB.ConnectionManager.SQL;

type
  TObjListHelper = class helper for TObjectList<TMigration>
  public
    function Find(AMigrationObj: TMigration): Boolean;
  end;

  IRunner = interface
    ['{DECF074C-109F-488F-A97D-4B3C68FB4F35}']
  end;

  TRunner = class(TInterfacedObject, IRunner)
  private
    FInternalMigrationList: TObjectDictionary<string, TObjectList<TMigration>>;
    FMigrationList: TObjectList<TMigration>;
    FSQLConnection: TSQLConnection;
  public
    constructor Create(ASQLConnection: TSQLConnection = nil);
    destructor Destroy; override;
    function GetDatabaseVersion: Integer;
    procedure UpgradeDatabase;
    procedure DownGrade(AVersion: Integer);
    procedure ArrangeMigrationList;

    property MigrationList: TObjectList<TMigration> read FMigrationList write FMigrationList;
  end;

implementation

{ TRunner }

procedure TRunner.ArrangeMigrationList;
var
  LvExternalMigration: TMigration;
  LvMigrationInternalList: TObjectList<TMigration>;
  LvNewInternalList: TObjectList<TMigration>;
begin
  for LvExternalMigration in FMigrationList do
  begin
    if FInternalMigrationList.ContainsKey(LvExternalMigration.EntityName) then
    begin
      LvMigrationInternalList := FInternalMigrationList.Items[LvExternalMigration.EntityName];

      if not LvMigrationInternalList.Find(LvExternalMigration) then
        LvMigrationInternalList.Add(LvExternalMigration);
    end
    else
    begin
      LvNewInternalList := TObjectList<TMigration>.Create;
      LvNewInternalList.Add(LvExternalMigration);
      FInternalMigrationList.Add(LvExternalMigration.EntityName, LvNewInternalList);
    end;
  end;
end;

constructor TRunner.Create(ASQLConnection: TSQLConnection = nil);
begin
  FMigrationList := TObjectList<TMigration>.Create;
  FInternalMigrationList := TObjectDictionary<string, TObjectList<TMigration>>.Create;
  if Assigned(ASQLConnection) then
    FSQLConnection:= ASQLConnection
  else
    FSQLConnection:= TSQLConnection.Instance.SetConnectionParam(TSQLConnection.Instance.ConnectionParams).ConnectEx;
end;

destructor TRunner.Destroy;
begin
  FMigrationList.Free;
  FInternalMigrationList.Free;
  inherited;
end;

procedure TRunner.UpgradeDatabase;
var
  LvDbVer: Int64;
  LvMigrationList: TObjectList<TMigration>;
  LvInternalMigration: TMigration;
  LvBiggestVer: Int64;
begin
  if FMigrationList.Count = 0 then
    Exit;

  LvDbVer := GetDatabaseVersion;
  LvBiggestVer := LvDbVer;

  ArrangeMigrationList;

  for LvMigrationList in FInternalMigrationList.Values do
  begin
    for LvInternalMigration in LvMigrationList do
    begin
      if LvInternalMigration.Version > LvDbVer then
      begin
        if LvInternalMigration.Version > LvBiggestVer then
          LvBiggestVer := LvInternalMigration.Version;

        LvInternalMigration.Upgrade;
      end;
    end;
  end;

// Create a Generic list for each Migration type
// find the database latest version
// Run downgrade from the bigest version to the given version.
// Update version table
end;

procedure TRunner.DownGrade(AVersion: Integer);
begin
// Create a generic list for each Migration Type
// Find the version siquence and compare with the latest version in Database
// Run the Latest Upgrades from smaller version to the bigest version
// Update database table
end;

function TRunner.GetDatabaseVersion: Integer;
var
  LvSQL: TSQLConnection;
begin
  if LvSQL.IsConnected then
    Result := LvSQL.OpenAsInteger('Select max(Version) from EasyDbVersion');
end;

{TObjListHelper}

function TObjListHelper.Find(AMigrationObj: TMigration): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Pred(Count) do
  begin
    if AMigrationObj.Version = Items[I].Version then
    begin
      Result := True;
      Break;
    end;
  end;
end;



end.
