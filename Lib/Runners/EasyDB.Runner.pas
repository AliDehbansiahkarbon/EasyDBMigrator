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
    constructor Create(ASQLConnection: TSQLConnection = nil); overload;
    constructor Create(ConnectionParams: TConnectionParams); overload;
    destructor Destroy; override;

    procedure UpgradeDatabase;
    procedure Downgrade(AVersion: Int64);
    procedure ArrangeMigrationList;
    procedure UpdateVersionInfo(ALatestVersion: Int64; AAuthor: string; ADescription: string);
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64);
    function GetDatabaseVersion: Int64;

    property MigrationList: TObjectList<TMigration> read FMigrationList write FMigrationList;
    property SQLConnection: TSQLConnection read FSQLConnection write FSQLConnection;
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

constructor TRunner.Create(ConnectionParams: TConnectionParams);
begin
  FMigrationList := TObjectList<TMigration>.Create;
  FInternalMigrationList := TObjectDictionary<string, TObjectList<TMigration>>.Create;
  FSQLConnection:= TSQLConnection.Instance.SetConnectionParam(ConnectionParams).ConnectEx;
end;

destructor TRunner.Destroy;
begin
  FMigrationList.Free;
  FInternalMigrationList.Free;
  FSQLConnection.Free;
  inherited;
end;

procedure TRunner.UpgradeDatabase;
var
  LvDbVer: Int64;
  LvBiggestVer: Int64;
  LvMigrationList: TObjectList<TMigration>;
  LvInternalMigration: TMigration;
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
        try
          LvInternalMigration.Upgrade;
          if LvInternalMigration.Version > LvBiggestVer then
            LvBiggestVer := LvInternalMigration.Version;
        except
          //Log TODO
        end;
      end;

      if LvBiggestVer > LvDbVer then
        UpdateVersionInfo(LvBiggestVer, LvInternalMigration.Author, LvInternalMigration.Description);
    end;
  end;
end;

procedure TRunner.Downgrade(AVersion: Int64);
var
  LvDbVer: Int64;
  LvMigrationList: TObjectList<TMigration>;
  LvInternalMigration: TMigration;
begin
  if FMigrationList.Count = 0 then
    Exit;

  LvDbVer := GetDatabaseVersion;
  ArrangeMigrationList;

  for LvMigrationList in FInternalMigrationList.Values do
  begin
    for LvInternalMigration in LvMigrationList do
    begin
      if LvInternalMigration.Version > AVersion then
        LvInternalMigration.Downgrade;
    end;
  end;

  DownGradeVersionInfo(AVersion);
end;

function TRunner.GetDatabaseVersion: Int64;
begin
  if FSQLConnection.IsConnected then
    Result := FSQLConnection.OpenAsInteger('Select max(Version) from EasyDBVersionInfo');
end;

procedure TRunner.UpdateVersionInfo(ALatestVersion: Int64; AAuthor: string; ADescription: string);
var
  LvScript: string;
begin
  //TODO
//  LvScript := 'INSERT INTO [' + DbName + '].[' + Schema + '].[EasyDBVersionInfo] ' + #10
  LvScript := 'INSERT INTO EasyDBVersionInfo ' + #10
     + '( ' + #10
     + '	Version, ' + #10
     + '	AppliedOn, ' + #10
     + '	Author, ' + #10
     + '	[Description] ' + #10
     + ') ' + #10
     + 'VALUES ' + #10
     + '( ' + #10
     + '	' + ALatestVersion.ToString + ', ' + #10
     + '	(getdate()), ' + #10
     + '	' + AAuthor.QuotedString + ', ' + #10
     + '	' + ADescription.QuotedString + ' ' + #10
     + ')';

  FSQLConnection.ExecuteAdHocQuery(LvScript);
end;

procedure TRunner.DownGradeVersionInfo(AVersionToDownGrade: Int64);
var
  LvScript: string;
begin
  LvScript := 'Delete from EasyDBVersionInfo Where Version > ' + AVersionToDownGrade.ToString;
  FSQLConnection.ExecuteAdHocQuery(LvScript);
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
