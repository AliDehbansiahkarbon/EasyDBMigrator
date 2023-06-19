unit EasyDB.Runner;

interface
uses
  System.Rtti, System.Generics.Collections, Vcl.Dialogs,
  System.SysUtils, TypInfo, Contnrs, System.Classes,
  EasyDB.Logger,
  EasyDB.Migration.Base,
  EasyDB.Attribute,
  EasyDB.ConnectionManager.SQL;

type
  TObjListHelper = class helper for TObjectList<TMigration>
  public
    function FindMigration(AMigrationObj: TMigration): Boolean;
  end;

  IRunner = interface
    ['{DECF074C-109F-488F-A97D-4B3C68FB4F35}']

    procedure UpdateVersionInfo(ALatestVersion: Int64; AAuthor: string; ADescription: string);
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64);
    function GetDatabaseVersion: Int64;
  end;

  TRunner = class(TInterfacedObject, IRunner)
  private
    FInternalMigrationList: TDictionary<string, TObjectList<TMigration>>;
    FMigrationList: TList<TMigration>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpgradeDatabase;
    procedure DowngradeDatabase(AVersion: Int64);
    procedure ArrangeMigrationList;
    function Logger: TLogger;

    procedure UpdateVersionInfo(ALatestVersion: Int64; AAuthor: string; ADescription: string); virtual; abstract;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); virtual; abstract;
    function GetDatabaseVersion: Int64; virtual; abstract;

    property MigrationList: TList<TMigration> read FMigrationList write FMigrationList;
  end;

implementation

{ TRunner }

constructor TRunner.Create;
begin
  FMigrationList := TList<TMigration>.Create;
  FInternalMigrationList := TDictionary<string, TObjectList<TMigration>>.Create;
end;

destructor TRunner.Destroy;
var
  LvKey: string;
  I: Integer;
begin
  if FInternalMigrationList.Count > 0 then
  begin
    for LvKey in FInternalMigrationList.Keys do
      FInternalMigrationList.Items[LvKey].Free;

    FreeAndNil(FInternalMigrationList);
    FreeAndNil(FMigrationList);
  end
  else
  begin
    FreeAndNil(FInternalMigrationList);

    for I := 0 to Pred(FMigrationList.Count) do
      FMigrationList[I].Free;

    FreeAndNil(FMigrationList);
  end;

  FreeAndNil(TLogger.Instance);
  inherited;
end;

procedure TRunner.ArrangeMigrationList;
var
  LvExternalMigration: TMigration;
  LvNewInternalList: TObjectList<TMigration>;
begin
  try
    for LvExternalMigration in FMigrationList do
    begin
      if FInternalMigrationList.ContainsKey(LvExternalMigration.EntityName) then
      begin
        if not FInternalMigrationList.Items[LvExternalMigration.EntityName].FindMigration(LvExternalMigration) then
          FInternalMigrationList.Items[LvExternalMigration.EntityName].Add(LvExternalMigration);
      end
      else
      begin
        LvNewInternalList := TObjectList<TMigration>.Create;
        LvNewInternalList.Add(LvExternalMigration);
        FInternalMigrationList.Add(LvExternalMigration.EntityName, LvNewInternalList);
      end;
    end;
  except on E: Exception do
    Logger.Log(atPreparingMigrations, E.Message);
  end;
end;

function TRunner.Logger: TLogger;
begin
  Result := TLogger.Instance;
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
          if LvInternalMigration.Version > LvBiggestVer then
            LvBiggestVer := LvInternalMigration.Version;

          LvInternalMigration.Upgrade;
        except on E: Exception do
          Logger.Log(atUpgrade, E.Message, LvInternalMigration.EntityName, LvInternalMigration.Version);
        end;
      end;

      if LvBiggestVer > LvDbVer then
        UpdateVersionInfo(LvBiggestVer, LvInternalMigration.Author, LvInternalMigration.Description);
    end;
  end;
end;

procedure TRunner.DowngradeDatabase(AVersion: Int64);
var
  LvDbVer: Int64;
  LvMigrationList: TObjectList<TMigration>;
  LvInternalMigration: TMigration;
begin
  if FMigrationList.Count = 0 then
    Exit;

  LvDbVer := GetDatabaseVersion;
  if LvDbVer <= AVersion then
    Exit;

  ArrangeMigrationList;

  for LvMigrationList in FInternalMigrationList.Values do
  begin
    for LvInternalMigration in LvMigrationList do
    begin
      try
        if LvInternalMigration.Version > AVersion then
          LvInternalMigration.Downgrade;

      except on E: Exception do
        Logger.Log(atUpgrade, E.Message, LvInternalMigration.EntityName, LvInternalMigration.Version);
      end;
    end;
  end;

  DownGradeVersionInfo(AVersion);
end;

{TObjListHelper}

function TObjListHelper.FindMigration(AMigrationObj: TMigration): Boolean;
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
