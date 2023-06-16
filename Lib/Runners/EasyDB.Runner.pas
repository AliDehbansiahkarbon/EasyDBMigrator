unit EasyDB.Runner;

interface
uses
  System.Rtti, System.Generics.Collections, EasyDB.Migration.Base, Vcl.Dialogs,
  EasyDB.Attribute, System.SysUtils, TypInfo, Contnrs, System.Classes,
  EasyDB.ConnectionManager.SQL, EasyDB.Logger;

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
  protected
    FInternalMigrationList: TObjectDictionary<string, TObjectList<TMigration>>;
    FMigrationList: TObjectList<TMigration>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure UpgradeDatabase;
    procedure DowngradeDatabase(AVersion: Int64);
    procedure ArrangeMigrationList;

    procedure UpdateVersionInfo(ALatestVersion: Int64; AAuthor: string; ADescription: string); virtual; abstract;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); virtual; abstract;
    function GetDatabaseVersion: Int64; virtual; abstract;

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
  try
    for LvExternalMigration in FMigrationList do
    begin
      if FInternalMigrationList.ContainsKey(LvExternalMigration.EntityName) then
      begin
        LvMigrationInternalList := FInternalMigrationList.Items[LvExternalMigration.EntityName];

        if not LvMigrationInternalList.FindMigration(LvExternalMigration) then
          LvMigrationInternalList.Add(LvExternalMigration);
      end
      else
      begin
        LvNewInternalList := TObjectList<TMigration>.Create;
        LvNewInternalList.Add(LvExternalMigration);
        FInternalMigrationList.Add(LvExternalMigration.EntityName, LvNewInternalList);
      end;
    end;
  except on E: Exception do
    TLogger.Instance.Log(atPreparingMigrations, E.Message);
  end;
end;

constructor TRunner.Create;
begin
  FMigrationList := TObjectList<TMigration>.Create;
  FInternalMigrationList := TObjectDictionary<string, TObjectList<TMigration>>.Create;
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
          TLogger.Instance.Log(atUpgrade, E.Message, LvInternalMigration.EntityName, LvInternalMigration.Version);
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
  ArrangeMigrationList;

  for LvMigrationList in FInternalMigrationList.Values do
  begin
    for LvInternalMigration in LvMigrationList do
    begin
      try
        if LvInternalMigration.Version > AVersion then
          LvInternalMigration.Downgrade;
      except on E: Exception do
        TLogger.Instance.Log(atUpgrade, E.Message, LvInternalMigration.EntityName, LvInternalMigration.Version);
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
