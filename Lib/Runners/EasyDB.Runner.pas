unit EasyDB.Runner;

interface
uses
  System.Rtti, System.Generics.Collections, Vcl.Dialogs,
  System.SysUtils, TypInfo, Contnrs, System.Classes, System.Generics.Defaults,

  EasyDB.Logger,
  EasyDB.Migration.Base,
  EasyDB.Migration.Attrib,
  EasyDB.Attribute,
  EasyDB.ConnectionManager.SQL;

type
  TMigrations = class(TObjectList<TMigrationBase>)
  end;

  TObjListHelper = class helper for TMigrations
  public
    function FindMigration(AMigrationObj: TMigrationBase): Boolean;
  end;

  IRunner = interface
    ['{DECF074C-109F-488F-A97D-4B3C68FB4F35}']

    procedure UpdateVersionInfo(ALatestVersion: Int64; AAuthor: string; ADescription: string; AInsertMode: Boolean = True);
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64);
    function GetDatabaseVersion: Int64;
  end;

  TRunner = class(TInterfacedObject, IRunner)
  private
    FInternalMigrationList: TDictionary<string, TMigrations>;
    FMigrationList: TList<TMigrationBase>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpgradeDatabase;
    procedure DowngradeDatabase(AVersion: Int64);
    procedure ArrangeMigrationList;
    function Logger: TLogger;
    procedure SortDictionaryByField(ADict: TDictionary<string, TMigrations>; AFieldName: string);

    procedure UpdateVersionInfo(ALatestVersion: Int64; AAuthor: string; ADescription: string; AInsertMode: Boolean = True); virtual; abstract;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); virtual; abstract;
    function GetDatabaseVersion: Int64; virtual; abstract;

    property MigrationList: TList<TMigrationBase> read FMigrationList write FMigrationList;
  end;

implementation

{ TRunner }

constructor TRunner.Create;
begin
  FMigrationList := TList<TMigrationBase>.Create;
  FInternalMigrationList := TDictionary<string, TMigrations>.Create;
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
  LvExternalMigration: TMigrationBase;
  LvNewInternalList: TMigrations;

  LvTempMigration: TMigration;
  LvTempMigrationEx: TMigrationEx;
begin
  try
    for LvExternalMigration in FMigrationList do
    begin
      if LvExternalMigration is TMigration then
      begin
        LvTempMigration := TMigration(LvExternalMigration);
        if FInternalMigrationList.ContainsKey(LvTempMigration.EntityName) then
        begin
          if not FInternalMigrationList.Items[LvTempMigration.EntityName].FindMigration(LvTempMigration) then
            FInternalMigrationList.Items[LvTempMigration.EntityName].Add(LvTempMigration);
        end
        else
        begin
          LvNewInternalList := TMigrations.Create;
          LvNewInternalList.Add(LvTempMigration);
          FInternalMigrationList.Add(LvTempMigration.EntityName, LvNewInternalList);
        end;

        SortDictionaryByField(FInternalMigrationList, 'Version');
      end
      else if LvExternalMigration is TMigrationEx then
      begin
        LvTempMigrationEx := TMigrationEx(LvExternalMigration);
        if FInternalMigrationList.ContainsKey(LvTempMigrationEx.AttribEntityName) then
        begin
          if not FInternalMigrationList.Items[LvTempMigrationEx.AttribEntityName].FindMigration(LvTempMigrationEx) then
            FInternalMigrationList.Items[LvTempMigrationEx.AttribEntityName].Add(LvTempMigrationEx);
        end
        else
        begin
          LvNewInternalList := TMigrations.Create;
          LvNewInternalList.Add(LvTempMigrationEx);
          FInternalMigrationList.Add(LvTempMigrationEx.AttribEntityName, LvNewInternalList);
        end;

        SortDictionaryByField(FInternalMigrationList, 'AttribVersion');
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
  LvMigrationList: TMigrations;
  LvInternalMigration: TMigrationBase;
  LvWrittenVersions: TList<Int64>;

  LvTempVersion: Int64;
  LvTempMigration: TMigration;
  LvTempMigrationEx: TMigrationEx;
begin
  if FMigrationList.Count = 0 then
    Exit;

  LvTempVersion := 0;
  LvWrittenVersions := TList<Int64>.Create;
  try
    LvDbVer := GetDatabaseVersion;
    ArrangeMigrationList;

    for LvMigrationList in FInternalMigrationList.Values do
    begin
      for LvInternalMigration in LvMigrationList do
      begin
        if LvInternalMigration is TMigration then
        begin
          LvTempMigration := TMigration(LvInternalMigration);
          LvTempVersion :=  LvTempMigration.Version;

          if LvTempVersion > LvDbVer then
          begin
            try
              LvTempMigration.Upgrade;

              if not LvWrittenVersions.Contains(LvTempVersion) then
              begin
                UpdateVersionInfo(LvTempVersion, LvTempMigration.Author, LvTempMigration.Description);
                LvWrittenVersions.Add(LvTempVersion);
              end
              else
                UpdateVersionInfo(LvTempVersion, LvTempMigration.Author, LvTempMigration.Description, False);
            except on E: Exception do
              Logger.Log(atUpgrade, E.Message, LvTempMigration.EntityName, LvTempVersion);
            end;
          end;
        end
        else if LvInternalMigration is TMigrationEx then
        begin
          LvTempMigrationEx := TMigrationEx(LvInternalMigration);
          LvTempVersion := LvTempMigrationEx.AttribVersion;

          if LvTempVersion > LvDbVer then
          begin
            try
              LvTempMigrationEx.Upgrade;

              if not LvWrittenVersions.Contains(LvTempVersion) then
              begin
                UpdateVersionInfo(LvTempVersion, LvTempMigrationEx.AttribAuthor, LvTempMigrationEx.AttribDescription);
                LvWrittenVersions.Add(LvTempVersion);
              end
              else
                UpdateVersionInfo(LvTempVersion, LvTempMigrationEx.AttribAuthor, LvTempMigrationEx.AttribDescription, False);
            except on E: Exception do
              Logger.Log(atUpgrade, E.Message, LvTempMigrationEx.AttribEntityName, LvTempVersion);
            end;
          end;
        end;
      end;
    end;
  finally
    LvWrittenVersions.Free;
  end;
end;

procedure TRunner.DowngradeDatabase(AVersion: Int64);
var
  LvDbVer: Int64;
  LvMigrationList: TMigrations;
  LvInternalMigration: TMigrationBase;

  LvTempMigration: TMigration;
  LvTempMigrationEx: TMigrationEx;
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
      if LvInternalMigration is TMigration then
      begin
        LvTempMigration := TMigration(LvInternalMigration);
        try
          if LvTempMigration.Version > AVersion then
            LvTempMigration.Downgrade;
        except on E: Exception do
          Logger.Log(atUpgrade, E.Message, LvTempMigration.EntityName, LvTempMigration.Version);
        end;
      end
      else if LvInternalMigration is TMigrationEx then
      begin
        LvTempMigrationEx := TMigrationEx(LvInternalMigration);
        try
          if LvTempMigrationEx.AttribVersion > AVersion then
            LvTempMigrationEx.Downgrade;
        except on E: Exception do
          Logger.Log(atUpgrade, E.Message, LvTempMigrationEx.AttribEntityName, LvTempMigrationEx.AttribVersion);
        end;
      end;
    end;
  end;

  DownGradeVersionInfo(AVersion);
end;

procedure TRunner.SortDictionaryByField(ADict: TDictionary<string, TMigrations>; AFieldName: string);
var
  LvValue: TMigrations;
begin
  for LvValue in ADict.Values do
  begin
    LvValue.Sort(TComparer<TMigrationBase>.Construct(
      function (const L,R: TMigrationBase): integer
      begin
        Result := CompareText(GetPropValue(L, AFieldName), GetPropValue(R, AFieldName));

//        if L is TMigration then
//        begin
//          if TMigration(L).Version > TMigration(R).Version then
//            Result := 1
//          else if TMigration(L).Version < TMigration(R).Version then
//            Result := -1
//          else
//            Result := 0;
//        end
//        else if L is TMigrationEx then
//        begin
//          if TMigrationEx(L).AttribVersion > TMigrationEx(R).AttribVersion then
//            Result := 1
//          else if TMigrationEx(L).AttribVersion < TMigrationEx(R).AttribVersion then
//            Result := -1
//          else
//            Result := 0;
//        end;
      end
      ));
  end;
end;

{TObjListHelper}

function TObjListHelper.FindMigration(AMigrationObj: TMigrationBase): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Pred(Count) do
  begin
    if AMigrationObj is TMigration then
    begin
      if TMigration(AMigrationObj).Version = TMigration(Items[I]).Version then
      begin
        Result := True;
        Break;
      end;
    end
    else if AMigrationObj is TMigrationEx then
    begin
      if TMigrationEx(AMigrationObj).AttribVersion = TMigrationEx(Items[I]).AttribVersion then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

end.
