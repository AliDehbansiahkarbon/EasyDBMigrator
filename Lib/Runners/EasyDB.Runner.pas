unit EasyDB.Runner;

interface
uses
  System.Rtti, System.Generics.Collections, Vcl.Dialogs, System.Threading, Vcl.ComCtrls,
  System.SysUtils, TypInfo, Contnrs, System.Classes, System.Generics.Defaults,

  EasyDB.Core,
  EasyDB.Logger,
  EasyDB.Migration,
  EasyDB.MigrationX,
  EasyDB.Attribute,
  EasyDB.ConnectionManager.SQL;

type
  TRunner = class(TInterfacedObject, IRunner)
  private
    FInternalMigrationList: TMigrationsDic;
    FMigrationList: TMigrations;
    FConfig: TConfig;
    FVersionToDowngrade: Int64;

    procedure DoUpgrade;
    procedure DoDowngrade;
    function CreateInternalMigration(AExternalMigration: TMigration): TMigration;
    function CreateInternalMigrationEx(AExternalMigrationEx: TMigrationX): TMigrationX;
  protected
    FRollBackAllByAnyError: Boolean;

    function GetDatabaseVersion: Int64; virtual; abstract;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); virtual; abstract;
    procedure UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function Logger: TLogger;
    function AddConfig: TConfig;
    procedure UpgradeDatabase;
    procedure DowngradeDatabase(AVersion: Int64);
    procedure ArrangeMigrationList(AArrangeMode: TArrangeMode);
    procedure SortDictionaryByField(ADict: TObjectDictionary<string, TMigrations>; AFieldName: string; AArrangeMode: TArrangeMode);

    property MigrationList: TMigrations read FMigrationList write FMigrationList;
  end;

implementation

{ TRunner }

constructor TRunner.Create;
begin
  FConfig := nil;
  FVersionToDowngrade := 0;
  FMigrationList := TMigrations.Create;
  FMigrationList.OwnsObjects := True;
  FInternalMigrationList := TMigrationsDic.Create([doOwnsValues]);
end;

function TRunner.CreateInternalMigration(AExternalMigration: TMigration): TMigration;
begin
  with AExternalMigration do
   Result := TMigration.Create(EntityName, Version, Author, Description, Upgrade, Downgrade);
end;

function TRunner.CreateInternalMigrationEx(AExternalMigrationEx: TMigrationX): TMigrationX;
begin
  Result := TMigrationX.Create(AExternalMigrationEx.Upgrade, AExternalMigrationEx.Downgrade);
  Result.HasAttribDic := True;

  with AExternalMigrationEx do
    Result.CreateHiddenAttribDic(AttribEntityName, AttribVersion, AttribAuthor, AttribDescription);
end;

destructor TRunner.Destroy;
begin
  FreeAndNil(FInternalMigrationList);
  FreeAndNil(FMigrationList);
  FreeAndNil(TLogger.Instance);

  if Assigned(FConfig) then
    FConfig.Free;

  inherited;
end;

function TRunner.AddConfig: TConfig;
begin
  FConfig := TConfig.Create;
  Result := FConfig;
end;

procedure TRunner.ArrangeMigrationList(AArrangeMode: TArrangeMode);
var
  LvExternalMigration: TMigrationBase;
  LvNewInternalList: TMigrations;

  LvTempMigration: TMigration;
  LvTempMigrationEx: TMigrationX;
  LvTempEntityName: string;
begin
  try
    if Assigned(FConfig.ProgressBar) then
    begin
      FConfig.ProgressBar.Min := 0;
      FConfig.ProgressBar.Max := FMigrationList.Count;
    end;

    for LvExternalMigration in FMigrationList do
    begin
      if LvExternalMigration is TMigration then
      begin
        LvTempMigration := TMigration(LvExternalMigration);
        LvTempEntityName := LvTempMigration.EntityName;

        if FInternalMigrationList.ContainsKey(LvTempEntityName) then
        begin
          if not FInternalMigrationList.Items[LvTempEntityName].FindMigration(LvTempMigration) then
            FInternalMigrationList.Items[LvTempEntityName].Add(CreateInternalMigration(LvTempMigration));
        end
        else
        begin
          LvNewInternalList := TMigrations.Create;
          LvNewInternalList.Add(CreateInternalMigration(LvTempMigration));
          FInternalMigrationList.Add(LvTempEntityName, LvNewInternalList);
        end;

        SortDictionaryByField(FInternalMigrationList, 'Version', AArrangeMode);
      end
      else if LvExternalMigration is TMigrationX then
      begin
        LvTempMigrationEx := TMigrationX(LvExternalMigration);
        LvTempEntityName := LvTempMigrationEx.AttribEntityName;

        if FInternalMigrationList.ContainsKey(LvTempEntityName) then
        begin
          if not FInternalMigrationList.Items[LvTempEntityName].FindMigration(LvTempMigrationEx) then
            FInternalMigrationList.Items[LvTempEntityName].Add(CreateInternalMigrationEx(LvTempMigrationEx));
        end
        else
        begin
          LvNewInternalList := TMigrations.Create;
          LvNewInternalList.Add(CreateInternalMigrationEx(LvTempMigrationEx));
          FInternalMigrationList.Add(LvTempEntityName, LvNewInternalList);
        end;

        SortDictionaryByField(FInternalMigrationList, 'AttribVersion', AArrangeMode);
      end;
    end;
  except on E: Exception do
    Logger.Log(atPreparingMigrations, E.Message);
  end;
end;

procedure TRunner.UpgradeDatabase;
begin
  if FConfig.UseThreadStat then
    TTask.Run(DoUpgrade)
  else
    DoUpgrade;
end;

procedure TRunner.DowngradeDatabase(AVersion: Int64);
begin
  FVersionToDowngrade := AVersion;
  if FConfig.UseThreadStat then
    TTask.Run(DoDowngrade)
  else
    DoDowngrade;
end;

function TRunner.Logger: TLogger;
begin
  Logger := TLogger.Instance;
end;

procedure TRunner.DoDowngrade;
var
  LvDbVer: Int64;
  LvMigrationList: TMigrations;
  LvInternalMigration: TMigrationBase;

  LvTempMigration: TMigration;
  LvTempMigrationEx: TMigrationX;
begin
  if FMigrationList.Count = 0 then
    Exit;

  LvDbVer := GetDatabaseVersion;
  if LvDbVer <= FVersionToDowngrade then
    Exit;

  ArrangeMigrationList(umDESC);

  for LvMigrationList in FInternalMigrationList.Values do
  begin
    for LvInternalMigration in LvMigrationList do
    begin
      if LvInternalMigration is TMigration then
      begin
        LvTempMigration := TMigration(LvInternalMigration);
        try
          if LvTempMigration.Version > FVersionToDowngrade then
          begin
            LvTempMigration.Downgrade;
            if FConfig.LogAllExecutionsStat then
              Logger.DoCallBack(atDownGrade, 'Executed Successfully', LvTempMigration.EntityName, LvTempMigration.Version);
          end;
        except on E: Exception do
          Logger.Log(atUpgrade, E.Message, LvTempMigration.EntityName, LvTempMigration.Version);
        end;

        if Assigned(FConfig.ProgressBar) then
          TThread.Synchronize(TThread.Current, procedure begin FConfig.ProgressBar.Position := FConfig.ProgressBar.Position + 1; end);
      end
      else if LvInternalMigration is TMigrationX then
      begin
        LvTempMigrationEx := TMigrationX(LvInternalMigration);
        try
          if LvTempMigrationEx.AttribVersion > FVersionToDowngrade then
          begin
            LvTempMigrationEx.Downgrade;

            if FConfig.LogAllExecutionsStat then
              Logger.DoCallBack(atDownGrade, 'Executed Successfully', LvTempMigrationEx.AttribEntityName, LvTempMigrationEx.AttribVersion);
          end;
        except on E: Exception do
          Logger.Log(atUpgrade, E.Message, LvTempMigrationEx.AttribEntityName, LvTempMigrationEx.AttribVersion);
        end;

        if Assigned(FConfig.ProgressBar) then
          TThread.Synchronize(TThread.Current, procedure begin FConfig.ProgressBar.Position := FConfig.ProgressBar.Position + 1; end);
      end;
    end;
  end;

  DownGradeVersionInfo(FVersionToDowngrade);

  if Assigned(FConfig.ProgressBar) then
    TThread.Synchronize(TThread.Current, procedure begin FConfig.ProgressBar.Position := 0; end);
end;

procedure TRunner.DoUpgrade;
var
  LvDbVer: Int64;
  LvMigrationList: TMigrations;
  LvInternalMigration: TMigrationBase;
  LvWrittenVersions: TList<Int64>;

  LvTempVersion: Int64;
  LvTempMigration: TMigration;
  LvTempMigrationEx: TMigrationX;
begin
  if FMigrationList.Count = 0 then
    Exit;

  LvTempVersion := 0;
  LvWrittenVersions := TList<Int64>.Create;
  try
    LvDbVer := GetDatabaseVersion;
    ArrangeMigrationList(umASC);

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
                UpdateVersionInfo(LvTempMigration);
                LvWrittenVersions.Add(LvTempVersion);
                if FConfig.LogAllExecutionsStat then
                  Logger.DoCallBack(atUpgrade, 'Executed Successfully', LvTempMigration.EntityName, LvTempMigration.Version);
              end
              else
                UpdateVersionInfo(LvTempMigration, False);
            except on E: Exception do
              Logger.Log(atUpgrade, E.Message, LvTempMigration.EntityName, LvTempVersion);
            end;

            if Assigned(FConfig.ProgressBar) then
              TThread.Synchronize(TThread.Current, procedure begin FConfig.ProgressBar.Position := FConfig.ProgressBar.Position + 1; end);
          end;
        end
        else if LvInternalMigration is TMigrationX then
        begin
          LvTempMigrationEx := TMigrationX(LvInternalMigration);
          LvTempVersion := LvTempMigrationEx.AttribVersion;

          if LvTempVersion > LvDbVer then
          begin
            try
              LvTempMigrationEx.Upgrade;

              if not LvWrittenVersions.Contains(LvTempVersion) then
              begin
                UpdateVersionInfo(LvTempMigrationEx);
                LvWrittenVersions.Add(LvTempVersion);

                if FConfig.LogAllExecutionsStat then
                  Logger.DoCallBack(atUpgrade, 'Executed Successfully', LvTempMigrationEx.AttribEntityName, LvTempMigrationEx.AttribVersion);
              end
              else
                UpdateVersionInfo(LvTempMigrationEx, False);

            except on E: Exception do
              Logger.Log(atUpgrade, E.Message, LvTempMigrationEx.AttribEntityName, LvTempVersion);
            end;

            if Assigned(FConfig.ProgressBar) then
              TThread.Synchronize(TThread.Current, procedure begin FConfig.ProgressBar.Position := FConfig.ProgressBar.Position + 1; end);
          end;
        end;
      end;
    end;
  finally
    LvWrittenVersions.Free;
    if Assigned(FConfig.ProgressBar) then
      TThread.Synchronize(TThread.Current, procedure begin FConfig.ProgressBar.Position := 0; end);
  end;
end;

procedure TRunner.SortDictionaryByField(ADict: TObjectDictionary<string, TMigrations>; AFieldName: string; AArrangeMode: TArrangeMode);
var
  LvValue: TMigrations;
begin
  for LvValue in ADict.Values do
  begin
    LvValue.Sort(TComparer<TMigrationBase>.Construct(
      function (const L,R: TMigrationBase): integer
      begin
        if AArrangeMode = umASC then
          Result := CompareText(GetPropValue(L, AFieldName), GetPropValue(R, AFieldName))
        else
          Result := CompareText(GetPropValue(R, AFieldName), GetPropValue(L, AFieldName));
      end
      ));
  end;
end;

end.
