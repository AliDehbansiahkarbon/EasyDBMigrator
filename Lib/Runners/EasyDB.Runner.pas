unit EasyDB.Runner;

interface
uses
  System.Rtti, System.Generics.Collections, Vcl.Dialogs, System.Threading, Vcl.ComCtrls,
  System.SysUtils, TypInfo, Contnrs, System.Classes, System.Generics.Defaults,

  EasyDB.Logger,
  EasyDB.Migration.Base,
  EasyDB.Migration.Attrib,
  EasyDB.Attribute,
  EasyDB.ConnectionManager.SQL;

type
  TArrangeMode = (umASC, umDESC);
  TMigrations = TObjectList<TMigrationBase>;
  TMigrationsDic = TObjectDictionary<string, TMigrations>;

  TObjListHelper = class helper for TMigrations
  public
    function FindMigration(AMigrationObj: TMigrationBase): Boolean;
  end;

  IRunner = interface
    ['{DECF074C-109F-488F-A97D-4B3C68FB4F35}']

    procedure UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True);
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64);
    function GetDatabaseVersion: Int64;
  end;

  TRunner = class(TInterfacedObject, IRunner)
  private
    FInternalMigrationList: TMigrationsDic;
    FMigrationList: TMigrations;
    FUseThread: Boolean;
    FLogAllExecutions: Boolean;
    FVersionToDowngrade: Int64;
    FProgressBar : TProgressBar;
    function CreateInternalMigrationEx(AExternalMigrationEx: TMigrationEx): TMigrationEx;
    procedure DoUpgrade;
    procedure DoDowngrade;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpgradeDatabase;
    procedure DowngradeDatabase(AVersion: Int64);
    procedure ArrangeMigrationList(AArrangeMode: TArrangeMode);
    function Logger: TLogger;
    procedure SortDictionaryByField(ADict: TObjectDictionary<string, TMigrations>; AFieldName: string; AArrangeMode: TArrangeMode);

    procedure UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True); virtual; abstract;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); virtual; abstract;
    function GetDatabaseVersion: Int64; virtual; abstract;

    property MigrationList: TMigrations read FMigrationList write FMigrationList;
    property UseInternalThread: Boolean read FUseThread write FUseThread;
    property LogAllExecutions: Boolean read FLogAllExecutions write FLogAllExecutions;
    property Progressbar: TProgressBar read FProgressBar write FProgressBar;
  end;

implementation

{ TRunner }

constructor TRunner.Create;
begin
  FUseThread := False;
  FLogAllExecutions := False;
  FVersionToDowngrade := 0;
  FProgressBar := nil;
  FMigrationList := TMigrations.Create;
  FMigrationList.OwnsObjects := True;
  FInternalMigrationList := TMigrationsDic.Create([doOwnsValues]);
end;

function TRunner.CreateInternalMigrationEx(AExternalMigrationEx: TMigrationEx): TMigrationEx;
begin
  Result := TMigrationEx.Create(AExternalMigrationEx.Upgrade, AExternalMigrationEx.Downgrade);
  Result.HasAttribDic := True;
  with AExternalMigrationEx do
    Result.CreateHiddenAttribDic(AttribEntityName, AttribVersion, AttribAuthor, AttribDescription);
end;

destructor TRunner.Destroy;
begin
  FreeAndNil(FInternalMigrationList);
  FreeAndNil(FMigrationList);
  FreeAndNil(TLogger.Instance);
  inherited;
end;

procedure TRunner.ArrangeMigrationList(AArrangeMode: TArrangeMode);
var
  LvExternalMigration: TMigrationBase;
  LvNewInternalList: TMigrations;

  LvTempMigration: TMigration;
  LvTempMigrationEx: TMigrationEx;
  LvTempEntityName: string;
begin
  try
    if Assigned(FProgressBar) then
    begin
      FProgressBar.Min := 0;
      FProgressBar.Max := FMigrationList.Count;
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
          begin
            with LvTempMigration do
              FInternalMigrationList.Items[LvTempEntityName].Add(TMigration.Create(EntityName, Version, Author, Description, Upgrade, Downgrade));
          end;
        end
        else
        begin
          LvNewInternalList := TMigrations.Create;
          LvNewInternalList.Add(LvTempMigration);
          FInternalMigrationList.Add(LvTempEntityName, LvNewInternalList);
        end;

        SortDictionaryByField(FInternalMigrationList, 'Version', AArrangeMode);
      end
      else if LvExternalMigration is TMigrationEx then
      begin
        LvTempMigrationEx := TMigrationEx(LvExternalMigration);
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

function TRunner.Logger: TLogger;
begin
  Result := TLogger.Instance;
end;

procedure TRunner.UpgradeDatabase;
begin
  if FUseThread then
    TTask.Run(DoUpgrade)
  else
    DoUpgrade;
end;

procedure TRunner.DowngradeDatabase(AVersion: Int64);
begin
  FVersionToDowngrade := AVersion;
  if FUseThread then
    TTask.Run(DoDowngrade)
  else
    DoDowngrade;
end;

procedure TRunner.DoDowngrade;
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
            if FLogAllExecutions then
              Logger.DoCallBack(atDownGrade, 'Executed Successfully', LvTempMigration.EntityName, LvTempMigration.Version);
          end;
        except on E: Exception do
          Logger.Log(atUpgrade, E.Message, LvTempMigration.EntityName, LvTempMigration.Version);
        end;

        if Assigned(FProgressBar) then
          TThread.Synchronize(TThread.Current, procedure begin FProgressBar.Position := FProgressBar.Position + 1; end);
      end
      else if LvInternalMigration is TMigrationEx then
      begin
        LvTempMigrationEx := TMigrationEx(LvInternalMigration);
        try
          if LvTempMigrationEx.AttribVersion > FVersionToDowngrade then
          begin
            LvTempMigrationEx.Downgrade;

            if FLogAllExecutions then
              Logger.DoCallBack(atDownGrade, 'Executed Successfully', LvTempMigrationEx.AttribEntityName, LvTempMigrationEx.AttribVersion);
          end;
        except on E: Exception do
          Logger.Log(atUpgrade, E.Message, LvTempMigrationEx.AttribEntityName, LvTempMigrationEx.AttribVersion);
        end;

        if Assigned(FProgressBar) then
          TThread.Synchronize(TThread.Current, procedure begin FProgressBar.Position := FProgressBar.Position + 1; end);
      end;
    end;
  end;

  DownGradeVersionInfo(FVersionToDowngrade);
  if Assigned(FProgressBar) then
    TThread.Synchronize(TThread.Current, procedure begin FProgressBar.Position := 0; end);
end;

procedure TRunner.DoUpgrade;
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
              LvTempMigration.Upgrade

              if not LvWrittenVersions.Contains(LvTempVersion) then
              begin
                UpdateVersionInfo(LvTempMigration);
                LvWrittenVersions.Add(LvTempVersion);
                if FLogAllExecutions then
                  Logger.DoCallBack(atUpgrade, 'Executed Successfully', LvTempMigration.EntityName, LvTempMigration.Version);
              end
              else
                UpdateVersionInfo(LvTempMigration, False);

            except on E: Exception do
              Logger.Log(atUpgrade, E.Message, LvTempMigration.EntityName, LvTempVersion);
            end;

            if Assigned(FProgressBar) then
              TThread.Synchronize(TThread.Current, procedure begin FProgressBar.Position := FProgressBar.Position + 1; end);
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
                UpdateVersionInfo(LvTempMigrationEx);
                LvWrittenVersions.Add(LvTempVersion);

                if FLogAllExecutions then
                  Logger.DoCallBack(atUpgrade, 'Executed Successfully', LvTempMigrationEx.AttribEntityName, LvTempMigrationEx.AttribVersion);
              end
              else
                UpdateVersionInfo(LvTempMigrationEx, False);

            except on E: Exception do
              Logger.Log(atUpgrade, E.Message, LvTempMigrationEx.AttribEntityName, LvTempVersion);
            end;

            if Assigned(FProgressBar) then
              TThread.Synchronize(TThread.Current, procedure begin FProgressBar.Position := FProgressBar.Position + 1; end);
          end;
        end;
      end;
    end;
  finally
    LvWrittenVersions.Free;
    if Assigned(FProgressBar) then
      TThread.Synchronize(TThread.Current, procedure begin FProgressBar.Position := 0; end);
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
