unit EasyDB.Runner;

interface
uses
  System.Rtti, System.Generics.Collections, Vcl.Dialogs, Vcl.ComCtrls, System.SysUtils,
  System.TypInfo, System.Contnrs, System.Classes, System.Generics.Defaults, System.StrUtils,
  {$IF CompilerVersion >= 30}System.Threading,{$IFEND}

  EasyDB.Core,
  EasyDB.Logger,
  EasyDB.Migration,
  EasyDB.MigrationX,
  EasyDB.Attribute,
  EasyDB.ORM;

type
  {$IF CompilerVersion < 30}
  TObjListHelper = class helper for TMigrations
  public
    function FindMigration(AMigrationObj: TMigrationBase): Boolean;
  end;
  {$IFEND}
  TRunner = class(TInterfacedObject, IRunner)
  private
    FInternalMigrationList: TMigrationsDic;
    FMigrationList: TMigrations;
    FConfig: TConfig;
    FLogger: TLogger;
    FVersionToDowngrade: Int64;
    FOrm: TORM;

    function GetLogger: TLogger;
    function CreateInternalMigration(AExternalMigration: TMigration): TMigration;
    function CreateInternalMigrationEx(AExternalMigrationEx: TMigrationX): TMigrationX;
    procedure SortArrayDesc(AArray: TArray<Int64>);
    procedure DoUpgrade;
    procedure DoDowngrade;
    procedure DoProgress(AZero: Boolean = False);
  protected
    FRollBackAllByAnyError: Boolean;

    function GetDatabaseVersion: Int64; virtual; abstract;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); virtual; abstract;
    procedure UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function AddLogger: TLogger;
    function Add(AMigrationBase: TMigrationBase): TRunner;
    function Clear: TRunner;

    procedure UpgradeDatabase;
    procedure DowngradeDatabase(AVersion: Int64);
    procedure ArrangeMigrationList(AArrangeMode: TArrangeMode);
    procedure SortDictionaryByField(ADict: TMigrationsDic; AArrangeMode: TArrangeMode; AIsMigrationX: Boolean);

    property MigrationList: TMigrations read FMigrationList write FMigrationList;
    property Config: TConfig read FConfig;
    property Logger: TLogger read GetLogger;
    property ORM: TORM read FOrm write FOrm;
  end;

implementation

{ TRunner }

function TRunner.Clear: TRunner;
begin
  FMigrationList.Clear;
  Result := Self;
end;

constructor TRunner.Create;
begin
  FOrm := nil;
  FVersionToDowngrade := 0;
  FConfig := TConfig.Create;
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
  FreeAndNil(FLogger);
  FreeAndNil(FConfig);

  if Assigned(FOrm) then
    FOrm.Free;

  inherited;
end;

function TRunner.Add(AMigrationBase: TMigrationBase): TRunner;
begin
  FMigrationList.Add(AMigrationBase);
  Result := Self;
end;

function TRunner.AddLogger: TLogger;
begin
  FLogger := TLogger.Instance;
  Result := FLogger;
end;

procedure TRunner.ArrangeMigrationList(AArrangeMode: TArrangeMode);
var
  LvExternalMigration: TMigrationBase;
  LvNewInternalList: TMigrations;

  LvTempMigration: TMigration;
  LvTempMigrationEx: TMigrationX;
  Lvkey: Int64;

  LvIsMigrationX: Boolean;
begin
  try
    LvIsMigrationX := False;
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
        Lvkey := LvTempMigration.Version;

        if FInternalMigrationList.ContainsKey(Lvkey) then
        begin
          if not FInternalMigrationList.Items[Lvkey].FindMigration(LvTempMigration) then
            FInternalMigrationList.Items[Lvkey].Add(CreateInternalMigration(LvTempMigration));
        end
        else
        begin
          LvNewInternalList := TMigrations.Create;
          LvNewInternalList.Add(CreateInternalMigration(LvTempMigration));
          FInternalMigrationList.Add(Lvkey, LvNewInternalList);
        end;
      end
      else if LvExternalMigration is TMigrationX then
      begin
        LvIsMigrationX := True;
        LvTempMigrationEx := TMigrationX(LvExternalMigration);
        Lvkey := LvTempMigrationEx.AttribVersion;

        if FInternalMigrationList.ContainsKey(Lvkey) then
        begin
          if not FInternalMigrationList.Items[Lvkey].FindMigration(LvTempMigrationEx) then
            FInternalMigrationList.Items[Lvkey].Add(CreateInternalMigrationEx(LvTempMigrationEx));
        end
        else
        begin
          LvNewInternalList := TMigrations.Create;
          LvNewInternalList.Add(CreateInternalMigrationEx(LvTempMigrationEx));
          FInternalMigrationList.Add(Lvkey, LvNewInternalList);
        end;
      end;
    end;

    SortDictionaryByField(FInternalMigrationList, AArrangeMode, LvIsMigrationX);
  except on E: Exception do
    Logger.Log(atPreparingMigrations, E.Message);
  end;
end;

procedure TRunner.UpgradeDatabase;
begin
  if FConfig.UseThreadStat then
    {$IF CompilerVersion >= 30}// 10 Seattle and above
    TTask.Run(DoUpgrade)
    {$ELSE}
    TThread.CreateAnonymousThread(DoUpgrade).Start
    {$IFEND}
  else
    DoUpgrade;
end;

procedure TRunner.DowngradeDatabase(AVersion: Int64);
begin
  FVersionToDowngrade := AVersion;
  if FConfig.UseThreadStat then
    {$IF CompilerVersion >= 30}// 10 Seattle and above
    TTask.Run(DoDowngrade)
    {$ELSE}
    TThread.CreateAnonymousThread(DoDowngrade).Start
    {$IFEND}
  else
    DoDowngrade;
end;

function TRunner.GetLogger: TLogger;
begin
  Result := TLogger.Instance;
end;

procedure TRunner.DoDowngrade;
var
  LvDbVer: Int64;
  LvMigrationList: TMigrations;
  LvInternalMigration: TMigrationBase;

  LvTempMigration: TMigration;
  LvTempMigrationEx: TMigrationX;

  LvKey: Int64;
  LvArray : TArray<Int64>;
begin
  if FMigrationList.Count = 0 then
    Exit;

  LvDbVer := GetDatabaseVersion;
  if LvDbVer <= FVersionToDowngrade then
    Exit;

  ArrangeMigrationList(umDESC);
  LvArray := FInternalMigrationList.Keys.ToArray;

  if Length(LvArray) = 0 then
    Exit;

  SortArrayDesc(LvArray);
  DoProgress;
  for LvKey in LvArray do
  begin
    if LvKey > LvDbVer then
    begin
      DoProgress;
      Continue;
    end;

    LvMigrationList := FInternalMigrationList.Items[LvKey];
    for LvInternalMigration in LvMigrationList do
    begin
      if FConfig.Delay > 0 then
        Sleep(FConfig.Delay);

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
          DoProgress;
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
          DoProgress;
      end;
    end;
  end;

  DownGradeVersionInfo(FVersionToDowngrade);

  if Assigned(FConfig.ProgressBar) then
    DoProgress(True);
end;

procedure TRunner.DoProgress(AZero: Boolean);
begin
  if AZero then
    TThread.Synchronize({$IF CompilerVersion >= 30}TThread.Current{$ELSE}TThread.CurrentThread{$IFEND}, procedure begin FConfig.ProgressBar.Position := 0; end)
  else
    TThread.Synchronize({$IF CompilerVersion >= 30}TThread.Current{$ELSE}TThread.CurrentThread{$IFEND}, procedure begin FConfig.ProgressBar.Position := FConfig.ProgressBar.Position + 1; end);
end;

procedure TRunner.DoUpgrade;
var
  LvDbVer: Int64;
  LvMigrationList: TMigrations;
  LvInternalMigration: TMigrationBase;
  LvWrittenVersions: TList<Int64>;

  LvTempVersion: Int64;
  LvKey: Int64;

  LvTempMigration: TMigration;
  LvTempMigrationEx: TMigrationX;

  LvArray : TArray<Int64>;
begin
  if FMigrationList.Count = 0 then
    Exit;

  LvWrittenVersions := TList<Int64>.Create;
  try
    LvDbVer := GetDatabaseVersion;
    ArrangeMigrationList(umASC);

    LvArray := FInternalMigrationList.Keys.ToArray;
    TArray.Sort<Int64>(LvArray);
    DoProgress;
    for LvKey in LvArray do
    begin
      LvMigrationList := FInternalMigrationList.Items[LvKey];

      for LvInternalMigration in LvMigrationList do
      begin
        if FConfig.Delay > 0 then
          Sleep(FConfig.Delay);

        if LvInternalMigration is TMigration then
        begin
          LvTempMigration := TMigration(LvInternalMigration);
          LvTempVersion :=  LvTempMigration.Version;
          if LvTempVersion > LvDbVer then
          begin
            try
              LvTempMigration.Upgrade;
              if FConfig.LogAllExecutionsStat then
                Logger.DoCallBack(atUpgrade, 'Executed Successfully', LvTempMigration.EntityName, LvTempMigration.Version);

              if not LvWrittenVersions.Contains(LvTempVersion) then
              begin
                UpdateVersionInfo(LvTempMigration);
                LvWrittenVersions.Add(LvTempVersion);
              end
              else
                UpdateVersionInfo(LvTempMigration, False);
            except on E: Exception do
              Logger.Log(atUpgrade, E.Message, LvTempMigration.EntityName, LvTempVersion);
            end;
          end;
          if Assigned(FConfig.ProgressBar) then
            DoProgress;
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
          end;
          if Assigned(FConfig.ProgressBar) then
            DoProgress;
        end;
      end;
    end;
  finally
    LvWrittenVersions.Free;
    if Assigned(FConfig.ProgressBar) then
      DoProgress(True);
  end;
end;

procedure TRunner.SortArrayDesc(AArray: TArray<Int64>);
begin
  TArray.Sort<Int64>(AArray,
  TComparer<Int64>.Construct(
    function(const Left, Right: Int64): Integer
    begin
      if Left = Right then
        Result := 0
      else if Left < Right then
        Result := 1
      else
        Result := -1;
    end)); // Desc sort
end;

procedure TRunner.SortDictionaryByField(ADict: TMigrationsDic; AArrangeMode: TArrangeMode; AIsMigrationX: Boolean);
var
  LvValue: TMigrations;
  LvFieldName: string;
begin
  LvFieldName := IfThen(AIsMigrationX, 'AttribVersion', 'Version');

  for LvValue in ADict.Values do
  begin
    LvValue.Sort(TComparer<TMigrationBase>.Construct(
      function (const L,R: TMigrationBase): integer
      begin
        if AArrangeMode = umASC then
          Result := CompareText(GetPropValue(L, LvFieldName), GetPropValue(R, LvFieldName))
        else
          Result := CompareText(GetPropValue(R, LvFieldName), GetPropValue(L, LvFieldName));
      end
      ));
  end;
end;

{$IF CompilerVersion < 30}
{ TObjListHelper }
function TObjListHelper.FindMigration(AMigrationObj: TMigrationBase): Boolean;
var
  I: Integer;
  LvTempMigration: TMigration;
  LvTempMigrationX: TMigrationX;
begin
  Result := False;
  for I := 0 to Pred(Self.Count) do
  begin
    if AMigrationObj is TMigration then
    begin
      LvTempMigration := TMigration(AMigrationObj);
      if (LvTempMigration.Version = TMigration(Self.Items[I]).Version) and
         (LvTempMigration.EntityName = TMigration(Self.Items[I]).EntityName) then
      begin
        Result := True;
        Break;
      end;
    end
    else if AMigrationObj is TMigrationX then
    begin
      LvTempMigrationX := TMigrationX(AMigrationObj);
      if (LvTempMigrationX.AttribVersion = TMigrationX(Self.Items[I]).AttribVersion) and
         (LvTempMigrationX.AttribEntityName = TMigrationX(Self.Items[I]).AttribEntityName) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;
{$IFEND}
end.
