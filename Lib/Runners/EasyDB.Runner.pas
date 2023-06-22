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
  TArrangeMode = (umASC, umDESC);
  TMigrations = TObjectList<TMigrationBase>;
  TMigrationsDic = TObjectDictionary<string, TMigrations>;

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
    FInternalMigrationList: TMigrationsDic;
    FMigrationList: TMigrations;
    function CreateInternalMigrationEx(AExternalMigrationEx: TMigrationEx): TMigrationEx;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpgradeDatabase;
    procedure DowngradeDatabase(AVersion: Int64);
    procedure ArrangeMigrationList(AArrangeMode: TArrangeMode);
    function Logger: TLogger;
    procedure SortDictionaryByField(ADict: TObjectDictionary<string, TMigrations>; AFieldName: string; AArrangeMode: TArrangeMode);

    procedure UpdateVersionInfo(ALatestVersion: Int64; AAuthor: string; ADescription: string; AInsertMode: Boolean = True); virtual; abstract;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); virtual; abstract;
    function GetDatabaseVersion: Int64; virtual; abstract;

    property MigrationList: TMigrations read FMigrationList write FMigrationList;
  end;

implementation

{ TRunner }

constructor TRunner.Create;
begin
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
var
  LvKey: string;
begin
//  if (FInternalMigrationList.Count > 0) and (FMigrationList.Items[0] is TMigrationEx) then
//  begin
//    for LvKey in FInternalMigrationList.Keys do
//      FreeAndNil(FInternalMigrationList.Items[LvKey]);
//
//    FMigrationList.OwnsObjects := False;
//  end;
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

  ArrangeMigrationList(umDESC);

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
