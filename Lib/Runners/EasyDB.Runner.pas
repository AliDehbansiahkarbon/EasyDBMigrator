{$M+}
unit EasyDB.Runner;

interface
uses
  System.Rtti, System.Generics.Collections, EasyDB.Migration.Base, Vcl.Dialogs,
  EasyDB.Attribute, System.SysUtils, TypInfo, Contnrs, System.Classes;

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
    FMigrationListInternal: TObjectDictionary<string, TObjectList<TMigration>>;
    FMigrationList: TObjectList<TMigration>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpgradeDatabase;
    procedure DownGrade(AVersion: Integer);
    procedure LoadMigrationList;
    function GetDatabaseVersion: Integer;
    procedure ArrangeMigrationList;

    property MigrationList: TObjectList<TMigration> read FMigrationList write FMigrationList;
  end;

implementation
uses
  EasyDB.ConnectionManager.SQL;

{ TRunner }

procedure TRunner.ArrangeMigrationList;
var
  LvExternalMigration: TMigration;
  LvMigrationInternalList: TObjectList<TMigration>;
begin
  for LvExternalMigration in FMigrationList do
  begin
    if FMigrationListInternal.ContainsKey(LvExternalMigration.AttribEntityName) then
    begin
      LvMigrationInternalList := FMigrationListInternal.Items[LvExternalMigration.AttribEntityName];

      if not LvMigrationInternalList.Find(LvExternalMigration) then
        LvMigrationInternalList.Add(LvExternalMigration);


      //if LvMigrationInternal LvMigration.AttribVersion then

    end
    else
    begin

    end;
  end;
end;

constructor TRunner.Create;
begin
  FMigrationList := TObjectList<TMigration>.Create;
  FMigrationListInternal := TObjectDictionary<string, TObjectList<TMigration>>.Create;
end;

destructor TRunner.Destroy;
begin
  FMigrationList.Free;
  FMigrationListInternal.Free;
  inherited;
end;

procedure TRunner.UpgradeDatabase;
var
  LvDbVer: Integer;
begin
  if FMigrationList.Count = 0 then
    Exit;

  LvDbVer := GetDatabaseVersion;
  ArrangeMigrationList;



  if LvDbVer = -1 then // Initial execution
  begin
  end
  else
  begin

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

procedure TRunner.LoadMigrationList;
var
  LvCtx: TRttiContext;
  LvTypeInfo: TRttiType;
  LvProp: TRttiProperty;
  LvField: TRttiField;

  LvMigration: TMigration;
  LvObj: TObject;

begin
  LVCtx := TRttiContext.Create;


  try
    for LvTypeInfo in LvCtx.GetTypes do
    begin
      if LvTypeInfo.IsInstance and LvTypeInfo.AsInstance.MetaclassType.InheritsFrom(TMigration) then
      begin
        ShowMessage('a');
//        lvObj := LvTypeInfo.AsInstance;
//        ShowMessage(LvTypeInfo.GetProperty('Version').GetValue(lvObj).AsInteger.ToString);

//        ShowMessage(TMigration(LvTypeInfo.GetMethod('GetMigration').Invoke(LvTypeInfo.AsInstance, []).AsObject).Version.ToString);
//
//        if LvTypeInfo.GetMethod('GetMigration').Invoke(LvTypeInfo.AsInstance, []).IsObject then
//        begin
//          LvMigration := LvTypeInfo.GetMethod('GetMigration').Invoke(LvTypeInfo.AsInstance, []).AsObject as TMigration;
//          ShowMessage(LvMigration.Version.ToString);
//        end;

        //ShowMessage(GetPropValue(TMigration(lvObj), 'Version'));

//        FMigrationList.ContainsKey(TMigration(lvObj).)
//        LvTempMigrationList.Add(TMigration(lvObj));
      end;
    end;

//    if LvTempMigrationList.Count > 0 then
//    begin
//
//    end;

  finally
    LvCtx.Free;
//    LvTempMigrationList
  end;


end;

{TObjListHelper}

function TObjListHelper.Find(AMigrationObj: TMigration): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Pred(Count) do
  begin
    if AMigrationObj.AttribVersion = Items[I].AttribVersion then
    begin
      Result := True;
      Break;
    end;
  end;
end;



end.
