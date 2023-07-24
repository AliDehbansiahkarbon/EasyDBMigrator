unit EasyDB.Core;

interface
uses
  Vcl.ComCtrls, System.Generics.Collections;

type
  TMigrationBase = class;
  TArrangeMode = (umASC, umDESC);
  TMigrations = TObjectList<TMigrationBase>;
  TMigrationsDic = TObjectDictionary<Int64, TMigrations>;

  TActionTypes = (atUpgrade, atDownGrade, atInitialize, atPreparingMigrations,
                  atDbConnection, atQueryExecution, atFileExecution, atCallBackEvent);

  TSqlConnectionParams = record
    Server: string;
    LoginTimeout: Integer;
    UserName: string;
    Pass: string;
    DatabaseName: string;
    Schema: string;
  end;

  TMySqlConnectionParams = record
    Server: string;
    LoginTimeout: Integer;
    Port: Integer;
    UserName: string;
    Pass: string;
    Schema: string;
  end;

  TPgConnectionParams = record
    Server: string;
    LoginTimeout: Integer;
    Port: Integer;
    UserName: string;
    Pass: string;
    DatabaseName: string;
    Schema: string;
  end;

  TOracleConnectionParams = record
    Server: string;
    LoginTimeout: Integer;
    Port: Integer;
    UserName: string;
    Pass: string;
    DatabaseName: string;
  end;

  TMongoConnectionParams = record
    Server: string;
    LoginTimeout: Integer;
    Port: Integer;
    UserName: string;
    Pass: string;
  end;

  TSQLiteConnectionParams = record
    Server: string;
    LoginTimeout: Integer;
    Port: Integer;
    UserName: string;
    Pass: string;
  end;

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

  TMigrationBase = class
  public
    HiddenAttribDic: TDictionary<string, Variant>;
    HasAttribDic: Boolean;
    procedure CreateHiddenAttribDic(AEntityName: string; AVersion: int64; AAuthor: string; ADescription: string);
    destructor Destroy; override;
  end;

  //Runner.LogAllExecutions(True).UseInternalThread(True).SetProgressbar(pbTotal).RollBackAllByAnyError(True); //each part This line is Optional

  TConfig = class
  private
    FLogAllExecutions: Boolean;
    FUseInternalThread: Boolean;
    FProgressBar: TProgressBar;
    FDelay: Cardinal;
  public
    constructor Create;
    function LogAllExecutions(AValue: Boolean): TConfig;
    function UseInternalThread(AValue: Boolean): TConfig;
    function SetProgressbar(AProgressbar: TProgressBar): TConfig;
    function DelayedExecution(ADelay: Cardinal): TConfig;

    property ProgressBar: TProgressBar read FProgressBar;
    property UseThreadStat: Boolean read FUseInternalThread;
    property LogAllExecutionsStat: Boolean read FLogAllExecutions;
    property Delay: Cardinal read FDelay;
  end;

implementation
{ TMigrationBase }

uses
  EasyDB.Migration, EasyDB.MigrationX;

procedure TMigrationBase.CreateHiddenAttribDic(AEntityName: string; AVersion: int64; AAuthor: string; ADescription: string);
begin
  HiddenAttribDic := TDictionary<string, Variant>.Create;
  HiddenAttribDic.Add('EntityName', AEntityName);
  HiddenAttribDic.Add('Version', AVersion);
  HiddenAttribDic.Add('Author', AAuthor);
  HiddenAttribDic.Add('Description', ADescription);
end;

destructor TMigrationBase.Destroy;
begin
  if Assigned(HiddenAttribDic) then
    HiddenAttribDic.Free;
  inherited;
end;

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

{ TConfig }

constructor TConfig.Create;
begin
  FUseInternalThread := False;
  FLogAllExecutions := False;
  FProgressBar := nil;
  FDelay := 0;
end;

function TConfig.DelayedExecution(ADelay: Cardinal): TConfig;
begin
  FDelay := ADelay;
  Result := Self;
end;

function TConfig.LogAllExecutions(AValue: Boolean): TConfig;
begin
  FLogAllExecutions := AValue;
  Result := Self;
end;

function TConfig.SetProgressbar(AProgressbar: TProgressBar): TConfig;
begin
  FProgressBar := AProgressbar;
  Result := Self;
end;

function TConfig.UseInternalThread(AValue: Boolean): TConfig;
begin
  FUseInternalThread := AValue;
  Result := Self;
end;

end.
