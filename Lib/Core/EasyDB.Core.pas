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

  TMariaDBConnectionParams = record
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

  TFirebirdConnectionParams = record
    Host: string;
    Database: string;
    UserName: string;
    Pass: string;
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
