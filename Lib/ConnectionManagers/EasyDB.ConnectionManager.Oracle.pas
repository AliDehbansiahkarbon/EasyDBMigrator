unit EasyDB.ConnectionManager.Oracle;

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.StrUtils,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet,{==Oracle==} FireDAC.Phys.OracleDef, FireDAC.Phys.Oracle,{==Oracle==}

  EasyDB.ConnectionManager.Base,
  EasyDB.Core,
  EasyDB.Logger,
  EasyDB.Consts,
  EasyDB.Runner;

type

  TOracleConnection = class(TConnection) // Singletone
  private
    FConnection: TFDConnection;
    FOracleDriver: TFDPhysOracleDriverLink;
    FQuery: TFDQuery;
    FConnectionParams: TOracleConnectionParams;
    FParentRunner: TRunner;
    Constructor Create;
    class var FInstance: TOracleConnection;
  public
    class function Instance: TOracleConnection;
    Destructor Destroy; override;

    function GetConnectionString: string; override;
    function SetConnectionParam(AConnectionParams: TOracleConnectionParams): TOracleConnection;
    function Connect: Boolean; override;
    function ConnectEx: TOracleConnection;
    function IsConnected: Boolean;
    function InitializeDatabase: Boolean;
    function Logger: TLogger; override;

    function ExecuteAdHocQuery(AScript: string): Boolean; override;
    function ExecuteAdHocQueryWithTransaction(AScript: string): Boolean;

    function RemoveCommentFromTSQL(const ASQLLine: string): string;
    function OpenAsInteger(AScript: string): Largeint;

    procedure BeginTrans;
    procedure CommitTrans;
    procedure RollBackTrans;

    property ConnectionParams: TOracleConnectionParams read FConnectionParams;
    property ParentRunner: TRunner read FParentRunner write FParentRunner;
  end;

implementation

{ TOracleConnection }

function TOracleConnection.ConnectEx: TOracleConnection;
begin
  if Connect then
    Result := FInstance
  else
    Result := nil;
end;

constructor TOracleConnection.Create;
begin
  FConnection := TFDConnection.Create(nil);
  FOracleDriver := TFDPhysOracleDriverLink.Create(nil);

  FConnection.DriverName := 'MSSQL';
  FConnection.LoginPrompt := False;

  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;

  FParentRunner := nil;
end;

destructor TOracleConnection.Destroy;
begin
  FQuery.Close;
  FQuery.Free;
  FOracleDriver.Free;

  FConnection.Close;
  FConnection.Free;

  FInstance := nil;
  inherited;
end;

procedure TOracleConnection.BeginTrans;
begin
  FConnection.Transaction.StartTransaction;
end;

function TOracleConnection.Connect: Boolean;
begin
  try
    FConnection.Connected := True;
    InitializeDatabase;

    Result := True;
  except on E: Exception do
    begin
      Logger.Log(atDbConnection, E.Message);
      Result := False;
    end;
  end;
end;

procedure TOracleConnection.CommitTrans;
begin
  FConnection.Transaction.Commit;
end;

function TOracleConnection.ExecuteAdHocQuery(AScript: string): Boolean;
begin
  try
    FConnection.ExecSQL(AScript);
    Result := True;
  except on E: Exception do
    begin
      E.Message := ' Script: ' + AScript + #13#10 + ' Error: ' + E.Message;
      Result := False;
      raise;
    end;
  end;
end;

function TOracleConnection.ExecuteAdHocQueryWithTransaction(AScript: string): Boolean;
begin
  try
    BeginTrans;
    FConnection.ExecSQL(AScript);
    CommitTrans;
    Result := True;
  except on E: Exception do
    begin
      RollBackTrans;
      E.Message := ' Script: ' + AScript + #13#10 + ' Error: ' + E.Message;
      Result := False;
      raise;
    end;
  end;
end;

function TOracleConnection.RemoveCommentFromTSQL(const ASQLLine: string): string;
var
  LvCommentIndex: Integer;
begin
  LvCommentIndex := Pos('--', ASQLLine);
  if LvCommentIndex > 0 then
    Result := Trim(Copy(ASQLLine, 1, LvCommentIndex - 1))
  else
    Result := ASQLLine;
end;

function TOracleConnection.GetConnectionString: string;
begin
  Result := FConnection.ConnectionString;
end;

function TOracleConnection.InitializeDatabase: Boolean;
var
  LvTbScript: string;
begin
  LvTbScript := 'DECLARE' + #10
   + 'table_count INTEGER;' + #10
   + 'BEGIN' + #10
   + 'SELECT COUNT(*)' + #10
   + 'INTO table_count' + #10
   + 'FROM   all_objects' + #10
   + 'WHERE  object_name = ''EasyDBVersionInfo'' AND object_type = ''TABLE'';' + #10
   + 'IF table_count = 0 THEN' + #10
   + '   EXECUTE IMMEDIATE ''CREATE TABLE EasyDBVersionInfo (' + #10
   + '                       Version NUMBER(19) PRIMARY KEY,' + #10
   + '                       AppliedOn DATE DEFAULT SYSDATE,' + #10
   + '                       Author NVARCHAR2(100),' + #10
   + '                       Description NCLOB' + #10
   + '                       )'';' + #10
   + 'END IF;' + #10
   + 'END;';

  try
    ExecuteAdHocQuery(LvTbScript);
    Result := True;
  except on E: Exception do
    begin
      Logger.Log(atInitialize, E.Message);
      Result := False;
    end;
  end;
end;

class function TOracleConnection.Instance: TOracleConnection;
begin
  if not Assigned(FInstance) then
    FInstance := TOracleConnection.Create;

  Result := FInstance;
end;

function TOracleConnection.IsConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TOracleConnection.Logger: TLogger;
begin
  Result := TLogger.Instance;
end;

function TOracleConnection.OpenAsInteger(AScript: string): Largeint;
begin
  FQuery.Open(AScript);
  if FQuery.RecordCount > 0 then
    Result := FQuery.Fields[0].AsLargeInt
  else
    Result := -1;
end;

procedure TOracleConnection.RollBackTrans;
begin
  FConnection.Transaction.Rollback;
end;

function TOracleConnection.SetConnectionParam(AConnectionParams: TOracleConnectionParams): TOracleConnection;
begin
  FConnectionParams := AConnectionParams;
  with FConnection.Params, FConnectionParams do
  begin
    Add('Server=' + Server);
    Add('User_Name=' + UserName);
    Add('Password=' + Pass);
    Add('DriverID=Oracle');
    Add('LoginTimeout=' + LoginTimeout.ToString);
    Add('Database=' + DatabaseName);
  end;

  Result := FInstance;
end;

end.
