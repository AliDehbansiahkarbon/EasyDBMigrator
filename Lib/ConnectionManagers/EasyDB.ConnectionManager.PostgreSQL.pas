unit EasyDB.ConnectionManager.PostgreSQL;

interface

uses
  System.SysUtils, System.Classes,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, {=PostgreSQL=} FireDAC.Phys.PGDef, FireDAC.Phys.PG, {=PostgreSQL=}

  EasyDB.ConnectionManager.Base,
  EasyDB.Core,
  EasyDB.Logger,
  EasyDB.Consts;

 type

  TPgConnection = class(TConnection) // Singletone
  private
    FConnection: TFDConnection;
    FPgDriver: TFDPhysPgDriverLink;
    FQuery: TFDQuery;
    FConnectionParams: TPgConnectionParams;
    Constructor Create;
    class var FInstance: TPgConnection;
  public
    class function Instance: TPgConnection;
    Destructor Destroy; override;

    function GetConnectionString: string; override;
    function SetConnectionParam(AConnectionParams: TPgConnectionParams): TPgConnection;
    function Connect: Boolean; override;
    function ConnectEx: TPgConnection;
    function IsConnected: Boolean;
    function InitializeDatabase: Boolean;
    function Logger: TLogger; override;

    function ExecuteAdHocQuery(AScript: string): Boolean; override;
    function ExecuteAdHocQueryWithTransaction(AScript: string): Boolean;
    function ExecuteScriptFile(AScriptPath: string; ADelimiter: string): Boolean; override;
    function OpenAsInteger(AScript: string): Largeint;

    procedure BeginTrans;
    procedure CommitTrans;
    procedure RollBackTrans;

    property ConnectionParams: TPgConnectionParams read FConnectionParams;
  end;

implementation

{ TPgConnection }

procedure TPgConnection.BeginTrans;
begin
  FConnection.Transaction.StartTransaction;
end;

procedure TPgConnection.CommitTrans;
begin
  FConnection.Transaction.Commit;
end;

function TPgConnection.Connect: Boolean;
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

function TPgConnection.ConnectEx: TPgConnection;
begin
  if Connect then
    Result := FInstance
  else
    Result := nil;
end;

constructor TPgConnection.Create;
begin
  FConnection := TFDConnection.Create(nil);
  FPgDriver := TFDPhysPgDriverLink.Create(nil);
  FPgDriver.VendorHome := '.';
  FPgDriver.VendorLib := 'libpq.dll';

  FConnection.DriverName := 'PG';
  FConnection.LoginPrompt := False;

  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
end;

destructor TPgConnection.Destroy;
begin
  FQuery.Close;
  FQuery.Free;
  FPgDriver.Free;

  FConnection.Close;
  FConnection.Free;
  inherited;
end;

function TPgConnection.ExecuteAdHocQuery(AScript: string): Boolean;
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

function TPgConnection.ExecuteAdHocQueryWithTransaction(AScript: string): Boolean;
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

function TPgConnection.ExecuteScriptFile(AScriptPath: string; ADelimiter: string): Boolean;
var
  LvStreamReader: TStreamReader;
  LvLine: string;
  LvStatement: string;
begin
  if FileExists(AScriptPath) then
  begin
    Result := True;
    LvStreamReader := TStreamReader.Create(AScriptPath, TEncoding.UTF8);
    LvLine := EmptyStr;
    LvStatement := EmptyStr;

    try
      while not LvStreamReader.EndOfStream do
      begin
        LvLine := LvStreamReader.ReadLine;
        if not LvLine.Trim.ToLower.Equals(ADelimiter) then
          LvStatement := LvStatement + ' ' + LvLine
        else
        begin
          if not LvStatement.Trim.IsEmpty then
          try
            ExecuteAdHocQuery(LvStatement);
          finally
            LvStatement := EmptyStr;
          end;
        end;
      end;
    finally
      LvStreamReader.Free;
    end;
    Result := True;
  end
  else
  begin
    Logger.Log(atFileExecution, 'Script file doesn''t exists.');
    Result := False;
  end;
end;

function TPgConnection.GetConnectionString: string;
begin
  Result := FConnection.ConnectionString;
end;

function TPgConnection.InitializeDatabase: Boolean;
var
  LvTbScript: string;
begin
  LvTbScript := 'CREATE TABLE IF NOT EXISTS EasyDBVersionInfo' + #10
                + '(' + #10
                + 'Version     BIGINT NOT NULL PRIMARY KEY,' + #10
                + 'AppliedOn   TIMESTAMP DEFAULT CURRENT_TIMESTAMP,' + #10
                + 'Author      VARCHAR(100),' + #10
                + 'Description TEXT' + #10
                + ');';
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

class function TPgConnection.Instance: TPgConnection;
begin
  if not Assigned(FInstance) then
    FInstance := TPgConnection.Create;

  Result := FInstance;
end;

function TPgConnection.IsConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TPgConnection.Logger: TLogger;
begin
  Result := TLogger.Instance;
end;

function TPgConnection.OpenAsInteger(AScript: string): Largeint;
begin
  FQuery.Open(AScript);
  if FQuery.RecordCount > 0 then
    Result := FQuery.Fields[0].AsLargeInt
  else
    Result := -1;
end;

procedure TPgConnection.RollBackTrans;
begin
  FConnection.Transaction.Rollback;
end;

function TPgConnection.SetConnectionParam(AConnectionParams: TPgConnectionParams): TPgConnection;
begin
  FConnectionParams := AConnectionParams;

  with FConnection.Params, FConnectionParams do
  begin
    Clear;
    Add('DriverID=PG');
    Add('Server=' + Server);
    Add('Port=' + Port.ToString);
    Add('Database=' + DatabaseName);
    Add('User_name=' + UserName);
    Add('Password=' + Pass);
    Add('LoginTimeout=' + LoginTimeout.ToString);
  end;

  Result := FInstance;
end;

end.
