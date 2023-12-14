{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
unit EasyDB.ConnectionManager.Firebird;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet,

  {=Firebird=}
  FireDAC.Comp.UI, FireDAC.Stan.Consts, FireDAC.VCLUI.Controls, FireDAC.VCLUI.Error,
  FireDAC.VCLUI.Login, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
  FireDAC.Phys.IBBase, FireDAC.Phys.IBWrapper, FireDAC.Phys.FB,
  {$IF CompilerVersion >= 30}FireDAC.Phys.FBDef,{$IFEND}
  {=Firebird=}

  EasyDB.ConnectionManager.Base,
  EasyDB.Core,
  EasyDB.Logger,
  EasyDB.Consts;

 type
  TFirebirdConnection = class(TConnection) // Singletone
  private
    FConnection: TFDConnection;
    FFirebirdDriver: TFDPhysFBDriverLink;
    FQuery: TFDQuery;
    FConnectionParams: TFirebirdConnectionParams;
    Constructor Create;
    class var FInstance: TFirebirdConnection;
  public
    class function Instance: TFirebirdConnection;
    Destructor Destroy; override;

    function GetConnectionString: string; override;
    function SetConnectionParam(AConnectionParams: TFirebirdConnectionParams): TFirebirdConnection;
    function Connect: Boolean; override;
    function ConnectEx: TFirebirdConnection;
    function IsConnected: Boolean;
    function InitializeDatabase: Boolean;
    function DoesTbExist(ATable_Name: string): Boolean;
    function Logger: TLogger; override;

    procedure ExecuteAdHocQuery(AScript: string); override;
    procedure ExecuteAdHocQueryWithTransaction(AScript: string);
    procedure ExecuteScriptFile(AScriptPath: string; ADelimiter: string); override;
    function OpenAsInteger(AScript: string): Largeint;

    procedure BeginTrans;
    procedure CommitTrans;
    procedure RollBackTrans;

    property ConnectionParams: TFirebirdConnectionParams read FConnectionParams;
  end;

implementation

{ TFirebirdConnection }

procedure TFirebirdConnection.BeginTrans;
begin
  FConnection.Transaction.StartTransaction;
end;

procedure TFirebirdConnection.CommitTrans;
begin
  FConnection.Transaction.Commit;
end;

function TFirebirdConnection.Connect: Boolean;
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

function TFirebirdConnection.ConnectEx: TFirebirdConnection;
begin
  if Connect then
    Result := FInstance
  else
    Result := nil;
end;

constructor TFirebirdConnection.Create;
begin
  FConnection := TFDConnection.Create(nil);
  FFirebirdDriver := TFDPhysFBDriverLink.Create(nil);
  FConnection.DriverName := 'FB';
  FConnection.LoginPrompt := False;

  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
end;

destructor TFirebirdConnection.Destroy;
begin
  FQuery.Close;
  FQuery.Free;
  FFirebirdDriver.Free;

  FConnection.Close;
  FConnection.Free;
  inherited;
end;

procedure TFirebirdConnection.ExecuteAdHocQuery(AScript: string);
begin
  try
    FConnection.ExecSQL(AScript);
  except on E: Exception do
    begin
      E.Message := ' Script: ' + AScript + #13#10 + ' Error: ' + E.Message;
      raise;
    end;
  end;
end;

procedure TFirebirdConnection.ExecuteAdHocQueryWithTransaction(AScript: string);
begin
  try
    BeginTrans;
    FConnection.ExecSQL(AScript);
    CommitTrans;
  except on E: Exception do
    begin
      RollBackTrans;
      E.Message := ' Script: ' + AScript + #13#10 + ' Error: ' + E.Message;
      raise;
    end;
  end;
end;

procedure TFirebirdConnection.ExecuteScriptFile(AScriptPath: string; ADelimiter: string);
var
  LvStreamReader: TStreamReader;
  LvLine: string;
  LvStatement: string;
begin
  if FileExists(AScriptPath) then
  begin
    LvStreamReader := TStreamReader.Create(AScriptPath, TEncoding.UTF8);
    LvLine := EmptyStr;
    LvStatement := EmptyStr;

    try
      while not LvStreamReader.EndOfStream do
      begin
        LvLine := LvStreamReader.ReadLine;

        if not RightStr(LvLine.Trim.ToLower, Length(ADelimiter)).Equals(ADelimiter) then
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
  end
  else
    Logger.Log(atFileExecution, 'Script file doesn''t exists.');
end;

function TFirebirdConnection.GetConnectionString: string;
begin
  Result := FConnection.ConnectionString;
end;

function TFirebirdConnection.InitializeDatabase: Boolean;
var
  LvTbScript: string;
begin
  if DoesTbExist(TB.ToUpper) then
    Exit(True);

  LvTbScript := 'CREATE TABLE ' + TB.ToUpper + ' ( ' +
    '  Version BIGINT NOT NULL PRIMARY KEY, ' +
    '  AppliedOn TIMESTAMP DEFAULT CURRENT_TIMESTAMP, ' +
    '  Author VARCHAR(100), ' +
    '  Description VARCHAR(4000) ' +
    ');';

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

function TFirebirdConnection.DoesTbExist(ATable_Name: string): Boolean;
var
  LvScript: string;
  LvQuery: TFDQuery;
begin
  Result := False;
  LvScript := Format('SELECT 1 FROM rdb$relations WHERE rdb$relation_name = ''%s''', [ATable_Name.ToUpper]);
  LvQuery := TFDQuery.Create(nil);
  try
    try
      LvQuery.Connection := FConnection;
      LvQuery.SQL.Add(LvScript);
      LvQuery.Open;
      Result :=  LvQuery.Fields[0].AsInteger <> 0;
    except on E: Exception do
      Logger.Log(atInitialize, E.Message);
    end;
  finally
    LvQuery.Free;
  end;
end;

class function TFirebirdConnection.Instance: TFirebirdConnection;
begin
  if not Assigned(FInstance) then
    FInstance := TFirebirdConnection.Create;

  Result := FInstance;
end;

function TFirebirdConnection.IsConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TFirebirdConnection.Logger: TLogger;
begin
  Result := TLogger.Instance;
end;

function TFirebirdConnection.OpenAsInteger(AScript: string): Largeint;
begin
  FQuery.Open(AScript);
  if FQuery.RecordCount > 0 then
    Result := FQuery.Fields[0].AsLargeInt
  else
    Result := -1;
end;

procedure TFirebirdConnection.RollBackTrans;
begin
  FConnection.Transaction.Rollback;
end;

function TFirebirdConnection.SetConnectionParam(AConnectionParams: TFirebirdConnectionParams): TFirebirdConnection;
begin
  FConnectionParams := AConnectionParams;

  with FConnection.Params do
  begin
    Clear;
    Add('DriverID=FB');
    Add('Server=' + FConnectionParams.Host);
    Add('Database=' + FConnectionParams.Database);
    Add('User_name=' + FConnectionParams.UserName);
    Add('Password=' + FConnectionParams.Pass);
  end;

  Result := FInstance;
end;

end.
