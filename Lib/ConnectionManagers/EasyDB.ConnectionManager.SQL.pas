unit EasyDB.ConnectionManager.SQL;

interface

uses
  System.SysUtils, System.Classes, System.StrUtils, {$IF CompilerVersion >= 27}System.Threading, {$IFEND}
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet,
  {==MSSQL==} {$IF CompilerVersion >= 30}FireDAC.Phys.MSSQLDef, {$IFEND} FireDAC.Phys.ODBCBase, FireDAC.Phys.MSSQL,{==MSSQL==}

  EasyDB.ConnectionManager.Base,
  EasyDB.Core,
  EasyDB.Logger,
  EasyDB.Consts,
  EasyDB.Runner;

type

  TSQLConnection = class(TConnection) // Singletone
  private
    FConnection: TFDConnection;
    FMSSQLDriver: TFDPhysMSSQLDriverLink;
    FQuery: TFDQuery;
    FConnectionParams: TSqlConnectionParams;
    FParentRunner: TRunner;
    Constructor Create;
    class var FInstance: TSQLConnection;
  public
    class function Instance: TSQLConnection;
    Destructor Destroy; override;

    function GetConnectionString: string; override;
    function SetConnectionParam(AConnectionParams: TSqlConnectionParams): TSQLConnection;
    function Connect: Boolean; override;
    function ConnectEx: TSQLConnection;
    function IsConnected: Boolean;
    function InitializeDatabase: Boolean;
    function Logger: TLogger; override;
    function RemoveCommentFromTSQL(const ASQLLine: string): string;
    function OpenAsInteger(AScript: string): Largeint;

    procedure ExecuteAdHocQuery(AScript: string); override;
    procedure ExecuteAdHocQueryWithTransaction(AScript: string);
    procedure ExecuteScriptFile(AScriptPath: string; ADelimiter: string); override;
    procedure BeginTrans;
    procedure CommitTrans;
    procedure RollBackTrans;

    property ConnectionParams: TSqlConnectionParams read FConnectionParams;
    property ParentRunner: TRunner read FParentRunner write FParentRunner;
  end;

implementation

{ TSQLConnection }

function TSQLConnection.ConnectEx: TSQLConnection;
begin
  if Connect then
    Result := FInstance
  else
    Result := nil;
end;

constructor TSQLConnection.Create;
begin
  FConnection := TFDConnection.Create(nil);
  FMSSQLDriver := TFDPhysMSSQLDriverLink.Create(nil);

  FConnection.DriverName := 'MSSQL';
  FConnection.LoginPrompt := False;

  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;

  FParentRunner := nil;
end;

destructor TSQLConnection.Destroy;
begin
  FQuery.Close;
  FQuery.Free;
  FMSSQLDriver.Free;

  FConnection.Close;
  FConnection.Free;

  FInstance := nil;
  inherited;
end;

procedure TSQLConnection.BeginTrans;
begin
  FConnection.Transaction.StartTransaction;
end;

function TSQLConnection.Connect: Boolean;
begin
  try
    FConnection.Connected := True;
    if not FConnectionParams.DatabaseName.ToLower.Trim.Equals('master') then
      InitializeDatabase;

    Result := True;
  except on E: Exception do
    begin
      Logger.Log(atDbConnection, E.Message);
      Result := False;
    end;
  end;
end;

procedure TSQLConnection.CommitTrans;
begin
  FConnection.Transaction.Commit;
end;

procedure TSQLConnection.ExecuteAdHocQuery(AScript: string);
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

procedure TSQLConnection.ExecuteAdHocQueryWithTransaction(AScript: string);
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

procedure TSQLConnection.ExecuteScriptFile(AScriptPath: string; ADelimiter: string);
var
  LvStreamReader: TStreamReader;
  LvLine: string;
  LvStatement: string;
  LvLineNumber: Integer;
  {$IF CompilerVersion >= 30}
  LvTask: ITask;
  {$ELSE}
  LvThread: TThread;
  {$ENDIF}
  LvLogExecutions: Boolean;
begin
  if Assigned(FParentRunner) and Assigned(FParentRunner.Config) then
    LvLogExecutions := FParentRunner.Config.LogAllExecutionsStat
  else
    LvLogExecutions := False;

  if FileExists(AScriptPath) then
  begin
    {$IF CompilerVersion >= 30}
    LvTask := TTask.Run(
    {$ELSE}
    LvThread := TThread.CreateAnonymousThread(
    {$ENDIF}
    procedure
    begin
      LvLineNumber := 1;
      LvStreamReader := TStreamReader.Create(AScriptPath, TEncoding.UTF8);
      LvLine := EmptyStr;
      LvStatement := EmptyStr;
      try
        while not LvStreamReader.EndOfStream do
        begin
          LvLine := LvStreamReader.ReadLine;
          if not LvLine.Trim.ToLower.Equals(ADelimiter.Trim.ToLower) then
          begin
            if not ((LeftStr(LvLine.Trim, 2) = '/*') or (RightStr(LvLine.Trim, 2) = '*/') or (LeftStr(LvLine.Trim, 2) = '--')) then
              LvStatement := LvStatement + ' ' + RemoveCommentFromTSQL(LvLine)
          end
          else
          begin
            if not LvStatement.Trim.IsEmpty then
            begin
              try
                try
                  if LvLogExecutions then
                    Logger.Log(atFileExecution, 'Line: ' + LvLineNumber.ToString + ' successfully executed');

                  ExecuteAdHocQuery(LvStatement);
                except on E: Exception  do
                  Logger.Log(atFileExecution, 'Error on Line: ' + LvLineNumber.ToString + #13 + E.Message);
                end;
              finally
                LvStatement := EmptyStr;
              end;
            end;
          end;
          Inc(LvLineNumber);
        end;
        Logger.Log(atFileExecution, 'Done!');
      finally
        LvStreamReader.Free;
      end;
    end);
    {$IF CompilerVersion < 30}
    LvThread.FreeOnTerminate := True;
    LvThread.Start;
    {$ENDIF}
  end
  else
    Logger.Log(atFileExecution, 'Script file doesn''t exists.');
end;

function TSQLConnection.RemoveCommentFromTSQL(const ASQLLine: string): string;
var
  LvCommentIndex: Integer;
begin
  LvCommentIndex := Pos('--', ASQLLine);
  if LvCommentIndex > 0 then
    Result := Trim(Copy(ASQLLine, 1, LvCommentIndex - 1))
  else
    Result := ASQLLine;
end;

function TSQLConnection.GetConnectionString: string;
begin
  Result := FConnection.ConnectionString;
end;

function TSQLConnection.InitializeDatabase: Boolean;
var
  LvTbScript: string;
begin
  LvTbScript := 'If Not Exists ( ' + #10
    + '       Select * ' + #10
    + '       From   sysobjects ' + #10
    + '       Where  Name          = ' + TB.QuotedString + ' ' + #10
    + '              And xtype     = ''U'' ' + #10
    + '   ) ' + #10
    + '    Create Table ' + TB + ' ' + #10
    + '    ( ' + #10
    + '    	Version Bigint Not null Primary Key, ' + #10
    + '    	AppliedOn Datetime Default(Getdate()), ' + #10
    + '    	Author Nvarchar(100), ' + #10
    + '    	Description Nvarchar(Max) ' + #10
    + '    	 ' + #10
    + '    )';

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

class function TSQLConnection.Instance: TSQLConnection;
begin
  if not Assigned(FInstance) then
    FInstance := TSQLConnection.Create;

  Result := FInstance;
end;

function TSQLConnection.IsConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

function TSQLConnection.Logger: TLogger;
begin
  Result := TLogger.Instance;
end;

function TSQLConnection.OpenAsInteger(AScript: string): Largeint;
begin
  FQuery.Open(AScript);
  if FQuery.RecordCount > 0 then
    Result := FQuery.Fields[0].AsLargeInt
  else
    Result := -1;
end;

procedure TSQLConnection.RollBackTrans;
begin
  FConnection.Transaction.Rollback;
end;

function TSQLConnection.SetConnectionParam(AConnectionParams: TSqlConnectionParams): TSQLConnection;
begin
  FConnectionParams := AConnectionParams;
  with FConnection.Params, FConnectionParams do
  begin
    Add('Server=' + Server);
    Add('User_Name=' + UserName);
    Add('Password=' + Pass);
    Add('DriverID=MSSQL');
    Add('LoginTimeout=' + LoginTimeout.ToString);
    Add('Database=' + DatabaseName);
  end;

  Result := FInstance;
end;

end.
