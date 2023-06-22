unit EasyDB.ConnectionManager.SQL;

interface

uses
  System.SysUtils,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet,{==MSSQL==} FireDAC.Phys.MSSQLDef, FireDAC.Phys.ODBCBase, FireDAC.Phys.MSSQL,{==MSSQL==}

  EasyDB.ConnectionManager.Base,
  EasyDB.Logger,
  EasyDB.Consts;
type
  TConnectionParams = record
    Server: string;
    LoginTimeout: Integer;
    UserName: string;
    Pass: string;
    DatabaseName: string;
    Schema: string;
  end;


  TSQLConnection = class(TConnection) // Singletone
  private
    FConnection: TFDConnection;
    FMSSQLDriver: TFDPhysMSSQLDriverLink;
    FQuery: TFDQuery;
    FServer: string;
    FUserName: string;
    FPass: string;
    FConnectionParams: TConnectionParams;
    Constructor Create;
    class var FInstance: TSQLConnection;
  public
    class function Instance: TSQLConnection;
    Destructor Destroy; override;

    function GetConnectionString: string; override;
    function SetConnectionParam(AConnectionParams: TConnectionParams): TSQLConnection;
    function Connect: Boolean; override;
    function ConnectEx: TSQLConnection;
    function IsConnected: Boolean;
    function InitializeDatabase: Boolean;
    function ExecuteAdHocQuery(AScript: string): Boolean; override;
    function OpenAsInteger(AScript: string): Largeint;
    function Logger: TLogger;

    property ConnectionParams: TConnectionParams read FConnectionParams;

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
end;

destructor TSQLConnection.Destroy;
begin
  FQuery.Close;
  FQuery.Free;
  FMSSQLDriver.Free;

  FConnection.Close;
  FConnection.Free;
  inherited;
end;

function TSQLConnection.Connect: Boolean;
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

function TSQLConnection.ExecuteAdHocQuery(AScript: string): Boolean;
begin
  try
    FConnection.ExecSQL(AScript);
    Result := True;
  except on E: Exception do
    begin
      Logger.Log(atQueryExecution, ' Script: ' + AScript + #13#10 + ' Error: ' + E.Message);
      Result := True;
    end;
  end;
end;

function TSQLConnection.GetConnectionString: string;
begin
  Result := FConnection.ConnectionString;
end;

function TSQLConnection.InitializeDatabase: Boolean;
var
  LvTbScript, LvDropScript, LvSpScript: string;
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

  LvDropScript := 'If Exists ( ' + #10  //TODO this should convert to script
       + '       Select type_desc, ' + #10
       + '              Type ' + #10
       + '       From   sys.procedures With(Nolock) ' + #10
       + '       Where  Name = ''EasyDBInsert'' ' + #10
       + '              And Type = ''P'' ' + #10
       + '   ) ' + #10
       + '    Drop Procedure EasyDBInsert ';

  LvSpScript := 'Create Procedure EasyDBInsert ' + #10
       + '	@Version Bigint, ' + #10
       + '	@Author Nvarchar(100), ' + #10
       + '	@Description Nvarchar(Max) ' + #10
       + 'As ' + #10
       + 'Begin ' + #10
       + '	If Not Exists( ' + #10
       + '	       Select 1 ' + #10
       + '	       From  ' + TB + ' ' + #10
       + '	       Where  Version = @Version ' + #10
       + '	   ) ' + #10
       + '	Begin ' + #10
       + '	    Insert Into ' + TB + ' ' + #10
       + '	      ( ' + #10
       + '	        Version, ' + #10
       + '	        AppliedOn, ' + #10
       + '	        Author, ' + #10
       + '	        [Description] ' + #10
       + '	      ) ' + #10
       + '	    Values ' + #10
       + '	      ( ' + #10
       + '	        @Version, ' + #10
       + '	        (Getdate()), ' + #10
       + '	        Null, ' + #10
       + '	        Null ' + #10
       + '	      ) ' + #10
       + '	End ' + #10
       + 'End; ';

  try
    ExecuteAdHocQuery(LvTbScript);
    ExecuteAdHocQuery(LvDropScript);
    ExecuteAdHocQuery(LvSpScript);
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

function TSQLConnection.SetConnectionParam(AConnectionParams: TConnectionParams): TSQLConnection;
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
