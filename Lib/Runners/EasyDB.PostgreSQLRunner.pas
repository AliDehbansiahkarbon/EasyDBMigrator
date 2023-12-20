{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
unit EasyDB.PostgreSQLRunner;

interface

uses
  System.SysUtils, System.StrUtils,
  EasyDB.Core,
  EasyDB.Consts,
  EasyDB.Migration,
  EasyDB.MigrationX,
  EasyDB.Runner,
  EasyDB.Logger,
  EasyDB.ConnectionManager.PostgreSQL;

type
  TPgRunner = class(TRunner)
  private
    FDbName: string;
    FSchema: string;
    FPgConnection: TPgConnection;
  protected
    procedure UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True); override;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); override;
    function GetDatabaseVersion: Int64; override;
  public
    constructor Create(AConnectionParams: TPgConnectionParams; ALoggerEventHandler: TLoggerEventHandler = nil); overload;
    constructor Create(AConnectionParams: TPgConnectionParams; ALocalLogFile: string); overload;
    destructor Destroy; override;

    property PG: TPgConnection read FPgConnection write FPgConnection;
    property DbName: string read FDbName write FDbName;
    property Schema: string read FSchema write FSchema;
  end;

implementation

{ TPgRunner }

constructor TPgRunner.Create(AConnectionParams: TPgConnectionParams; ALoggerEventHandler: TLoggerEventHandler);
begin
  inherited Create;
  if Assigned(ALoggerEventHandler) then
    GetLogger.OnLog := ALoggerEventHandler;

  FPgConnection:= TPgConnection.Instance.SetConnectionParam(AConnectionParams).ConnectEx;
  if not Assigned(FPgConnection) then
  begin
    if (not Assigned(ALoggerEventHandler)) and (not Assigned(TLogger.Instance.OnLog)) then
      raise Exception.Create(NoConnectionMsg);
  end;

  FDbName := AConnectionParams.DatabaseName;
  FSchema := AConnectionParams.Schema;
end;

constructor TPgRunner.Create(AConnectionParams: TPgConnectionParams; ALocalLogFile: string);
begin
  inherited Create;
  GetLogger.ConfigLocal(True, ALocalLogFile);

  FPgConnection:= TPgConnection.Instance.SetConnectionParam(AConnectionParams).ConnectEx;
  if not Assigned(FPgConnection) then
  begin
    if not FileExists(ALocalLogFile) then
      raise Exception.Create(NoConnectionMsg);
  end;

  FDbName := AConnectionParams.DatabaseName;
  FSchema := AConnectionParams.Schema;
end;

destructor TPgRunner.Destroy;
begin
  if Assigned(FPgConnection) then
    FPgConnection.Free;
  inherited;
end;

procedure TPgRunner.DownGradeVersionInfo(AVersionToDownGrade: Int64);
var
  LvScript: string;
begin
  LvScript := 'Delete from ' + TB + ' Where Version > ' + AVersionToDownGrade.ToString;
  FPgConnection.ExecuteAdHocQuery(LvScript);
end;

function TPgRunner.GetDatabaseVersion: Int64;
begin
  if FPgConnection.IsConnected then
    Result := FPgConnection.OpenAsInteger('Select max(Version) from ' + TB)
  else
    Result := -1;
end;

procedure TPgRunner.UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean);
var
  LvScript: string;
  LvLatestVersion: Int64;
  LvAuthor:string;
  LvDescription: string;
begin
  if AMigration is TMigration then
  begin
    LvLatestVersion := TMigration(AMigration).Version;
    LvAuthor := TMigration(AMigration).Author;
    LvDescription := TMigration(AMigration).Description;
  end
  else if AMigration is TMigrationX then
  begin
    LvLatestVersion := TMigrationX(AMigration).AttribVersion;
    LvAuthor := TMigrationX(AMigration).AttribAuthor;
    LvDescription := TMigrationX(AMigration).AttribDescription;
  end;

  if AInsertMode then
  begin
    LvScript :=
    IfThen(((not FDbName.IsEmpty) and (not FSchema.IsEmpty)),
    'INSERT INTO ' + FSchema + '.' + FDbName + '.' + 'easydbversioninfo',
    'INSERT INTO ' + FDbName + '.' + 'easydbversioninfo')
    + '(' + #10
    + '    version,' + #10
    + '    appliedon,' + #10
    + '    author,' + #10
    + '    description' + #10
    + ')' + #10
    + 'VALUES' + #10
    + '(' + #10
    + ' ' + LvLatestVersion.ToString + ',' + #10
    + '    NOW(),' + #10
    + '	' + LvAuthor.QuotedString + ', ' + #10
    + '	' + LvDescription.QuotedString + ' ' + #10
    + ')';
  end
  else
  begin
    LvScript :=
    'UPDATE library.easydbversioninfo' + #10
    + 'SET    appliedon       = NOW(),' + #10
    + '       author          = CONCAT(author, '' -- '', ' + LvAuthor.QuotedString + '),' + #10
    + '       description     = CONCAT(description, '' -- '', ' + LvDescription.QuotedString + ')' + #10
    + 'WHERE  version         = 202301010003;';
  end;

  FPgConnection.ExecuteAdHocQuery(LvScript);
end;

end.
