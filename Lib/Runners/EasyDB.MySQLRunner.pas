{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
unit EasyDB.MySQLRunner;

interface

uses
  System.SysUtils, System.StrUtils,
  EasyDB.Core,
  EasyDB.Consts,
  EasyDB.Migration,
  EasyDB.MigrationX,
  EasyDB.Runner,
  EasyDB.Logger,
  EasyDB.ConnectionManager.MySQL;

type
  TMySQLRunner = class(TRunner)
  private
    FSchema: string;
    FMySQLConnection: TMySQLConnection;
  protected
    procedure UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True); override;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); override;
    function GetDatabaseVersion: Int64; override;
  public
    constructor Create(AConnectionParams: TMySqlConnectionParams; ALoggerEventHandler: TLoggerEventHandler = nil); overload;
    constructor Create(AConnectionParams: TMySqlConnectionParams; ALocalLogFile: string); overload;
    destructor Destroy; override;

    property MySQL: TMySQLConnection read FMySQLConnection write FMySQLConnection;
    property Schema: string read FSchema write FSchema;
  end;

implementation

{ TMySQLRunner }

constructor TMySQLRunner.Create(AConnectionParams: TMySqlConnectionParams; ALoggerEventHandler: TLoggerEventHandler);
begin
  inherited Create;
  if Assigned(ALoggerEventHandler) then
    GetLogger.OnLog := ALoggerEventHandler;

  FMySQLConnection:= TMySQLConnection.Instance.SetConnectionParam(AConnectionParams).ConnectEx;
  if not Assigned(FMySQLConnection) then
  begin
    if (not Assigned(ALoggerEventHandler)) and (not Assigned(TLogger.Instance.OnLog)) then
      raise Exception.Create(NoConnectionMsg);
  end;

  FSchema := AConnectionParams.Schema;
end;

constructor TMySQLRunner.Create(AConnectionParams: TMySqlConnectionParams; ALocalLogFile: string);
begin
  inherited Create;
  GetLogger.ConfigLocal(True, ALocalLogFile);

  FMySQLConnection:= TMySQLConnection.Instance.SetConnectionParam(AConnectionParams).ConnectEx;
  if not Assigned(FMySQLConnection) then
    raise Exception.Create(NoConnectionMsg);

  FSchema := AConnectionParams.Schema;
end;

destructor TMySQLRunner.Destroy;
begin
  if Assigned(FMySQLConnection) then
    FMySQLConnection.Free;
  inherited;
end;

procedure TMySQLRunner.DownGradeVersionInfo(AVersionToDownGrade: Int64);
var
  LvScript: string;
begin
  LvScript := 'Delete from ' + TB + ' Where Version > ' + AVersionToDownGrade.ToString;
  FMySQLConnection.ExecuteAdHocQuery(LvScript);
end;

function TMySQLRunner.GetDatabaseVersion: Int64;
begin
  if FMySQLConnection.IsConnected then
    Result := FMySQLConnection.OpenAsInteger('Select max(Version) from ' + TB)
  else
    Result := -1;
end;

procedure TMySQLRunner.UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean);
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
    LvScript := 'INSERT INTO `' + FSchema + '`.`' + TB + '`' + #10
    + '(`Version`,' + #10
    + '`AppliedOn`,' + #10
    + '`Author`,' + #10
    + '`Description`)' + #10
    + 'VALUES' + #10
    + '(' + LvLatestVersion.ToString + ',' + #10
    + 'CURRENT_TIMESTAMP,' + #10
    + '	' +  LvAuthor.QuotedString + ',' + #10
    + '	' + LvDescription.QuotedString + ' ' + #10
    + ');';
  end
  else
  begin
    LvScript :=
    'UPDATE `' + FSchema + '`.`' + TB + '`' + #10
    + 'SET' + #10
    + '`Version` = ' + LvLatestVersion.ToString + #10
    + ',`AppliedOn` = CURRENT_TIMESTAMP' + #10
    + ',`Author` = CONCAT(`Author`,' + QuotedStr(' -- ') + ', ' + LvAuthor.QuotedString + ')' + #10
    + ',`Description` = CONCAT(`Description`,' + QuotedStr(' -- ') + ', ' + LvDescription.QuotedString + ')' + #10
    + 'WHERE `Version` = ' + LvLatestVersion.ToString + ';';
  end;

  FMySQLConnection.ExecuteAdHocQuery(LvScript);
end;

end.
