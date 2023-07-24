unit EasyDB.MariaDBRunner;

interface

uses
  System.SysUtils, System.StrUtils,

  EasyDB.Core,
  EasyDB.Consts,
  EasyDB.Migration,
  EasyDB.MigrationX,
  EasyDB.Runner,
  EasyDB.ConnectionManager.MariaDB;

type
  TMariaDBRunner = class(TRunner)
  private
    FSchema: string;
    FMariaDBConnection: TMariaDBConnection;

    procedure UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True); override;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); override;
    function GetDatabaseVersion: Int64; override;
  public
    constructor Create(AMariaDBConnection: TMariaDBConnection = nil); overload;
    constructor Create(AConnectionParams: TMariaDBConnectionParams); overload;
    destructor Destroy; override;

    property MySQL: TMariaDBConnection read FMariaDBConnection write FMariaDBConnection;
    property Schema: string read FSchema write FSchema;
  end;

implementation

{ TMariaDBRunner }

constructor TMariaDBRunner.Create(AMariaDBConnection: TMariaDBConnection);
begin
  inherited Create;
  if Assigned(AMariaDBConnection) then
    FMariaDBConnection:= AMariaDBConnection
  else
    FMariaDBConnection:= TMariaDBConnection.Instance.SetConnectionParam(TMariaDBConnection.Instance.ConnectionParams).ConnectEx;
end;

constructor TMariaDBRunner.Create(AConnectionParams: TMariaDBConnectionParams);
begin
  inherited Create;
  FMariaDBConnection:= TMariaDBConnection.Instance.SetConnectionParam(AConnectionParams).ConnectEx;
  FSchema := AConnectionParams.Schema;
end;

destructor TMariaDBRunner.Destroy;
begin
  FMariaDBConnection.Free;
  inherited;
end;

procedure TMariaDBRunner.DownGradeVersionInfo(AVersionToDownGrade: Int64);
var
  LvScript: string;
begin
  LvScript := 'Delete from ' + TB + ' Where Version > ' + AVersionToDownGrade.ToString;
  FMariaDBConnection.ExecuteAdHocQuery(LvScript);
end;

function TMariaDBRunner.GetDatabaseVersion: Int64;
begin
  if FMariaDBConnection.IsConnected then
    Result := FMariaDBConnection.OpenAsInteger('Select max(Version) from ' + TB)
  else
    Result := -1;
end;

procedure TMariaDBRunner.UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean);
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

  FMariaDBConnection.ExecuteAdHocQuery(LvScript);
end;

end.
