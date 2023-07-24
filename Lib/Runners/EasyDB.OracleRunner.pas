unit EasyDB.OracleRunner;

interface

uses
  System.SysUtils, System.StrUtils,
  EasyDB.Migration,
  EasyDB.MigrationX,
  EasyDB.Runner,
  EasyDB.ConnectionManager.Oracle,
  EasyDB.Consts,
  EasyDB.Logger,
  EasyDB.Core;

type

  TOracleRunner = class(TRunner)
  private
    FDbName: string;
    FOracleConnection: TOracleConnection;

    procedure UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True); override;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); override;
    function GetDatabaseVersion: Int64; override;
  public
    constructor Create(ASQLConnection: TOracleConnection = nil); overload;
    constructor Create(AConnectionParams: TOracleConnectionParams); overload;
    destructor Destroy; override;

    property Oracle: TOracleConnection read FOracleConnection write FOracleConnection;
    property DbName: string read FDbName write FDbName;
  end;

implementation

constructor TOracleRunner.Create(ASQLConnection: TOracleConnection = nil);
begin
  inherited Create;
  if Assigned(ASQLConnection) then
    FOracleConnection:= ASQLConnection
  else
    FOracleConnection:= TOracleConnection.Instance.SetConnectionParam(TOracleConnection.Instance.ConnectionParams).ConnectEx;
end;

constructor TOracleRunner.Create(AConnectionParams: TOracleConnectionParams);
begin
  inherited Create;
  FOracleConnection:= TOracleConnection.Instance.SetConnectionParam(AConnectionParams).ConnectEx;
  FOracleConnection.ParentRunner := Self;
  FDbName := AConnectionParams.DatabaseName;
end;

destructor TOracleRunner.Destroy;
begin
  FOracleConnection.Free;
  FOracleConnection := nil;
  inherited;
end;

function TOracleRunner.GetDatabaseVersion: Int64;
begin
  if not FDbName.ToLower.Trim.Equals('master') then
  begin
    if FOracleConnection.IsConnected then
      Result := FOracleConnection.OpenAsInteger('Select max(Version) from ' + TB)
    else
      Result := -1;
  end
  else
    Result := -1;
end;

procedure TOracleRunner.UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True);
var
  LvScript: string;
  LvLatestVersion: Int64;
  LvAuthor:string;
  LvDescription: string;
begin
  if FDbName.ToLower.Trim.Equals('master') then
    Exit;

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
    LvScript := 'INSERT INTO ' + TB + ' ' + #10
       + '( ' + #10
       + '	Version, ' + #10
       + '	AppliedOn, ' + #10
       + '	Author, ' + #10
       + '	"Description" ' + #10
       + ') ' + #10
       + 'VALUES ' + #10
       + '( ' + #10
       + '	' + LvLatestVersion.ToString + ', ' + #10
       + '	(SYSDATE), ' + #10
       + '	' + LvAuthor.QuotedString + ', ' + #10
       + '	' + LvDescription.QuotedString + ' ' + #10
       + ')';
  end
  else
  begin
    LvScript :=
       'UPDATE ' + TB + ' ' + #10
       + 'SET ' + #10
       + ' AppliedOn = (SYSDATE) ' + #10
       + ' ,Author = Author || ' + QuotedStr(' -- ') + ' || ' + LvAuthor.QuotedString + #10
       + ' ,"Description" = "Description" || ' + QuotedStr(' -- ') + ' || ' + LvDescription.QuotedString + #10
       + ' Where Version = ' + LvLatestVersion.ToString;
  end;

  FOracleConnection.ExecuteAdHocQuery(LvScript);
end;

procedure TOracleRunner.DownGradeVersionInfo(AVersionToDownGrade: Int64);
var
  LvScript: string;
begin
  if FDbName.ToLower.Trim.Equals('master') then
    Exit;

  LvScript := 'Delete from ' + TB + ' Where Version > ' + AVersionToDownGrade.ToString;
  FOracleConnection.ExecuteAdHocQuery(LvScript);
end;

end.
