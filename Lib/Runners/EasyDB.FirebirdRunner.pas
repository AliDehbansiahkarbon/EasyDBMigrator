unit EasyDB.FirebirdRunner;

interface

uses
  System.SysUtils, System.StrUtils,
  EasyDB.Core,
  EasyDB.Consts,
  EasyDB.Migration,
  EasyDB.MigrationX,
  EasyDB.Runner,
  EasyDB.ConnectionManager.Firebird;

type
  TFirebirdRunner = class(TRunner)
  private
    FDatabase: string;
    FirebirdConnection: TFirebirdConnection;
  protected
    procedure UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True); override;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); override;
    function GetDatabaseVersion: Int64; override;
  public
    constructor Create(AConnectionParams: TFirebirdConnectionParams); overload;
    destructor Destroy; override;
    property Firebird: TFirebirdConnection read FirebirdConnection write FirebirdConnection;
    property Database: string read FDatabase write FDatabase;
  end;

implementation

{ TFirebirdRunner }

constructor TFirebirdRunner.Create(AConnectionParams: TFirebirdConnectionParams);
begin
  inherited Create;
  FDatabase := AConnectionParams.Database;
  FirebirdConnection:= TFirebirdConnection.Instance.SetConnectionParam(AConnectionParams).ConnectEx;
end;

destructor TFirebirdRunner.Destroy;
begin
  FirebirdConnection.Free;
  inherited;
end;

procedure TFirebirdRunner.DownGradeVersionInfo(AVersionToDownGrade: Int64);
var
  LvScript: string;
begin
  LvScript := 'Delete from ' + TB + ' Where Version > ' + AVersionToDownGrade.ToString;
  FirebirdConnection.ExecuteAdHocQuery(LvScript);
end;

function TFirebirdRunner.GetDatabaseVersion: Int64;
begin
  if FirebirdConnection.IsConnected then
    Result := FirebirdConnection.OpenAsInteger('Select max(Version) from ' + TB)
  else
    Result := -1;
end;

procedure TFirebirdRunner.UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean);
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
    LvScript := 'INSERT INTO ' + TB + #10
    + '(VERSION, APPLIEDON, AUTHOR, DESCRIPTION)'
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
    'UPDATE ' + TB + #10
    + 'SET AppliedOn = CURRENT_TIMESTAMP' + #10
    + ',Author = Author || ' + QuotedStr(' -- ') + ' || ' + LvAuthor.QuotedString + #10
    + ',Description = Description || ' + QuotedStr(' -- ') + LvDescription.QuotedString + #10
    + 'WHERE Version = ' + LvLatestVersion.ToString + ';';
  end;

  FirebirdConnection.ExecuteAdHocQuery(LvScript);
end;

end.
