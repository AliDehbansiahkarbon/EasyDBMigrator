unit EasyDB.MSSQLRunner;

interface

uses
  System.SysUtils, System.StrUtils,
  EasyDB.Migration.Base,
  EasyDB.Migration.Attrib,
  EasyDB.Runner,
  EasyDB.ConnectionManager.SQL,
  EasyDB.Consts,
  EasyDB.Logger;

type

  TSQLRunner = class(TRunner)
  private
    FDbName: string;
    FSchema: string;
    FSQLConnection: TSQLConnection;
    procedure UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True); override;
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64); override;
    function GetDatabaseVersion: Int64; override;
  public
    constructor Create(ASQLConnection: TSQLConnection = nil); overload;
    constructor Create(ConnectionParams: TConnectionParams; AUseTransaction: Boolean = False); overload;
    destructor Destroy; override;

    property SQLConnection: TSQLConnection read FSQLConnection write FSQLConnection;
    property DbName: string read FDbName write FDbName;
    property Schema: string read FSchema write FSchema;
  end;

implementation

constructor TSQLRunner.Create(ASQLConnection: TSQLConnection = nil);
begin
  inherited Create;
  if Assigned(ASQLConnection) then
    FSQLConnection:= ASQLConnection
  else
    FSQLConnection:= TSQLConnection.Instance.SetConnectionParam(TSQLConnection.Instance.ConnectionParams).ConnectEx;
end;

constructor TSQLRunner.Create(ConnectionParams: TConnectionParams; AUseTransaction: Boolean = False);
begin
  inherited Create;
  FSQLConnection:= TSQLConnection.Instance.SetConnectionParam(ConnectionParams).ConnectEx;
  FSQLConnection.UseTransaction := AUseTransaction;
  FDbName := ConnectionParams.DatabaseName;
  FSchema := ConnectionParams.Schema;
end;

destructor TSQLRunner.Destroy;
begin
  FSQLConnection.Free;
  inherited;
end;

function TSQLRunner.GetDatabaseVersion: Int64;
begin
  if FSQLConnection.IsConnected then
    Result := FSQLConnection.OpenAsInteger('Select max(Version) from ' + TB)
  else
    Result := -1;
end;

procedure TSQLRunner.UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True);
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
  else if AMigration is TMigrationEx then
  begin
    LvLatestVersion := TMigrationEx(AMigration).AttribVersion;
    LvAuthor := TMigrationEx(AMigration).AttribAuthor;
    LvDescription := TMigrationEx(AMigration).AttribDescription;
  end;

  if AInsertMode then
  begin
    LvScript := IfThen(((not FDbName.IsEmpty) and (not FSchema.IsEmpty)),
       'INSERT INTO [' + FDbName + '].[' + FSchema + '].[' + TB + '] ' + #10,
       'INSERT INTO ' + TB + ' ' + #10)
       + '( ' + #10
       + '	Version, ' + #10
       + '	AppliedOn, ' + #10
       + '	Author, ' + #10
       + '	[Description] ' + #10
       + ') ' + #10
       + 'VALUES ' + #10
       + '( ' + #10
       + '	' + LvLatestVersion.ToString + ', ' + #10
       + '	(getdate()), ' + #10
       + '	' + LvAuthor.QuotedString + ', ' + #10
       + '	' + LvDescription.QuotedString + ' ' + #10
       + ')';
  end
  else
  begin
    LvScript := IfThen(((not FDbName.IsEmpty) and (not FSchema.IsEmpty)),
       'UPDATE [' + FDbName + '].[' + FSchema + '].[' + TB + '] ' + #10,
       'UPDATE ' + TB + ' ' + #10)
       + 'SET ' + #10
       + ' AppliedOn = (getdate()) ' + #10
       + ' ,Author = Author + ' + QuotedStr(' -- ') + ' + ' + LvAuthor.QuotedString + #10
       + ' ,[Description] = [Description] + ' + QuotedStr(' -- ') + ' + ' + LvDescription.QuotedString + #10
       + ' Where Version = ' + LvLatestVersion.ToString;
  end;

  FSQLConnection.ExecuteAdHocQuery(LvScript);
end;

procedure TSQLRunner.DownGradeVersionInfo(AVersionToDownGrade: Int64);
var
  LvScript: string;
begin
  LvScript := 'Delete from ' + TB + ' Where Version > ' + AVersionToDownGrade.ToString;
  FSQLConnection.ExecuteAdHocQuery(LvScript);
end;

end.
