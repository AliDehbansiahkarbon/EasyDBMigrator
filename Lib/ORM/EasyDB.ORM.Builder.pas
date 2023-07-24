unit EasyDB.ORM.Builder;

interface
uses
  System.Classes, System.SysUtils, System.StrUtils,

  EasyDB.ORM.Core,
  EasyDB.ORM,
  EasyDB.ConnectionManager.SQL,
  EasyDB.ConnectionManager.MySQL;

type

  TBuilder = class
  private
    FOrm: TORM;
    FFinalScript: TStringList;
    procedure GenerateSQLScript;
    procedure GenerateMySQLScript;
    function GetColType(ADataType: TDataType): string;
    function GetParamType(ADataType: TDataType): string;
    function GetColPosition(ADataType: TDataType): string;
    function GetOtherSwithches(ACol: TColumn): string;
    function GetObjectType(ADelObject: TDelete): string;
  public
    constructor Create(AORM: TORM);
    destructor Destroy; override;
    procedure Submit;
  end;

implementation

{ TBuilder }

constructor TBuilder.Create(AORM: TORM);
begin
  FFinalScript := TStringList.Create;
  FOrm := AORM;
end;

destructor TBuilder.Destroy;
begin
  FFinalScript.Free;
  inherited;
end;

procedure TBuilder.GenerateMySQLScript;
var
  LvCreate: TCreate;
  LvDbObject: TDbBaseObject;

  LvDatabase: TMySQLDatabase;
  LvTable: TTable;
  LvProcedure: TProcedure;
  LvFunction: TFunction;
  Param: string;

  LvAlter: TAlter;
  LvAlterTable: TAlterTable;
  LvDelete: TDelete;

  LvCol: TColumn;
  LvStatement: string;

  procedure ConCat(ANewLine: string; AIsLatest: Boolean = False);
  begin
    if AIsLatest then
      LvStatement := LeftStr(LvStatement.Trim, Length(LvStatement.Trim) - 1) + #10 + ANewLine + #10
    else
     LvStatement := LvStatement + #10 + ANewLine + #10;
  end;
begin
  LvStatement := EmptyStr;

  for LvCreate in FOrm.GetCreateList do
  begin
    LvDbObject := LvCreate.GetObject;

    if LvDbObject is TMySQLDatabase then
    begin
      LvDatabase := TMySQLDatabase(LvCreate.GetObject);
      LvStatement := 'CREATE DATABASE ' + LvDatabase.DBName;
      ConCat('DEFAULT CHARACTER SET ' + LvDatabase.CharacterSet + ' COLLATE ' + LvDatabase.Collate+ ';')
    end
    else if LvDbObject is TTable then
    begin
      LvTable :=  TTable(LvCreate.GetObject);
      LvStatement := 'CREATE TABLE IF NOT EXISTS ';
      ConCat(LvTable.TableName + ' ( ');

      if LvTable.HasAutoID then
        ConCat(' ID INT NOT NULL PRIMARY KEY AUTO_INCREMENT, ');

      for LvCol in LvTable.ColumnList do
      begin
        if (LvCol.ColName = 'ID') and (LvTable.HasAutoID) then
          Continue;

        ConCat(LvCol.ColName + ' ' + GetColType(LvCol.DataType) + GetOtherSwithches(LvCol) + ',');
      end;

      ConCat(');', True);
    end
    else if LvDbObject is TProcedure then
    begin
      LvProcedure := TProcedure(LvCreate.GetObject);
      LvStatement := 'DROP PROCEDURE IF EXISTS `' + LvProcedure.Name + '`;';
      FFinalScript.Add(LvStatement);

      LvStatement := EmptyStr;
      LvStatement := 'CREATE PROCEDURE ' + LvProcedure.Name;
      ConCat('(');
      for Param in LvProcedure.Params.Keys do //param = parameter name
        ConCat(GetParamType(LvProcedure.Params.Items[Param]) + ' ' + Param + ' ' + GetColType(LvProcedure.Params.Items[Param]) + ',');

      ConCat(')', True);
      ConCat(' BEGIN ');
      ConCat(LvProcedure.Body);
      ConCat(' END;');
    end
    else if LvDbObject is TFunction then
    begin
      LvFunction := TFunction(LvCreate.GetObject);
      LvStatement := 'DROP FUNCTION IF EXISTS `' + LvFunction.Name + '`;';
      FFinalScript.Add(LvStatement);
      LvStatement := EmptyStr;

      ConCat('CREATE FUNCTION `' + LvFunction.Name + '`');
      ConCat('(');

      for Param in LvFunction.Params.Keys do //param = parameter name
        ConCat(Param + ' ' + GetColType(LvFunction.Params.Items[Param]) + ',');

      ConCat(')', True);

      ConCat(' RETURNS ' + GetColType(LvFunction.GetReturnType) + IfThen(LvFunction.IsDeterministic, ' DETERMINISTIC ', ''));
      ConCat('BEGIN');
      ConCat(LvFunction.Body);
      ConCat('END;')
    end;

    FFinalScript.Add(LvStatement);
  end;
  FOrm.GetCreateList.Clear;

  LvStatement := EmptyStr;
  for LvAlter in FOrm.GetAlterList do
  begin
    LvAlterTable := LvAlter.GetTable;
    LvStatement := 'ALTER TABLE ' + LvAlterTable.TableName;

    case LvAlterTable.AlterMode of
      amAdd: ConCat(' ADD COLUMN ' + LvAlterTable.Column.ColName + GetColType(LvAlterTable.Column.DataType) + GetColPosition(LvAlterTable.Column.DataType));
      amDrop: ConCat(' DROP COLUMN ' + LvAlterTable.ColName);
      amEdit: ConCat(' MODIFY COLUMN ' + LvAlterTable.Column.ColName + GetColType(LvAlterTable.Column.DataType));
      amRename: ConCat(' RENAME COLUMN ' + LvAlterTable.ColName + ' TO ' +  LvAlterTable.NewColName);
    end;
    FFinalScript.Add(LvStatement);
  end;
  FOrm.GetAlterList.Clear;

  LvStatement := EmptyStr;
  for LvDelete in FOrm.GetDeletes do
  begin
    if Assigned(LvDelete) then
      FFinalScript.Add('DROP ' + GetObjectType(LvDelete) + LvDelete.ObjectName);
  end;

  FOrm.GetDeletes.Clear;
end;

procedure TBuilder.GenerateSQLScript;
var
  LvCreate: TCreate;
  LvDbObject: TDbBaseObject;

  LvDatabase: TSQLServerDatabase;
  LvTable: TTable;
  LvProcedure: TProcedure;
  LvFunction: TFunction;
  Param: string;

  LvAlter: TAlter;
  LvAlterTable: TAlterTable;
  LvDelete: TDelete;

  LvCol: TColumn;
  LvStatement: string;

  procedure ConCat(ANewLine: string; AIsLatest: Boolean = False);
  begin
    if AIsLatest then
      LvStatement := LeftStr(LvStatement.Trim, Length(LvStatement.Trim) - 1) + #10 + ANewLine + #10
    else
     LvStatement := LvStatement + #10 + ANewLine + #10;
  end;
begin
  LvStatement := EmptyStr;

  for LvCreate in FOrm.GetCreateList do
  begin
    LvDbObject := LvCreate.GetObject;

    if not Assigned(LvDbObject) then
      Continue;

    if LvDbObject is TSQLServerDatabase then
    begin
      LvDatabase := TSQLServerDatabase(LvDbObject);
      LvStatement := 'IF NOT EXISTS (SELECT name FROM sys.databases WHERE name = ' + LvDatabase.DBName.QuotedString + ')';
      ConCat('BEGIN');
      ConCat('CREATE DATABASE ' + LvDatabase.DBName);
      ConCat('ON (NAME = ''db_data'', FILENAME = ' + LvDatabase.GetMdfFileName.QuotedString +
        ', SIZE = ' + LvDatabase.GetMdfSize + ', MAXSIZE = ' + LvDatabase.GetMdfMaxSize + ', FILEGROWTH = ' + LvDatabase.GetMdfFileGrowth + ')');

      ConCat('LOG ON (NAME = ''db_log'', FILENAME = ' + LvDatabase.GetLdfFileName.QuotedString +
        ', SIZE = ' + LvDatabase.GetLdfSize + ', MAXSIZE = ' + LvDatabase.GetLdfMaxSize + ', FILEGROWTH = ' + LvDatabase.GetLdfFileGrowth + ')');

      ConCat('COLLATE ' + LvDatabase.GetCollation);
      ConCat('END');
    end else if LvDbObject is TTable then
    begin
      LvTable :=  TTable(LvDbObject);
      LvStatement := 'IF NOT EXISTS( SELECT 1 FROM SYSOBJECTS WHERE NAME = ' +  LvTable.TableName.QuotedString + ' AND XTYPE = ''U'')';
      ConCat('CREATE TABLE ' + LvTable.TableName + '(');

      if LvTable.HasAutoID then
        ConCat(' ID INT PRIMARY KEY IDENTITY(1, 1) NOT NULL, ');

      for LvCol in LvTable.ColumnList do
      begin
        if (LvCol.ColName = 'ID') and (LvTable.HasAutoID) then
          Continue;

        ConCat(LvCol.ColName + ' ' + GetColType(LvCol.DataType) + GetOtherSwithches(LvCol) + ',');
      end;

      ConCat(');', True);
    end
    else if LvDbObject is TProcedure then
    begin
      LvProcedure := TProcedure(LvDbObject);
      LvStatement := 'IF EXISTS (SELECT 1 FROM SYS.OBJECTS WHERE OBJECT_ID = OBJECT_ID(N' + LvProcedure.Name.QuotedString + ') AND TYPE IN (N''P'', N''PC''))'
                     + #10 + ' DROP PROCEDURE ' + LvProcedure.Name + #10;

      FFinalScript.Add(LvStatement);
      LvStatement := EmptyStr;

      ConCat('CREATE PROCEDURE ' + LvProcedure.Name);
      for Param in LvProcedure.Params.Keys do
        ConCat('@' + Param + ' ' + GetColType(LvProcedure.Params.Items[Param]) + ',');

      ConCat('AS', True);
      ConCat('BEGIN');
      ConCat(LvProcedure.Body);
      ConCat('END');
    end
    else if LvDbObject is TFunction then
    begin
      LvFunction := TFunction(LvDbObject);
      LvStatement := 'IF EXISTS (SELECT 1 FROM SYSOBJECTS WHERE ID = OBJECT_ID(N' + LvFunction.Name.QuotedString + ') AND XTYPE IN (N''FN'', N''IF'', N''TF''))'
                     + #10 + ' DROP FUNCTION ' + LvFunction.Name + #10;

      FFinalScript.Add(LvStatement);
      LvStatement := EmptyStr;

      ConCat('CREATE FUNCTION ' + LvFunction.Name);
      ConCat('(');
      for Param in LvFunction.Params.Keys do
        ConCat('@' + Param + ' ' + GetColType(LvFunction.Params.Items[Param]) + ',');

      ConCat(')', True);
      ConCat('RETURNS ' + GetColType(LvFunction.GetReturnType));
      ConCat('BEGIN');
      ConCat(LvFunction.Body);
      ConCat('END');
    end;

    FFinalScript.Add(LvStatement);
  end;
  FOrm.GetCreateList.Clear;

  LvStatement := EmptyStr;
  for LvAlter in FOrm.GetAlterList do
  begin
    LvAlterTable := LvAlter.GetTable;
    LvStatement := 'ALTER TABLE ' + LvAlterTable.TableName;
    case LvAlterTable.AlterMode of
      amAdd: ConCat(' ADD ' + LvAlterTable.Column.ColName + GetColType(LvAlterTable.Column.DataType));
      amDrop: ConCat(' DROP COLUMN ' + LvAlterTable.ColName);
      amEdit: ConCat(' ALTER COLUMN ' + LvAlterTable.Column.ColName + GetColType(LvAlterTable.Column.DataType));
      amRename: ConCat(' RENAME COLUMN ' + LvAlterTable.ColName + ' ' +  LvAlterTable.NewColName);
    end;
    FFinalScript.Add(LvStatement);
  end;
  FOrm.GetAlterList.Clear;

  LvStatement := EmptyStr;
  for LvDelete in FOrm.GetDeletes do
  begin
    if Assigned(LvDelete) then
      FFinalScript.Add('DROP ' + GetObjectType(LvDelete) + LvDelete.ObjectName);
  end;

  FOrm.GetDeletes.Clear;
end;

function TBuilder.GetColPosition(ADataType: TDataType): string;
begin
  Result := ' ' + ADataType.ParamPosition + ' ';
end;

function TBuilder.GetColType(ADataType: TDataType): string;
begin
  case FOrm.GetTarget of
    ttSQLServer :
    begin

      case ADataType.ColType of
        ctBigInt: Result := ' BIGINT';
        ctInt: Result := ' INT';
        ctSmallInt: Result := ' SMALLINT';
        ctTinyInt: Result := ' TINYINT';
        ctBit: Result := ' BIT';
        ctDecimal: Result := ' DECIMAL(' + ADataType.Precision.ToString + ', ' + ADataType.Scale.ToString + ')';
        ctNumeric: Result := ' NUMERIC(' + ADataType.Precision.ToString + ', ' + ADataType.Scale.ToString + ')';
        ctMoney: Result := ' MONEY';
        ctSmallMoney: Result := ' SMALLMONEY';
        ctFloat: Result := ' FlOAT';
        ctReal: Result := ' REAL';
        ctDateTime: Result := ' DATETIME';
        ctSmallDateTime: Result := ' SMALLDATETIME';
        ctDate: Result := ' DATE';
        ctTime: Result := ' TIME';
        ctDateTimeOffset: Result := ' DATETIMEOFFSET';
        ctDatetime2: Result := ' DATETIME2';
        ctChar: Result := ' CHAR(' + ADataType.ColSize.ToString + ')';
        ctVarchar: Result := ' VARCHAR(' + ADataType.ColSize.ToString + ')';
        ctVarcharMmax: Result := ' VARCHAR(MAX)';
        ctText: Result := ' TEXT';
        ctNchar: Result := ' NCHAR(' + ADataType.ColSize.ToString + ')';
        ctNvarchar: Result := ' NVARCHAR(' + ADataType.ColSize.ToString + ')';
        ctNtext: Result := ' NTEXT';
        ctBinary: Result := ' BINARY(' + ADataType.ColSize.ToString + ')';
        ctVarbinary: Result := ' VARBINARY(' + ADataType.ColSize.ToString + ')';
        ctImage: Result := ' IMAGE';
        ctNone: Result := '';
      end;

    end;

    ttMySQL, ttMariaDB:
    begin

      case ADataType.ColType of
        ctBigInt: Result := ' BIGINT';
        ctInt: Result := ' INT';
        ctSmallInt: Result := ' SMALLINT';
        ctTinyInt: Result := ' TINYINT';
        ctBit: Result := ' BIT';
        ctDecimal: Result := ' DECIMAL(' + ADataType.Precision.ToString + ', ' + ADataType.Scale.ToString + ')';
        ctNumeric: Result := ' NUMERIC(' + ADataType.Precision.ToString + ', ' + ADataType.Scale.ToString + ')';
        ctMoney: Result := ' DOUBLE';
        ctSmallMoney: Result := ' FLOAT';
        ctFloat: Result := ' FLOAT';
        ctReal: Result := ' FLOAT';
        ctDateTime: Result := ' DATETIME';
        ctSmallDateTime: Result := ' DATETIME';
        ctDate: Result := ' DATE';
        ctTime: Result := ' TIME';
        ctDateTimeOffset: Result := ' DateTimeOffset is not supported';
        ctDatetime2: Result := ' DATETIME ';
        ctChar: Result := ' CHAR(' + ADataType.ColSize.ToString + ')';
        ctVarchar: Result := ' VARCHAR(' + ADataType.ColSize.ToString + ')';
        ctVarcharMmax: Result := ' LONGTEXT ';
        ctText: Result := ' Text';
        ctNchar: Result := ' NCHAR (' + ADataType.ColSize.ToString + ')';
        ctNvarchar: Result := ' NVARCHAR (' + ADataType.ColSize.ToString + ')';
        ctNtext: Result := ' LONGTEXT ';
        ctBinary: Result := ' BINARY(' + ADataType.ColSize.ToString + ')';
        ctVarbinary: Result := ' VARBINARY(' + ADataType.ColSize.ToString + ')';
        ctImage: Result := ' BLOB';
        ctNone: Result := '';
      end;

    end;
  end;
end;

function TBuilder.GetObjectType(ADelObject: TDelete): string;
begin
  case ADelObject.ObjectType of
    otTable: Result := ' TABLE ';
    otView: Result := ' VIEW ';
    otStoredProcedure: Result := ' PROCEDURE ';
    otFunction: Result := ' FUNCTION ';
  end;
end;

function TBuilder.GetOtherSwithches(ACol: TColumn): string;
begin
  Result := '';

  if ACol.DataType.IsPrimary then
    Result := ' PRIMARY KEY ';


  if ACol.DataType.IsAutoIdentity then
  begin
    case FOrm.GetTarget of
      ttSQLServer:
      begin
        Result := Result +
        ' IDENTITY(' + ACol.DataType.AutoIdentityStart.ToString + ', ' + ACol.DataType.AutoIdentityStep.ToString + ') ';
      end;

      ttMySQL, ttMariaDB:
      begin
        Result := Result +
          ' AUTOINCREMENT((' + ACol.DataType.AutoIdentityStart.ToString + ', ' + ACol.DataType.AutoIdentityStep.ToString + ') ';
      end;
    end;
  end;

  if ACol.DataType.IsNullable then
    Result := Result + ' NULL '
  else
    Result := Result + ' NOT NULL ';
end;

function TBuilder.GetParamType(ADataType: TDataType): string;
begin
  case ADataType.ParamType of
    ptIN : Result := ' IN ';
    ptOUT : Result := ' OUT ';
    ptINOUT : Result := ' INOUT ';
    ptNone : Result := '';
  end;
end;

procedure TBuilder.Submit;
var
  I: Integer;
begin
  case FOrm.GetTarget of
    ttSQLServer:
    begin
      GenerateSQLScript;
      for I := 0 to Pred(FFinalScript.Count) do
        TSQLConnection.Instance.ExecuteAdHocQuery(FFinalScript[I]);
    end;

    ttMySQL, ttMariaDB:
    begin
      GenerateMySQLScript;
      for I := 0 to Pred(FFinalScript.Count) do
        TMySQLConnection.Instance.ExecuteAdHocQuery(FFinalScript[I]);
    end;
  end;
end;

end.
