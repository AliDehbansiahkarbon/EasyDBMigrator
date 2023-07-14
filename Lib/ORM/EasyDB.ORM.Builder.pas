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
begin
//TODO
end;

procedure TBuilder.GenerateSQLScript;
var
  LvCreate: TCreate;
  LvDbObject: TDbBaseObject;

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

    if LvDbObject is TTable then
    begin
      LvTable :=  TTable(LvCreate.GetObject);
      LvStatement := 'If Not Exists( Select 1 From sysobjects Where Name = ' +  LvTable.TableName.QuotedString + ' And xtype = ''U'')';
      ConCat('Create Table ' + LvTable.TableName + '(');

      if LvTable.HasAutoID then
        ConCat(' ID Int Primary key Identity(1, 1) Not null, ');

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
      LvStatement := 'If Exists (Select 1 From sys.objects Where Object_id = Object_id(N' + LvProcedure.Name.QuotedString + ') And Type In (N''P'', N''PC''))'
                     + #10 + ' Drop Procedure ' + LvProcedure.Name + #10;

      FFinalScript.Add(LvStatement);
      LvStatement := EmptyStr;

      ConCat('Create Procedure ' + LvProcedure.Name);
      for Param in LvProcedure.Params.Keys do
        ConCat('@' + Param + ' ' + GetColType(LvProcedure.Params.Items[Param]) + ',');

      ConCat('As', True);
      ConCat('Begin');
      ConCat(LvProcedure.Body);
      ConCat('End');
    end
    else if LvDbObject is TFunction then
    begin
      LvFunction := TFunction(LvCreate.GetObject);
      LvStatement := 'If Exists (Select * From   sysobjects Where  id = Object_id(N' + LvFunction.Name.QuotedString + ') And xtype In (N''FN'', N''IF'', N''TF''))'
                     + #10 + ' Drop Function ' + LvFunction.Name + #10;

      FFinalScript.Add(LvStatement);
      LvStatement := EmptyStr;

      ConCat('Create Function ' + LvFunction.Name);
      ConCat('(');
      for Param in LvFunction.Params.Keys do
        ConCat('@' + Param + ' ' + GetColType(LvFunction.Params.Items[Param]) + ',');

      ConCat(')', True);
      ConCat('Returns ' + GetColType(LvFunction.GetReturnType));
      ConCat('Begin');
      ConCat(LvFunction.Body);
      ConCat('End');
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

function TBuilder.GetColType(ADataType: TDataType): string;
begin
  case ADataType.ColType of
    ctBigInt: Result := ' BigInt';
    ctInt: Result := ' Int';
    ctSmallInt: Result := ' SmallInt';
    ctTinyInt: Result := ' TinyInt';
    ctBit: Result := ' Bit';
    ctDecimal: Result := ' Decimal(' + ADataType.Precision.ToString + ', ' + ADataType.Scale.ToString + ')';
    ctNumeric: Result := ' Numeric(' + ADataType.Precision.ToString + ', ' + ADataType.Scale.ToString + ')';
    ctMoney: Result := ' Money';
    ctSmallMoney: Result := ' SmallMoney';
    ctFloat: Result := ' Float';
    ctReal: Result := ' Real';
    ctDateTime: Result := ' DateTime';
    ctSmallDateTime: Result := ' SmallDateTime';
    ctDate: Result := ' Date';
    ctTime: Result := ' Time';
    ctDateTimeOffset: Result := ' DateTimeOffset';
    ctDatetime2: Result := ' Datetime2';
    ctChar: Result := ' Char(' + ADataType.ColSize.ToString + ')';
    ctVarchar: Result := ' Varchar(' + ADataType.ColSize.ToString + ')';
    ctVarcharMmax: Result := ' Varchar(Max)';
    ctText: Result := ' Text';
    ctNchar: Result := ' Nchar(' + ADataType.ColSize.ToString + ')';
    ctNvarchar: Result := ' Nvarchar(' + ADataType.ColSize.ToString + ')';
    ctNtext: Result := ' Ntext';
    ctBinary: Result := ' Binary(' + ADataType.ColSize.ToString + ')';
    ctVarbinary: Result := ' Varbinary(' + ADataType.ColSize.ToString + ')';
    ctImage: Result := ' Image';
    ctNone: Result := '';
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
    Result := ' Primary key ';

  if ACol.DataType.IsAutoIdentity then
    Result := Result +
    ' Identity(' + ACol.DataType.AutoIdentityStart.ToString + ', ' + ACol.DataType.AutoIdentityStep.ToString + ') ';

  if ACol.DataType.IsNullable then
    Result := Result + ' NULL '
  else
    Result := Result + ' NOT NULL ';
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

    ttMySQL:
    begin
      GenerateMySQLScript;
      for I := 0 to Pred(FFinalScript.Count) do
        TMySQLConnection.Instance.ExecuteAdHocQuery(FFinalScript[I]);
    end;
  end;
end;

end.
