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
    FORM: TORM;
    FFinalScript: TStringList;
    procedure GenerateSQLScript;
    procedure GenerateMySQLScript;
    function GetColType(ACol: TColumn): string;
    function GetOtherSwithches(ACol: TColumn): string;
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
  FORM := AORM;
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
  LvTable: TTable;
  LvCol: TColumn;
  LvStatement: string;

  procedure ConCat(ANewLine: string; AIsLatest: Boolean = False);
  begin
    if AIsLatest then
      LvStatement := LeftStr(LvStatement.Trim, Length(LvStatement) - 1) + #10 + ANewLine
    else
     LvStatement := LvStatement + #10 + ANewLine + #10;
  end;
begin
  for LvCreate in FORM.GetCreateList do
  begin
    LvTable := LvCreate.GetTable;
    LvStatement := 'If Not Exists( Select 1 From sysobjects Where Name = ' +  LvTable.TableName.QuotedString + ' And xtype = ''U'')';
    ConCat('Create Table ' + LvTable.TableName + '(');

    if LvTable.HasAutoID then
      ConCat(' ID Int Primary key Identity(1, 1) Not null, ');

    for LvCol in LvTable.ColumnList do
    begin
      if (LvCol.ColName = 'ID') and (LvTable.HasAutoID) then
        Continue;

      ConCat(LvCol.ColName + ' ' + GetColType(LvCol) + GetOtherSwithches(LvCol) + ',');
    end;

    ConCat(');', True);

    FFinalScript.Add(LvStatement);
  end;
end;

function TBuilder.GetColType(ACol: TColumn): string;
begin
  case ACol.DataType.ColType of
    ctBigInt: Result := 'BigInt';
    ctInt: Result := 'Int';
    ctSmallInt: Result := 'SmallInt';
    ctTinyInt: Result := 'TinyInt';
    ctBit: Result := 'Bit';
    ctDecimal: Result := 'Decimal(' + ACol.DataType.Precision.ToString + ', ' + ACol.DataType.Scale.ToString + ')';
    ctNumeric: Result := 'Numeric(' + ACol.DataType.Precision.ToString + ', ' + ACol.DataType.Scale.ToString + ')';
    ctMoney: Result := 'Money';
    ctSmallMoney: Result := 'SmallMoney';
    ctFloat: Result := 'Float';
    ctReal: Result := 'Real';
    ctDateTime: Result := 'DateTime';
    ctSmallDateTime: Result := 'SmallDateTime';
    ctDate: Result := 'Date';
    ctTime: Result := 'Time';
    ctDateTimeOffset: Result := 'DateTimeOffset';
    ctDatetime2: Result := 'Datetime2';
    ctChar: Result := 'Char(' + ACol.DataType.ColSize.ToString + ')';
    ctVarchar: Result := 'Varchar(' + ACol.DataType.ColSize.ToString + ')';
    ctVarcharMmax: Result := 'Varchar(Max)';
    ctText: Result := 'Text';
    ctNchar: Result := 'Nchar(' + ACol.DataType.ColSize.ToString + ')';
    ctNvarchar: Result := 'Nvarchar(' + ACol.DataType.ColSize.ToString + ')';
    ctNtext: Result := 'Ntext';
    ctBinary: Result := 'Binary(' + ACol.DataType.ColSize.ToString + ')';
    ctVarbinary: Result := 'Varbinary(' + ACol.DataType.ColSize.ToString + ')';
    ctImage: Result := 'Image';
    ctNone: Result := '';
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
  case FORM.GetTarget of
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
