unit EasyDB.ORM;

interface
uses
  Data.FmtBcd, Data.SqlTimSt, System.Generics.Collections;

type
  Largeint = Int64;
  TTable = class;

  TColType = (ctBigInt, ctInt, ctSmallInt, ctTinyInt, ctBit, ctDecimal, ctNumeric, ctMoney, ctSmallMoney,
              ctFloat, ctReal, ctDateTime, ctSmallDateTime, ctDate, ctTime, ctDateTimeOffset, ctDatetime2,
              ctChar, ctVarchar, ctVarcharMmax, ctText, ctNchar, ctNvarchar, ctNtext, ctBinary, ctVarbinary,
              ctImage, ctNone);

  TDataType = class
  private
    FColType: TColType;
    FColSize: Integer;
    FPrecision: Byte;
    FScale: Byte;
    FIsNullable: Boolean;
    FIsPrimay: Boolean;
    FAutoIdentity: Boolean;
    FAutoIdentityStart: Int64;
    FAutoIdentityStep: Int64;
  public
    constructor Create;
    function NotNullable: TTable;
    function Nullable: TTable;

    function IsPrimary: TDataType;
    function AutoIdentity(AStart, AStep: Int64): TDataType;

    property ColType: TColType read FColType write FColType;
    property ColSize: Integer read FColSize write FColSize;
    property Precision: Byte read FPrecision write FPrecision;
    property Scale: Byte read FScale write FScale;
  end;

  TColumn = class
    FColName: string;
    FDataType: TDataType;
  public
    constructor Create(AColName: string);
    function AsBigInt: TDataType;
    function AsInt: TDataType;
    function AsSmallInt: TDataType;
    function AsTinyInt: TDataType;
    function AsBit: TDataType;
    function AsDecimal(APrecision, AScale: Byte): TDataType;
    function AsNumeric(APrecision, AScale: Byte): TDataType;
    function AsMoney: TDataType;
    function AsSmallMoney: TDataType;
    function AsFloat: TDataType;
    function AsReal: TDataType;
    function AsDateTime: TDataType;
    function AsSmallDateTime: TDataType;
    function AsDate: TDataType;
    function AsTime: TDataType;
    function AsDateTimeOffset: TDataType;
    function AsDatetime2: TDataType;
    function AsChar(ASize: Integer): TDataType;
    function AsVarchar(ASize: Integer): TDataType;
    function AsVarcharMmax: TDataType;
    function AsText: TDataType;
    function AsNchar(ASize: Integer): TDataType;
    function AsNvarchar(ASize: Integer): TDataType;
    function AsNtext: TDataType;
    function AsBinary(ASize: Integer): TDataType;
    function AsVarbinary(ASize: Integer): TDataType;
    function AsImage: TDataType;
  end;

  TTable = class
  private
    FTableName: string;
    FColumnList: TObjectList<TColumn>;
    constructor Create(ATableName: string);
    destructor Destroy; override;
  public
    function WithIdColumn: TTable;
    function WithColumn(AColName: string): TColumn;
  end;

  TCreate = class
  private
    FTable: TTable;
  public
    function Table(ATableName: string): TTable;
    destructor Destroy; override;
  end;

  TAlter = class
  private
    FTable: TTable;
  public
    function Table(ATableName: string): TTable;
  end;

  TDelete = class
  private
    FTableName: string;
  public
    procedure Table(ATableName: string);
  end;

  TORM = class  //Singleton
  private
    class var FInstance: TORM;
    FCreateList: TObjectList<TCreate>;
    FAlterList: TObjectList<TAlter>;
    FDeleteList: TList<TDelete>;
    constructor NewORM;
  public
    class function GetInstance: TORM;
    destructor Destroy; override;
    function Create: TCreate;
    function Alter: TAlter;
    function Delete: TDelete;
  end;

{
  TParam = class
  private
    FName: string;
    FDataType: TTypeKind;
  end;

  TProcedure = class
  private
    FName: string;
    FParmList: TDictionary<string, TParam>;
  end;

  TFunction = class
  private
    FName: string;
    FParmList: TDictionary<string, TParam>;
  end;
}

implementation

{ TORM }

function TORM.Alter: TAlter;
begin
  Result := FAlterList[FAlterList.Add(TAlter.Create)];
end;

function TORM.Create: TCreate;
begin
  Result := FCreateList[FCreateList.Add(TCreate.Create)];
end;

function TORM.Delete: TDelete;
begin
  Result := FDeleteList[FDeleteList.Add(TDelete.Create)];
end;

class function TORM.GetInstance: TORM;
begin
  if not Assigned(FInstance) then
    FInstance := TORM.NewORM;
  Result := FInstance;
end;

constructor TORM.NewORM;
begin
  inherited Create;
  FCreateList := TObjectList<TCreate>.Create;
  FAlterList := TObjectList<TAlter>.Create;
  FDeleteList := TObjectList<TDelete>.Create;
end;

destructor TORM.Destroy;
begin
  FCreateList.Free;
  FAlterList.Free;
  FDeleteList.Free;

  inherited Destroy;
end;

{ TCreate }

destructor TCreate.Destroy;
begin
  FTable.Free;
  inherited;
end;

function TCreate.Table(ATableName: string): TTable;
begin
  FTable := TTable.Create(ATableName);
  Result := FTable;
end;

{ TTable }

constructor TTable.Create(ATableName: string);
begin
  FTableName := ATableName;
  FColumnList := TObjectList<TColumn>.Create;
end;

destructor TTable.Destroy;
begin
  FColumnList.Free;
  inherited;
end;

function TTable.WithColumn(AColName: string): TColumn;
var
  LvColumn: TColumn;
begin
  LvColumn := TColumn.Create(AColName);
  FColumnList.Add(LvColumn);
  Result := LvColumn;
end;

function TTable.WithIdColumn: TTable;
var
  LvColumn: TColumn;
begin
  LvColumn := TColumn.Create('ID');
  LvColumn.AsInt.IsPrimary.AutoIdentity(1, 1).NotNullable;
  FColumnList.Add(LvColumn);
  Result := Self;
end;

{ TColumn }

function TColumn.AsBigInt: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctBigInt;
  Result := FDataType;
end;

function TColumn.AsBinary(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctBinary;
  FDataType.ColSize := ASize;
  Result := FDataType;
end;

function TColumn.AsBit: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctBit;
  Result := FDataType;
end;

function TColumn.AsChar(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctChar;
  FDataType.ColSize := ASize;
  Result := FDataType;
end;

function TColumn.AsDate: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctDate;
  Result := FDataType;
end;

function TColumn.AsDateTime: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctDateTime;
  Result := FDataType;
end;

function TColumn.AsDatetime2: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctDatetime2;
  Result := FDataType;
end;

function TColumn.AsDateTimeOffset: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctDateTimeOffset;
  Result := FDataType;
end;

function TColumn.AsDecimal(APrecision, AScale: Byte): TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctDecimal;
  FDataType.Precision := APrecision;
  FDataType.Scale := AScale;
  Result := FDataType;
end;

function TColumn.AsFloat: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctFloat;
  Result := FDataType;
end;

function TColumn.AsImage: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctImage;
  Result := FDataType;
end;

function TColumn.AsInt: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctInt;
  Result := FDataType;
end;

function TColumn.AsMoney: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctMoney;
  Result := FDataType;
end;

function TColumn.AsNchar(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctNchar;
  FDataType.ColSize := ASize;
  Result := FDataType;
end;

function TColumn.AsNtext: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctNtext;
  Result := FDataType;
end;

function TColumn.AsNumeric(APrecision, AScale: Byte): TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctNumeric;
  FDataType.Precision := APrecision;
  FDataType.Scale := AScale;
  Result := FDataType;
end;

function TColumn.AsNvarchar(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctNvarchar;
  FDataType.ColSize := ASize;
  Result := FDataType;
end;

function TColumn.AsReal: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctReal;
  Result := FDataType;
end;

function TColumn.AsSmallDateTime: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctSmallDateTime;
  Result := FDataType;
end;

function TColumn.AsSmallInt: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctSmallInt;
  Result := FDataType;
end;

function TColumn.AsSmallMoney: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctSmallMoney;
  Result := FDataType;
end;

function TColumn.AsText: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctText;
  Result := FDataType;
end;

function TColumn.AsTime: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctTime;
  Result := FDataType;
end;

function TColumn.AsTinyInt: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctTinyInt;
  Result := FDataType;
end;

function TColumn.AsVarbinary(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctVarbinary;
  FDataType.ColSize := ASize;
  Result := FDataType;
end;

function TColumn.AsVarchar(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctVarchar;
  FDataType.ColSize := ASize;
  Result := FDataType;
end;

function TColumn.AsVarcharMmax: TDataType;
begin
  FDataType := TDataType.Create;
  FDataType.ColType := ctVarcharMmax;
  Result := FDataType;
end;

constructor TColumn.Create(AColName: string);
begin
  FColName := AColName;
end;

{ TDataType }

function TDataType.AutoIdentity(AStart, AStep: Int64): TDataType;
begin
  FAutoIdentity := True;
  FAutoIdentityStart := AStart;
  FAutoIdentityStep := AStep;
  Result := Self;
end;

constructor TDataType.Create;
begin
  FColType := ctNone;
  FColSize := 0;
  FPrecision := 0;
  FPrecision := 0;
  FScale := 0;
  FAutoIdentityStart := 1;
  FAutoIdentityStep := 1;
  FIsNullable := True;
  FIsPrimay := False;
  FAutoIdentity := False;
end;

function TDataType.IsPrimary: TDataType;
begin
  FIsPrimay := True;
  Result := Self;
end;

function TDataType.NotNullable: TTable;
begin
  FIsNullable := False;
end;

function TDataType.Nullable: TTable;
begin
  FIsNullable := True;
end;

{ TDelete }

procedure TDelete.Table(ATableName: string);
begin
  FTableName := ATableName;
end;

{ TAlter }

function TAlter.Table(ATableName: string): TTable;
begin
  FTable := TTable.Create(ATableName);
  Result := FTable;
end;

end.



