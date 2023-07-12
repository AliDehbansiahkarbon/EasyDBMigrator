unit EasyDB.ORM;

interface
uses
  Data.FmtBcd, Data.SqlTimSt, System.Generics.Collections,
  EasyDB.ORM.Core;

type
  TTable = class;
  TAlterTable = class;

  TDataType = class
  private
    FParentTable: TTable;
    FColType: TColType;
    FColSize: Integer;
    FPrecision: Byte;
    FScale: Byte;
    FIsNullable: Boolean;
    FIsPrimary: Boolean;
    FAutoIdentity: Boolean;
    FAutoIdentityStart: Int64;
    FAutoIdentityStep: Int64;
  public
    constructor Create(AParentTable: TTable);
    function NotNullable: TTable;
    function Nullable: TTable;

    function PrimaryKey: TDataType;
    function AutoIdentity(AStart, AStep: Int64): TDataType;

    property ColType: TColType read FColType write FColType;
    property ColSize: Integer read FColSize write FColSize;
    property Precision: Byte read FPrecision write FPrecision;
    property Scale: Byte read FScale write FScale;

    /// <summary cref="readonly">readonly</summary>
    property IsAutoIdentity: Boolean read FAutoIdentity;

    /// <summary cref="readonly">readonly</summary>
    property AutoIdentityStart: Int64 read FAutoIdentityStart;

    /// <summary cref="readonly">readonly</summary>
    property AutoIdentityStep: Int64 read FAutoIdentityStep;

    /// <summary cref="readonly">readonly</summary>
    property IsNullable: Boolean read FIsNullable;

    /// <summary cref="readonly">readonly</summary>
    property IsPrimary: Boolean read FIsPrimary;
  end;

  TColumn = class
    FParentTable: TTable;
    FColName: string;
    FDataType: TDataType;
  public
    constructor Create(AColName: string; AParentTable: TTable);
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

    /// <summary cref="readonly">readonly</summary>
    property DataType: TDataType read FDataType;

    /// <summary cref="readonly">readonly</summary>
    property ColName: string read FColName;
  end;

  TTable = class
  private
    FTableName: string;
    FHasAutoID: Boolean;
    FColumnList: TObjectList<TColumn>;
    constructor Create(ATableName: string);
    destructor Destroy; override;
  public
    function WithIdColumn: TTable;
    function WithColumn(AColName: string): TColumn;

    /// <summary cref="readonly">readonly</summary>
    property TableName: string read FTableName;

    /// <summary cref="readonly">readonly</summary>
    property HasAutoID: Boolean read FHasAutoID;

    /// <summary cref="readonly">Readonly</summary>
    property ColumnList: TObjectList<TColumn> read FColumnList;
  end;

  TProcedure = class
  private
    FName: string;
    FParams: TDictionary<string, TColType>;
    constructor Create(AName: string);
  public
    destructor Destroy; override;
    function AddParam(AName: string; ADataType: TColType): TProcedure;
    property Params: TDictionary<string, TColType> read FParams write FParams;
  end;

  TFunction = class
  private
    FName: string;
    FParams: TDictionary<string, TColType>;
  public
    constructor Create(AName: string);
    property Name: string read FName write FName;
    property Params: TDictionary<string, TColType> read FParams write FParams;
  end;

  TCreate = class
  private
    FTable: TTable;
    FProcedure: TProcedure;
    FFunction: TFunction;
  public
    constructor Create;
    destructor Destroy; override;

    function Table(ATableName: string): TTable;
    function StoredProc(AProcedureName: string): TProcedure;
    function StoredFunction(AFunctionName: string): TFunction;
    function GetTable: TTable;
  end;

  TAlterTable = class
  private
    FAlterMode: TAlterMode;
    FColumn: TColumn;

    FColName: string;
    FNewColName: string;
    FTableName: string;
  public
    constructor Create(ATableName: string);
    procedure DropColumn(AColName: string);
    function AddColumn(AColName: string): TColumn;
    function AlterColumn(AColName: string): TColumn;

    /// <summary> readonly </summary>
    property TableName: string read FTableName;
    /// <summary> readonly </summary>
    property AlterMode: TAlterMode read FAlterMode;
    /// <summary> readonly </summary>
    property Column: TColumn read FColumn;
    /// <summary> readonly </summary>
    property ColName: string read FColName;
    /// <summary> readonly </summary>
    property NewColName: string read FNewColName;
  end;

  TAlter = class
  private
    FTable: TAlterTable;
  public
    function Table(ATableName: string): TAlterTable;
    function GetTable: TAlterTable;
  end;

  TDelete = class
  private
    FObjectType: TObjectType;
    FObjectName: string;
  public
    procedure Table(ATableName: string);
    procedure StoredProc(AProcedureName: string);
    procedure StoredFunction(AFunction: string);
    procedure View(AViewName: string);

    /// <summary> readonly </summary>
    property ObjectName: string read FObjectName;
    /// <summary> readonly </summary>
    property ObjectType: TObjectType read FObjectType;
  end;

  TORM = class  //Singleton
  private
    class var FInstance: TORM;
    FTarget: TTargetType;
    FCreateList: TObjectList<TCreate>;
    FAlterList: TObjectList<TAlter>;
    FDeleteList: TList<TDelete>;
    constructor NewORM;
  public
    class function GetInstance(ATarget: TTargetType): TORM;
    destructor Destroy; override;
    function Create: TCreate;
    function Alter: TAlter;
    function Delete: TDelete;
    procedure SubmitChanges;
    function GetTarget: TTargetType;

    function GetCreateList: TObjectList<TCreate>;
    function GetAlterList: TObjectList<TAlter>;
    function GetDeletes: TList<TDelete>;
  end;

implementation
uses
  EasyDB.ORM.Builder;

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

function TORM.GetAlterList: TObjectList<TAlter>;
begin
  Result := FAlterList;
end;

function TORM.GetCreateList: TObjectList<TCreate>;
begin
  Result := FCreateList;
end;

function TORM.GetDeletes: TList<TDelete>;
begin
  Result := FDeleteList;
end;

class function TORM.GetInstance(ATarget: TTargetType): TORM;
begin
  if not Assigned(FInstance) then
    FInstance := TORM.NewORM;

  FInstance.FTarget := ATarget;
  Result := FInstance;
end;

function TORM.GetTarget: TTargetType;
begin
  Result := FTarget;
end;

constructor TORM.NewORM;
begin
  inherited Create;
  FCreateList := TObjectList<TCreate>.Create;
  FAlterList := TObjectList<TAlter>.Create;
  FDeleteList := TObjectList<TDelete>.Create;
end;

procedure TORM.SubmitChanges;
var
  LvBuilder: TBuilder;
begin
  LvBuilder := TBuilder.Create(Self);
  try
    LvBuilder.Submit;
  finally
    LvBuilder.Free;
  end;
end;

destructor TORM.Destroy;
begin
  FCreateList.Free;
  FAlterList.Free;
  FDeleteList.Free;

  inherited Destroy;
end;

{ TCreate }

constructor TCreate.Create;
begin
  FTable := nil;
  FProcedure := nil;
  FFunction := nil;
end;

destructor TCreate.Destroy;
begin
  if Assigned(FTable) then
    FTable.Free;

  if Assigned(FProcedure) then
    FProcedure.Free;

  if Assigned(FFunction) then
    FFunction.Free;

  inherited;
end;

function TCreate.GetTable: TTable;
begin
  Result := FTable;
end;

function TCreate.StoredFunction(AFunctionName: string): TFunction;
begin
  FFunction := TFunction.Create(AFunctionName);
end;

function TCreate.StoredProc(AProcedureName: string): TProcedure;
begin
  FProcedure := TProcedure.Create(AProcedureName);
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
  FHasAutoID := False;
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
  LvColumn := TColumn.Create(AColName, Self);
  FColumnList.Add(LvColumn);
  Result := LvColumn;
end;

function TTable.WithIdColumn: TTable;
var
  LvColumn: TColumn;
begin
  LvColumn := TColumn.Create('ID', Self);
  LvColumn.AsInt.PrimaryKey.AutoIdentity(1, 1).NotNullable;
  FColumnList.Add(LvColumn);
  FHasAutoID := True;
  Result := Self;
end;

{ TColumn }

function TColumn.AsBigInt: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctBigInt;
  Result := FDataType;
end;

function TColumn.AsBinary(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctBinary;
  FDataType.ColSize := ASize;
  Result := FDataType;
end;

function TColumn.AsBit: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctBit;
  Result := FDataType;
end;

function TColumn.AsChar(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctChar;
  FDataType.ColSize := ASize;
  Result := FDataType;
end;

function TColumn.AsDate: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctDate;
  Result := FDataType;
end;

function TColumn.AsDateTime: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctDateTime;
  Result := FDataType;
end;

function TColumn.AsDatetime2: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctDatetime2;
  Result := FDataType;
end;

function TColumn.AsDateTimeOffset: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctDateTimeOffset;
  Result := FDataType;
end;

function TColumn.AsDecimal(APrecision, AScale: Byte): TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctDecimal;
  FDataType.Precision := APrecision;
  FDataType.Scale := AScale;
  Result := FDataType;
end;

function TColumn.AsFloat: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctFloat;
  Result := FDataType;
end;

function TColumn.AsImage: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctImage;
  Result := FDataType;
end;

function TColumn.AsInt: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctInt;
  Result := FDataType;
end;

function TColumn.AsMoney: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctMoney;
  Result := FDataType;
end;

function TColumn.AsNchar(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctNchar;
  FDataType.ColSize := ASize;
  Result := FDataType;
end;

function TColumn.AsNtext: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctNtext;
  Result := FDataType;
end;

function TColumn.AsNumeric(APrecision, AScale: Byte): TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctNumeric;
  FDataType.Precision := APrecision;
  FDataType.Scale := AScale;
  Result := FDataType;
end;

function TColumn.AsNvarchar(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctNvarchar;
  FDataType.ColSize := ASize;
  Result := FDataType;
end;

function TColumn.AsReal: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctReal;
  Result := FDataType;
end;

function TColumn.AsSmallDateTime: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctSmallDateTime;
  Result := FDataType;
end;

function TColumn.AsSmallInt: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctSmallInt;
  Result := FDataType;
end;

function TColumn.AsSmallMoney: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctSmallMoney;
  Result := FDataType;
end;

function TColumn.AsText: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctText;
  Result := FDataType;
end;

function TColumn.AsTime: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctTime;
  Result := FDataType;
end;

function TColumn.AsTinyInt: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctTinyInt;
  Result := FDataType;
end;

function TColumn.AsVarbinary(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctVarbinary;
  FDataType.ColSize := ASize;
  Result := FDataType;
end;

function TColumn.AsVarchar(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctVarchar;
  FDataType.ColSize := ASize;
  Result := FDataType;
end;

function TColumn.AsVarcharMmax: TDataType;
begin
  FDataType := TDataType.Create(FParentTable);
  FDataType.ColType := ctVarcharMmax;
  Result := FDataType;
end;

constructor TColumn.Create(AColName: string; AParentTable: TTable);
begin
  FColName := AColName;
  FParentTable := AParentTable;
end;

{ TDataType }

function TDataType.AutoIdentity(AStart, AStep: Int64): TDataType;
begin
  FAutoIdentity := True;
  FAutoIdentityStart := AStart;
  FAutoIdentityStep := AStep;
  Result := Self;
end;

constructor TDataType.Create(AParentTable: TTable);
begin
  FColType := ctNone;
  FColSize := 0;
  FPrecision := 0;
  FPrecision := 0;
  FScale := 0;
  FAutoIdentityStart := 1;
  FAutoIdentityStep := 1;
  FIsNullable := True;
  FIsPrimary := False;
  FAutoIdentity := False;
  FParentTable := AParentTable;
end;

function TDataType.PrimaryKey: TDataType;
begin
  FIsPrimary := True;
  Result := Self;
end;

function TDataType.NotNullable: TTable;
begin
  FIsNullable := False;
  Result := FParentTable;
end;

function TDataType.Nullable: TTable;
begin
  FIsNullable := True;
  Result := FParentTable;
end;

{ TDelete }
procedure TDelete.StoredFunction(AFunction: string);
begin
  FObjectType := otFunction;
  FObjectName := AFunction;
end;

procedure TDelete.StoredProc(AProcedureName: string);
begin
  FObjectType := otStoredProcedure;
  FObjectName := AProcedureName;
end;

procedure TDelete.Table(ATableName: string);
begin
  FObjectType := otTable;
  FObjectName := ATableName;
end;

procedure TDelete.View(AViewName: string);
begin
  FObjectType := otView;
  FObjectName := AViewName;
end;

{ TAlter }

function TAlter.GetTable: TAlterTable;
begin
  Result := FTable;
end;

function TAlter.Table(ATableName: string): TAlterTable;
begin
  FTable := TAlterTable.Create(ATableName);
  Result := FTable;
end;

{ TAlterTable }

function TAlterTable.AddColumn(AColName: string): TColumn;
begin
  FAlterMode := amAdd;
  FColumn := TColumn.Create(AColName, nil);
  Result := FColumn;
end;

function TAlterTable.AlterColumn(AColName: string): TColumn;
begin
  FAlterMode := amEdit;
  FColumn := TColumn.Create(AColName, nil);
  Result := FColumn;
end;

constructor TAlterTable.Create(ATableName: string);
begin
  FTableName := ATableName;
  FColumn := nil;
end;

procedure TAlterTable.DropColumn(AColName: string);
begin
  FColName := AColName;
  FAlterMode := amDrop;
end;

{ TFunction }

constructor TFunction.Create(AName: string);
begin
  FName := AName;
end;

{ TProcedure }

function TProcedure.AddParam(AName: string; ADataType: TColType): TProcedure;
begin
  FParams.Add(AName, ADataType);
end;

constructor TProcedure.Create(AName: string);
begin
  FName := AName;
  FParams := TDictionary<string, TColType>.Create;
end;

destructor TProcedure.Destroy;
begin
  FParams.Free;
  inherited;
end;

end.



