unit EasyDB.ORM;

interface
uses
  Data.FmtBcd, Data.SqlTimSt, System.Generics.Collections,
  EasyDB.ORM.Core;

type
  TTable = class;
  TAlterTable = class;

  TBaseDataType = class
  private
    FType: TColType;
    FSize: Integer;
    FPrecision: Byte;
    FScale: Byte;
    FName: string;
  public
    constructor Create(AType: TColType); overload;
    constructor Create(AType: TColType; ASize: Integer); overload;
    constructor Create(AType: TColType; APrecision: Byte; AScale: Byte); overload;
    constructor Create(AName: string; AType: TColType; ASize: Integer; APrecision: Byte; AScale: Byte); overload;

    property ColType: TColType read FType;
    property ColSize: Integer read FSize;
    property Precision: Byte read FPrecision;
    property Scale: Byte read FScale;
  end;

  TDataType = class(TBaseDataType)
  private
    FParentTable: TTable;
    FIsNullable: Boolean;
    FIsPrimary: Boolean;
    FAutoIdentity: Boolean;
    FAutoIdentityStart: Int64;
    FAutoIdentityStep: Int64;
    FParamType: TParamType;
    FParamPosition: string;
    constructor Create; overload;
    {$IFDEF FullOptions}
    constructor Create(AParentTable: TTable); overload;
    constructor Create(AParentTable: TTable; AColType: TColType; AParamType: TParamType); overload;
    constructor Create(AParentTable: TTable; AColType: TColType; AParamPosition: string); overload;
    constructor Create(AParentTable: TTable; AColType: TColType; AParamType: TParamType; AParamPosition: string); overload;
    {$ENDIF}
    constructor Create(AParentTable: TTable; AColType: TColType); overload;
    constructor Create(AParentTable: TTable; AColType: TColType; AColSize: Integer); overload;
    constructor Create(AParentTable: TTable; AColType: TColType; APrecision: Byte; AScale: Byte); overload;
  public
    function NotNullable: TTable;
    function Nullable: TTable;
    function PrimaryKey: TDataType;
    function AutoIdentity(AStart, AStep: Int64): TDataType;

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
    /// <summary cref="readonly">MySQL only- readonly</summary>
    property ParamType: TParamType read FParamType;
    /// <summary cref="readonly">MySQL only- readonly</summary>
    property ParamPosition: string read FParamPosition;
  end;

  TColumn = class
  private
    FParentTable: TTable;
    FColName: string;
    FDataType: TDataType;
    constructor Create(AColName: string; AParentTable: TTable);
  public
    destructor Destroy; override;

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

  TDbBaseObject = class
  end;

  TTable = class(TDbBaseObject)
  private
    FTableName: string;
    FHasAutoID: Boolean;
    FColumnList: TObjectList<TColumn>;
    constructor Create(ATableName: string);
  public
    destructor Destroy; override;
    function WithIdColumn: TTable;
    function WithColumn(AColName: string): TColumn;

    /// <summary cref="readonly">readonly</summary>
    property TableName: string read FTableName;
    /// <summary cref="readonly">readonly</summary>
    property HasAutoID: Boolean read FHasAutoID;
    /// <summary cref="readonly">Readonly</summary>
    property ColumnList: TObjectList<TColumn> read FColumnList;
  end;

  TProcedure = class(TDbBaseObject)
  private
    FName: string;
    FBody: string;
    FParams: TObjectDictionary<string, TDataType>;
    constructor Create(AName: string);
  public
    destructor Destroy; override;
    function AddParam(AName: string; ADataType: TDataType): TProcedure;
    procedure AddBody(ABody: string);

    /// <summary cref="readonly">Readonly</summary>
    property Params: TObjectDictionary<string, TDataType> read FParams;
    /// <summary cref="readonly">Readonly</summary>
    property Name: string read FName;
    /// <summary cref="readonly">Readonly</summary>
    property Body: string read FBody;
  end;

  TFunction = class(TDbBaseObject)
  private
    FName: string;
    FBody: string;
    FParams: TObjectDictionary<string, TDataType>;
    FReturnType: TDataType;
    FIsDeterministic: Boolean;
    constructor Create(AName: string);
  public
    destructor Destroy; override;
    function AddParam(AName: string; ADataType: TDataType): TFunction;
    function ReturnType(AType: TDataType): TFunction; overload;
    function ReturnType(AType: TDataType; AIsDeterministic: Boolean): TFunction; overload;
    procedure AddBody(ABody: string);
    function GetReturnType: TDataType;

    /// <summary cref="readonly">Readonly</summary>
    property Params: TObjectDictionary<string, TDataType> read FParams;
    /// <summary cref="readonly">Readonly</summary>
    property Name: string read FName;
    /// <summary cref="readonly">Readonly</summary>
    property Body: string read FBody;
    /// <summary cref="readonly">Readonly</summary>
    property IsDeterministic: Boolean read FIsDeterministic;
  end;

  TSQLServerDatabase = class(TDbBaseObject)
  private
    FDBName: string;
    FMdfFileName: string;
    FMdfSize: string;
    FMdfFileGrowth: string;
    FMdfMaxSize: string;
    FLdfFileName: string;
    FLdfSize: string;
    FLdfFileGrowth: string;
    FLdfMaxSize: string;
    FCollation: string;
  public
    constructor Create(ADBName: string);
    function MdfFileName(AName: string): TSQLServerDatabase;
    function MdfSize(ASize: string): TSQLServerDatabase;
    function MdfFileGrowth(ASize: string): TSQLServerDatabase;
    function MdfMaxSize(ASize: string): TSQLServerDatabase;
    function LdfFileName(AName: string): TSQLServerDatabase;
    function LdfSize(ASize: string): TSQLServerDatabase;
    function LdfFileGrowth(ASize: string): TSQLServerDatabase;
    function LdfMaxSize(ASize: string): TSQLServerDatabase;
    function Collation(ACollName: string): TSQLServerDatabase;

    /// <summary> readonly </summary>
    property DBName: string read FDBName;
    /// <summary> readonly </summary>
    property GetMdfFileName: string read FMdfFileName;
    /// <summary> readonly </summary>
    property GetMdfSize: string read FMdfSize;
    /// <summary> readonly </summary>
    property GetMdfFileGrowth: string read FMdfFileGrowth;
    /// <summary> readonly </summary>
    property GetMdfMaxSize: string read FMdfMaxSize;
    /// <summary> readonly </summary>
    property GetLdfFileName: string read FLdfFileName;
    /// <summary> readonly </summary>
    property GetLdfSize: string read FLdfSize;
    /// <summary> readonly </summary>
    property GetLdfFileGrowth: string read FLdfFileGrowth;
    /// <summary> readonly </summary>
    property GetLdfMaxSize: string read FLdfMaxSize;
    /// <summary> readonly </summary>
    property GetCollation: string read FCollation;
  end;

  TMySQLDatabase = class(TDbBaseObject)
  private
    FDBName: string;
    FCharacterSet: string;
    FCollate: string;
  public
     function GetCollate(ACollateName: string): TMySQLDatabase;
     function GetCharacterSet(ACharacterSet: string): TMySQLDatabase;

    /// <summary> readonly </summary>
    property DBName: string read FDBName;
    /// <summary> readonly </summary>
    property CharacterSet: string read FCharacterSet;
    /// <summary> readonly </summary>
    property Collate: string read FCollate;
  end;

  TCreate = class
  private
    FDatabase: TSQLServerDatabase;
    FTable: TTable;
    FProcedure: TProcedure;
    FFunction: TFunction;
  public
    constructor Create;
    destructor Destroy; override;

    function Database(ADbName: string): TSQLServerDatabase;
    function Table(ATableName: string): TTable;
    function StoredProc(AProcedureName: string): TProcedure;
    function StoredFunction(AFunctionName: string): TFunction;
    function GetObject: TDbBaseObject;
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
    destructor Destroy; override;

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
    constructor Create;
    destructor Destroy; override;
    function Table(ATableName: string): TAlterTable;
    function GetTable: TAlterTable;
  end;

  TDelete = class
  private
    FObjectType: TObjectType;
    FObjectName: string;
  public
    procedure Database(ADBName: string);
    procedure Table(ATableName: string);
    procedure StoredProc(AProcedureName: string);
    procedure StoredFunc(AFunction: string);
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
  FInstance := nil;
  inherited Destroy;
end;

{ TCreate }

constructor TCreate.Create;
begin
  FDatabase := nil;
  FTable := nil;
  FProcedure := nil;
  FFunction := nil;
end;

function TCreate.Database(ADbName: string): TSQLServerDatabase;
begin
  FDatabase := TSQLServerDatabase.Create(ADbName);
  Exit(FDatabase);
end;

destructor TCreate.Destroy;
begin
  if Assigned(FDatabase) then
    FDatabase.Free;

  if Assigned(FTable) then
    FTable.Free;

  if Assigned(FProcedure) then
    FProcedure.Free;

  if Assigned(FFunction) then
    FFunction.Free;

  inherited;
end;

function TCreate.GetObject: TDbBaseObject;
begin
  if Assigned(FDatabase) then
    Result := FDatabase
  else
  if Assigned(FTable) then
    Result := FTable
  else if Assigned(FProcedure) then
    Result := FProcedure
  else if Assigned(FFunction) then
    Result := FFunction
  else
    Result := nil;
end;

function TCreate.StoredFunction(AFunctionName: string): TFunction;
begin
  FFunction := TFunction.Create(AFunctionName);
  Result := FFunction;
end;

function TCreate.StoredProc(AProcedureName: string): TProcedure;
begin
  FProcedure := TProcedure.Create(AProcedureName);
  Exit(FProcedure);
end;

function TCreate.Table(ATableName: string): TTable;
begin
  FTable := TTable.Create(ATableName);
  Exit(FTable);
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
  Exit(LvColumn);
end;

function TTable.WithIdColumn: TTable;
var
  LvColumn: TColumn;
begin
  LvColumn := TColumn.Create('ID', Self);
  LvColumn.AsInt.PrimaryKey.AutoIdentity(1, 1).NotNullable;
  FColumnList.Add(LvColumn);
  FHasAutoID := True;
  Exit(Self);
end;

{ TColumn }

function TColumn.AsBigInt: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctBigInt);
  Exit(FDataType);
end;

function TColumn.AsBinary(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctBinary, ASize);
  Exit(FDataType);
end;

function TColumn.AsBit: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctBit);
  Exit(FDataType);
end;

function TColumn.AsChar(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctChar, ASize);
  Exit(FDataType);
end;

function TColumn.AsDate: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctDate);
  Exit(FDataType);
end;

function TColumn.AsDateTime: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctDateTime);
  Exit(FDataType);
end;

function TColumn.AsDatetime2: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctDatetime2);
  Exit(FDataType);
end;

function TColumn.AsDateTimeOffset: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctDateTimeOffset);
  Exit(FDataType);
end;

function TColumn.AsDecimal(APrecision, AScale: Byte): TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctDecimal, APrecision, AScale);
  Exit(FDataType);
end;

function TColumn.AsFloat: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctFloat);
  Exit(FDataType);
end;

function TColumn.AsImage: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctImage);
  Exit(FDataType);
end;

function TColumn.AsInt: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctInt);
  Exit(FDataType);
end;

function TColumn.AsMoney: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctMoney);
  Exit(FDataType);
end;

function TColumn.AsNchar(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctNchar, ASize);
  Exit(FDataType);
end;

function TColumn.AsNtext: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctNtext);
  Exit(FDataType);
end;

function TColumn.AsNumeric(APrecision, AScale: Byte): TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctNumeric, APrecision, AScale);
  Exit(FDataType);
end;

function TColumn.AsNvarchar(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctNvarchar, ASize);
  Exit(FDataType);
end;

function TColumn.AsReal: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctReal);
  Exit(FDataType);
end;

function TColumn.AsSmallDateTime: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctSmallDateTime);
  Exit(FDataType);
end;

function TColumn.AsSmallInt: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctSmallInt);
  Exit(FDataType);
end;

function TColumn.AsSmallMoney: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctSmallMoney);
  Exit(FDataType);
end;

function TColumn.AsText: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctText);
  Exit(FDataType);
end;

function TColumn.AsTime: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctTime);
  Exit(FDataType);
end;

function TColumn.AsTinyInt: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctTinyInt);
  Exit(FDataType);
end;

function TColumn.AsVarbinary(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctVarbinary, ASize);
  Exit(FDataType);
end;

function TColumn.AsVarchar(ASize: Integer): TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctVarchar, ASize);
  Exit(FDataType);
end;

function TColumn.AsVarcharMmax: TDataType;
begin
  FDataType := TDataType.Create(FParentTable, ctVarcharMmax);
  Exit(FDataType);
end;

constructor TColumn.Create(AColName: string; AParentTable: TTable);
begin
  FColName := AColName;
  FParentTable := AParentTable;
end;

destructor TColumn.Destroy;
begin
  FDataType.Free;
  inherited;
end;

{ TDataType }

function TDataType.AutoIdentity(AStart, AStep: Int64): TDataType;
begin
  FAutoIdentity := True;
  FAutoIdentityStart := AStart;
  FAutoIdentityStep := AStep;
  Exit(Self);
end;

function TDataType.PrimaryKey: TDataType;
begin
  FIsPrimary := True;
  Exit(Self);
end;

constructor TDataType.Create(AParentTable: TTable; AColType: TColType);
begin
  Create;
  FParentTable := AParentTable;
  FType := AColType;
end;

constructor TDataType.Create;
begin
  FType := ctNone;
  FSize := 0;
  FPrecision := 0;
  FPrecision := 0;
  FScale := 0;
  FAutoIdentityStart := 1;
  FAutoIdentityStep := 1;
  FIsNullable := True;
  FIsPrimary := False;
  FAutoIdentity := False;
  FParamType := ptNone;
end;

function TDataType.NotNullable: TTable;
begin
  FIsNullable := False;
  Exit(FParentTable);
end;

function TDataType.Nullable: TTable;
begin
  FIsNullable := True;
  Exit(FParentTable);
end;

constructor TDataType.Create(AParentTable: TTable; AColType: TColType; AColSize: Integer);
begin
  Create;
  FParentTable := AParentTable;
  FType := AColType;
  FSize := AColSize;
end;

constructor TDataType.Create(AParentTable: TTable; AColType: TColType; APrecision, AScale: Byte);
begin
  Create;
  FParentTable := AParentTable;
  FType := AColType;
  FPrecision := APrecision;
  FScale := AScale;
end;

{$IFDEF FullOptions}
constructor TDataType.Create(AParentTable: TTable);
begin
  Create;
  FParentTable := AParentTable;
end;

constructor TDataType.Create(AParentTable: TTable; AColType: TColType; AParamType: TParamType; AParamPosition: string);
begin
  Create;
  FParentTable := AParentTable;
  FType := AColType;
  FParamType := AParamType;
  FParamPosition := AParamPosition;
end;

constructor TDataType.Create(AParentTable: TTable; AColType: TColType; AParamPosition: string);
begin
  Create;
  FParentTable := AParentTable;
  FType := AColType;
  FParamPosition := AParamPosition;
end;

constructor TDataType.Create(AParentTable: TTable; AColType: TColType; AParamType: TParamType);
begin
  Create;
  FParentTable := AParentTable;
  FType := AColType;
  FParamType := AParamType;
end;
{$ENDIF}

{ TDelete }
procedure TDelete.Database(ADBName: string);
begin
  FObjectType := otDatabase;
  FObjectName := ADBName;
end;

procedure TDelete.StoredFunc(AFunction: string);
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

constructor TAlter.Create;
begin
  FTable := nil;
end;

destructor TAlter.Destroy;
begin
  if Assigned(FTable) then
    FTable.Free;
  inherited;
end;

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

destructor TAlterTable.Destroy;
begin
  if Assigned(FColumn) then
    FColumn.Free;

  inherited;
end;

procedure TAlterTable.DropColumn(AColName: string);
begin
  FColName := AColName;
  FAlterMode := amDrop;
end;

{ TFunction }

procedure TFunction.AddBody(ABody: string);
begin
  FBody := ABody;
end;

function TFunction.AddParam(AName: string; ADataType: TDataType): TFunction;
begin
  FParams.Add(AName, ADataType);
  Exit(Self);
end;

function TFunction.ReturnType(AType: TDataType; AIsDeterministic: Boolean): TFunction;
begin
  FReturnType := AType;
  FIsDeterministic := AIsDeterministic;
  Exit(Self);
end;

constructor TFunction.Create(AName: string);
begin
  FName := AName;
  FParams := TObjectDictionary<string, TDataType>.Create([doOwnsValues]);
end;

destructor TFunction.Destroy;
begin
  FParams.Free;
  if Assigned(FReturnType) then
    FReturnType.Free;
  inherited;
end;

function TFunction.GetReturnType: TDataType;
begin
  Result := FReturnType;
end;

function TFunction.ReturnType(AType: TDataType): TFunction;
begin
  FReturnType := AType;
  Exit(Self);
end;

{ TProcedure }

procedure TProcedure.AddBody(ABody: string);
begin
  FBody := ABody;
end;

function TProcedure.AddParam(AName: string; ADataType: TDataType): TProcedure;
begin
  FParams.Add(AName, ADataType);
  Exit(Self);
end;

constructor TProcedure.Create(AName: string);
begin
  FName := AName;
  FParams := TObjectDictionary<string, TDataType>.Create([doOwnsValues]);
end;

destructor TProcedure.Destroy;
begin
  FParams.Free;
  inherited;
end;

{ TBaseDataType }

constructor TBaseDataType.Create(AName: string; AType: TColType; ASize: Integer; APrecision, AScale: Byte);
begin
  FName := AName;
  FType := AType;
  FSize := ASize;
  FPrecision := APrecision;
  FScale := AScale;
end;

constructor TBaseDataType.Create(AType: TColType; ASize: Integer);
begin
  FType := AType;
  FSize := ASize;
end;

constructor TBaseDataType.Create(AType: TColType);
begin
  FType := AType;
end;

constructor TBaseDataType.Create(AType: TColType; APrecision, AScale: Byte);
begin
  FType := AType;
  FPrecision := APrecision;
  FScale := AScale;
end;

{ TSQLServerDatabase }

function TSQLServerDatabase.Collation(ACollName: string): TSQLServerDatabase;
begin
  FCollation := ACollName;
  Exit(Self);
end;

constructor TSQLServerDatabase.Create(ADBName: string);
begin
  FDBName := ADBName;
end;

function TSQLServerDatabase.LdfFileGrowth(ASize: string): TSQLServerDatabase;
begin
  FLdfFileGrowth := ASize;
  Exit(Self);
end;

function TSQLServerDatabase.LdfFileName(AName: string): TSQLServerDatabase;
begin
  FLdfFileName := AName;
  Exit(Self);
end;

function TSQLServerDatabase.LdfMaxSize(ASize: string): TSQLServerDatabase;
begin
  FLdfMaxSize := ASize;
  Exit(Self);
end;

function TSQLServerDatabase.LdfSize(ASize: string): TSQLServerDatabase;
begin
  FLdfSize := ASize;
  Exit(Self);
end;

function TSQLServerDatabase.MdfMaxSize(ASize: string): TSQLServerDatabase;
begin
  FMdfMaxSize := ASize;
  Exit(Self);
end;

function TSQLServerDatabase.MdfFileGrowth(ASize: string): TSQLServerDatabase;
begin
  FMdfFileGrowth := ASize;
  Exit(Self);
end;

function TSQLServerDatabase.MdfFileName(AName: string): TSQLServerDatabase;
begin
  FMdfFileName := AName;
  Exit(Self);
end;

function TSQLServerDatabase.MdfSize(ASize: string): TSQLServerDatabase;
begin
  FMdfSize := ASize;
  Exit(Self);
end;

{ TMySQLDatabase }

function TMySQLDatabase.GetCharacterSet(ACharacterSet: string): TMySQLDatabase;
begin
  FCharacterSet := ACharacterSet;
  Exit(Self);
end;

function TMySQLDatabase.GetCollate(ACollateName: string): TMySQLDatabase;
begin
  FCollate := ACollateName;
  Exit(Self);
end;

end.



