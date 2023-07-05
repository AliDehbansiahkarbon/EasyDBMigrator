unit EasyDB.ORM;

interface
uses
  Data.FmtBcd, Data.SqlTimSt, System.Generics.Collections;

type
  Largeint = Int64;
  TTable = class;

  TDataType = class
  private
    FColType: TTypeKind;
  public
    function NotNullable: TTable;
    function Nullable: TTable;
  end;

  TColumn = class
    FColName: string;

    FColSize: Integer;
  public
    function AsBCD: TDataType;
    function AsBoolean: TDataType;
    function AsCurrency: TDataType;
    function AsDateTime: TDataType;
    function AsSQLTimeStamp: TDataType;
    function AsSQLTimeStampOffset: TDataType;
    function AsSingle: TDataType;
    function AsFloat: TDataType;
    function AsExtended: TDataType;
    function AsInteger: TDataType;
    function AsLargeInt: TDataType;
    function AsLongWord: TDataType;
    function AsString: TDataType;
    function AsWideString: TDataType;
  end;

  TTable = class
  private
    FTableName: string;
    FColumnList: TList<TColumn>;
    constructor Create;
    destructor Destroy; override;
  public
    function WithIdColumn(AColName: string): TTable;
    function WithColumn(AColName: string): TColumn;
  end;

  TCreate = class
  private
    FTable: TTable;
  public
    function Table: TTable;
  end;

  TAlter = class
  private
    FTable: TTable;
  public
    function Table: TTable;
  end;

  ORM = class
  private
    FCreateList: TObjectList<TCreate>;
  public
    constructor NewORM;
    destructor Destroy; override;
    function Create: TCreate;
  end;

implementation

end.
