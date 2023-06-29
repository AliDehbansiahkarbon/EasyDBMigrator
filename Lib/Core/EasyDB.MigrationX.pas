unit EasyDB.MigrationX;

interface
uses
  System.SysUtils, System.Rtti, Vcl.Dialogs, System.Generics.Collections,
  EasyDB.Core,
  EasyDB.Attribute,
  EasyDB.Migration,
  EasyDB.Logger;

type
  {$M+}
  {$RTTI EXPLICIT PROPERTIES([vcPrivate, vcPublic, vcProtected, vcPublished])}
  TMigrationX = class(TMigrationBase) // You must use class level attributes with this type and you should implement Upgrade and Downgrade procedures manually;
  private
    FUp: TProc;
    FDown: TProc;
    function GetAttribEntityName: string;
    function GetAttribVersion: Int64;
    function GetAttribAuthor: string;
    function GetAttribDescription: string;
  public
    constructor Create; overload;
    constructor Create(AUp, ADown: TProc); overload;
    function Logger: TLogger;
    procedure Upgrade; virtual;
    procedure Downgrade; virtual;

    property AttribEntityName: string read GetAttribEntityName;
    property AttribAuthor: string read GetAttribAuthor;
    property AttribDescription: string read GetAttribDescription;
  published
    property AttribVersion: Int64 read GetAttribVersion; // published to be accessible in GetPropValue function.
  end;

implementation

{ TMigrationEx }

constructor TMigrationX.Create(AUp, ADown: TProc);
begin
  FUp := AUp;
  FDown := ADown;
end;

constructor TMigrationX.Create;
begin
  inherited;
end;

function TMigrationX.GetAttribEntityName: string;
var
  LvContext: TRttiContext;
  LvType: TRttiType;
  LvAttr: TCustomAttribute;
begin
  if HasAttribDic then
    Result := HiddenAttribDic.Items['EntityName']
  else
  begin
    LvContext := TRttiContext.Create;
    try
      LvType := LvContext.GetType(Self.ClassType);
      for LvAttr in LvType.GetAttributes do
      begin
        if LvAttr is TCustomMigrationAttribute then
          Result := TCustomMigrationAttribute(LvAttr).EntityName;
      end;
    finally
      LvContext.Free;
    end;
  end;
end;

function TMigrationX.GetAttribVersion: Int64;
var
  LvContext: TRttiContext;
  LvType: TRttiType;
  LvAttr: TCustomAttribute;
begin
  Result := -1;

  if HasAttribDic then
    Result := HiddenAttribDic.Items['Version']
  else
  begin
    LvContext := TRttiContext.Create;
    try
      LvType := LvContext.GetType(Self.ClassType);
      for LvAttr in LvType.GetAttributes do
      begin
        if LvAttr is TCustomMigrationAttribute then
          Result := TCustomMigrationAttribute(LvAttr).Version;
      end;
    finally
      LvContext.Free;
    end;
  end;
end;

function TMigrationX.GetAttribAuthor: string;
var
  LvContext: TRttiContext;
  LvType: TRttiType;
  LvAttr: TCustomAttribute;
begin
  if HasAttribDic then
    Result := HiddenAttribDic.Items['Author']
  else
  begin
    LvContext := TRttiContext.Create;
    try
      LvType := LvContext.GetType(Self.ClassType);
      for LvAttr in LvType.GetAttributes do
      begin
        if LvAttr is TCustomMigrationAttribute then
          Result := TCustomMigrationAttribute(LvAttr).Author;
      end;
    finally
      LvContext.Free;
    end;
  end;
end;

function TMigrationX.GetAttribDescription: string;
var
  LvContext: TRttiContext;
  LvType: TRttiType;
  LvAttr: TCustomAttribute;
begin
  if HasAttribDic then
    Result := HiddenAttribDic.Items['Description']
  else
  begin
    LvContext := TRttiContext.Create;
    try
      LvType := LvContext.GetType(Self.ClassType);
      for LvAttr in LvType.GetAttributes do
      begin
        if LvAttr is TCustomMigrationAttribute then
          Result := TCustomMigrationAttribute(LvAttr).Description;
      end;
    finally
      LvContext.Free;
    end;
  end;
end;

function TMigrationX.Logger: TLogger;
begin
  Result := TLogger.Instance;
end;

procedure TMigrationX.Upgrade;
begin
  if Assigned(FUp) then
    FUp;
end;

procedure TMigrationX.Downgrade;
begin
  if Assigned(FDown) then
    FDown;
end;

end.
