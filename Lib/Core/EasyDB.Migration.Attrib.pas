unit EasyDB.Migration.Attrib;

interface
uses
  System.SysUtils, System.Rtti, Vcl.Dialogs, System.Generics.Collections,
  EasyDB.Attribute,
  EasyDB.Migration.Base,
  EasyDB.Logger;

type
  {$M+}
  {$RTTI EXPLICIT PROPERTIES([vcPrivate, vcPublic, vcProtected, vcPublished])}
  TMigrationEx = class(TMigrationBase) // You must use class level attributes with this type and you should implement Upgrade and Downgrade procedures manually;
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

constructor TMigrationEx.Create(AUp, ADown: TProc);
begin
  FUp := AUp;
  FDown := ADown;
end;

constructor TMigrationEx.Create;
begin
  inherited;
end;

function TMigrationEx.GetAttribEntityName: string;
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

function TMigrationEx.GetAttribVersion: Int64;
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

function TMigrationEx.GetAttribAuthor: string;
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

function TMigrationEx.GetAttribDescription: string;
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

function TMigrationEx.Logger: TLogger;
begin
  Result := TLogger.Instance;
end;

procedure TMigrationEx.Upgrade;
begin
  if Assigned(FUp) then
    FUp;
end;

procedure TMigrationEx.Downgrade;
begin
  if Assigned(FDown) then
    FDown;
end;

end.
