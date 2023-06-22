unit EasyDB.Migration.Attrib;

interface
uses
  System.SysUtils, System.Rtti, Vcl.Dialogs,
  EasyDB.Attribute,
  EasyDB.Migration.Base,
  EasyDB.Logger;

type
  {$RTTI EXPLICIT PROPERTIES([vcPrivate, vcPublic, vcProtected, vcPublished])}
  TMigrationEx = class(TMigrationBase) // You must use class level attributes with this type and you should implement Upgrade and Downgrade procedures manually;
  private
    function GetAttribEntityName: string;
    function GetAttribVersion: Int64;
    function GetAttribAuthor: string;
    function GetAttribDescription: string;
  public
    function Logger: TLogger;
    procedure Upgrade; virtual; abstract;
    procedure Downgrade; virtual; abstract;

    property AttribEntityName: string read GetAttribEntityName;
    property AttribAuthor: string read GetAttribAuthor;
    property AttribDescription: string read GetAttribDescription;
  published
    property AttribVersion: Int64 read GetAttribVersion; // published to be accessible in GetPropValue function.
  end;

implementation

uses
  UUsers;

{ TMigrationEx }

function TMigrationEx.GetAttribEntityName: string;
var
  LvContext: TRttiContext;
  LvType: TRttiType;
  LvAttr: TCustomAttribute;
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

function TMigrationEx.GetAttribVersion: Int64;
var
  LvContext: TRttiContext;
  LvType: TRttiType;
  LvAttr: TCustomAttribute;
begin
  Result := -1;
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

function TMigrationEx.GetAttribAuthor: string;
var
  LvContext: TRttiContext;
  LvType: TRttiType;
  LvAttr: TCustomAttribute;
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

function TMigrationEx.GetAttribDescription: string;
var
  LvContext: TRttiContext;
  LvType: TRttiType;
  LvAttr: TCustomAttribute;
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

function TMigrationEx.Logger: TLogger;
begin
  Result := TLogger.Instance;
end;

end.
