unit EasyDB.Migration.Attrib;

interface
uses
  System.SysUtils, System.Rtti,
  EasyDB.Attribute,
  EasyDB.Logger;

type
  TMigrationEx = class // You must use class level attributes with this type and you should implement Upgrade and Downgrade procedures manually;
  protected
    function AttribEntityName: string;
    function AttribVersion: Integer;
    function Logger: TLogger;
  public
    procedure Upgrade; virtual; abstract;
    procedure Downgrade; virtual; abstract;
  end;

implementation

{ TMigrationEx }

function TMigrationEx.AttribEntityName: string;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LAttr: TCustomAttribute;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(TypeInfo(TMigrationEx));
    for LAttr in LType.GetAttributes() do
    begin
      if LAttr is TCustomMigrationAttribute then
        Result := TCustomMigrationAttribute(LAttr).EntityName;
    end;
  finally
    LContext.Free;
  end;
end;

function TMigrationEx.AttribVersion: Integer;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LAttr: TCustomAttribute;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(TypeInfo(TMigrationEx));
    for LAttr in LType.GetAttributes() do
    begin
      if LAttr is TCustomMigrationAttribute then
        Result := TCustomMigrationAttribute(LAttr).Version;
    end;
  finally
    LContext.Free;
  end;
end;

function TMigrationEx.Logger: TLogger;
begin
  Result := TLogger.Instance;
end;

end.
