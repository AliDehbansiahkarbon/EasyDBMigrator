unit EasyDB.Migration.Base;

interface
uses
  System.SysUtils, System.Classes, EasyDB.Attribute, Vcl.Dialogs, System.Rtti;

type

  TMigration = class
  private
    FMigration: TMigration;
    FAuthor: string;
    FDescription: string;
    FUp: TProc;
    FDown: TProc;
  public
    Constructor Create(const AVersion: Integer; const AAuthor: string; const ADescription: string; const AUp, ADown: TProc);
    procedure Upgrade; virtual;
    procedure Downgrade; virtual;
    function AttribEntityName: string;
    function AttribVersion: Integer;
  end;

implementation

{ TMigration }

function TMigration.AttribEntityName: string;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LAttr: TCustomAttribute;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(TypeInfo(TMigration));
    for LAttr in LType.GetAttributes() do
    begin
      if LAttr is TCustomMigrationAttribute then
        Result := TCustomMigrationAttribute(LAttr).EntityName;
    end;
  finally
    LContext.Free;
  end;
end;

function TMigration.AttribVersion: Integer;
var
  LContext: TRttiContext;
  LType: TRttiType;
  LAttr: TCustomAttribute;
begin
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(TypeInfo(TMigration));
    for LAttr in LType.GetAttributes() do
    begin
      if LAttr is TCustomMigrationAttribute then
        Result := TCustomMigrationAttribute(LAttr).Version;
    end;
  finally
    LContext.Free;
  end;
end;

constructor TMigration.Create(const AVersion: Integer; const AAuthor: string; const ADescription: string; const AUp, ADown: TProc);
begin
  FVersion := AVersion;
  FAuthor := AAuthor;
  FDescription := ADescription;
  FUp := AUp;
  FDown := ADown;
  FMigration := Self;
end;

procedure TMigration.Downgrade;
begin
  if Assigned(FDown) then
    FDown;
end;

procedure TMigration.Upgrade;
begin
  if Assigned(FUp) then
    FUp;
end;

end.
