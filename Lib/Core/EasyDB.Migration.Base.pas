unit EasyDB.Migration.Base;

interface
uses
  System.SysUtils, System.Classes, EasyDB.Attribute, Vcl.Dialogs, System.Rtti;

type
  IMigration = interface
    ['{E43EA412-E62C-42F7-9087-ED75BDADC5CA}']
  end;

  TMigration = class(TInterfacedObject, IMigration) // You don't need class level attribute and you can use anonymouse method to define Upgrade and Downgrade procedures.
  private
    FUp: TProc;
    FDown: TProc;
    FVersion: Int64;
    FAuthor: string;
    FDescription: string;
    FEntityName: string;
  public
    Constructor Create(const AEntityName: string; const AVersion: Int64; AAuthor: string; const ADescription: string; const AUp, ADown: TProc);
    procedure Upgrade; virtual;
    procedure Downgrade; virtual;

    property EntityName: string read FEntityName;
    property Version: Int64 read FVersion;
  end;

//  TMigrationEx = class(TInterfacedObject, IMigration) // You must use class level attributes with this type and you should implement Upgrade and Downgrade procedures manually;
//  public
//    procedure Upgrade; virtual; abstract;
//    procedure Downgrade; virtual; abstract;
//    function AttribEntityName: string;
//    function AttribVersion: Integer;
//  end;

implementation

{ TMigration }

constructor TMigration.Create(const AEntityName: string; const AVersion: Int64; AAuthor: string; const ADescription: string; const AUp, ADown: TProc);
begin
  FVersion := AVersion;
  FAuthor := AAuthor;
  FDescription := ADescription;
  FEntityName := AEntityName;
  FUp := AUp;
  FDown := ADown;
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


{ TMigrationEx }
{
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
  }
end.
