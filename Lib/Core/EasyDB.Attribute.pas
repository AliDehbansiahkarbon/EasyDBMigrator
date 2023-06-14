unit EasyDB.Attribute;

interface

type

  TCustomMigrationAttribute = class(TCustomAttribute)
  private
    FEntityName: string;
    FVersion: Integer;
    FDescription: string;
  public
    constructor Create(const AEntityName: string; const AVersion: Integer; const ADescription: string);
    property Version: Integer read FVersion;
    property EntityName: string read FEntityName;
  end;

implementation

{ CustomMigrationAttribute }

constructor TCustomMigrationAttribute.Create(const AEntityName: string; const AVersion: Integer; const ADescription: string);
begin
  FEntityName := AEntityName;
  FVersion := AVersion;
  FDescription := ADescription;
end;

end.
