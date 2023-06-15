unit EasyDB.Attribute;

interface

type

  TCustomMigrationAttribute = class(TCustomAttribute)
  private
    FEntityName: string;
    FVersion: Integer;
    FDescription: string;
    FAuthor: string;
  public
    constructor Create(const AEntityName: string; const AVersion: Integer; const ADescription: string; const AAuthor: string);
    property Version: Integer read FVersion;
    property EntityName: string read FEntityName;
  end;

implementation

{ CustomMigrationAttribute }

constructor TCustomMigrationAttribute.Create(const AEntityName: string; const AVersion: Integer; const ADescription: string; const AAuthor: string);
begin
  FEntityName := AEntityName;
  FVersion := AVersion;
  FDescription := ADescription;
  FAuthor := AAuthor;
end;

end.
