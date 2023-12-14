{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
unit EasyDB.Attribute;

interface

type
  TCustomMigrationAttribute = class(TCustomAttribute)
  public
    EntityName: string;
    Version: Int64;
    Description: string;
    Author: string;
    constructor Create(const AEntityName: string; const AVersion: Int64; const ADescription: string; const AAuthor: string);
  end;

implementation

{ CustomMigrationAttribute }

constructor TCustomMigrationAttribute.Create(const AEntityName: string; const AVersion: Int64; const ADescription: string; const AAuthor: string);
begin
  EntityName := AEntityName;
  Version := AVersion;
  Description := ADescription;
  Author := AAuthor;
end;

end.
