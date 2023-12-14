{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
unit UHelper;

interface
uses
  EasyDB.ConnectionManager.PostgreSQL;

  function PG: TPgConnection;

implementation

function PG: TPgConnection;
begin
  Result := TPgConnection.Instance;
end;

end.
