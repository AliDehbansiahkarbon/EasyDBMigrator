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
