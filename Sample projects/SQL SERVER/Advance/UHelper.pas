unit UHelper;

interface
uses
  EasyDB.ConnectionManager.SQL;

  function SQL: TSQLConnection;

implementation

function SQL: TSQLConnection;
begin
  Result := TSQLConnection.Instance;
end;

end.
