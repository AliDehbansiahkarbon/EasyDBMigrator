unit UHelper;

interface
uses
  EasyDB.ConnectionManager.Oracle;

  function Oracle: TOracleConnection;

implementation

function Oracle: TOracleConnection;
begin
  Result := TOracleConnection.Instance;
end;

end.
