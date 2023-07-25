unit UHelper;

interface
uses
  EasyDB.ConnectionManager.MariaDB;

  function MariaDB: TMariaDBConnection;

implementation

function MariaDB: TMariaDBConnection;
begin
  Result := TMariaDBConnection.Instance;
end;

end.
