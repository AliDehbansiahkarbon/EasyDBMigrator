{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
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
