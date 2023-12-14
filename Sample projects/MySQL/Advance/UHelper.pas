{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
unit UHelper;

interface
uses
  EasyDB.ConnectionManager.MySQL;

  function MySQL: TMySQLConnection;

implementation

function MySQL: TMySQLConnection;
begin
  Result := TMySQLConnection.Instance;
end;

end.
