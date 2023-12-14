{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
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
