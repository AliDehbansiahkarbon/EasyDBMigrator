{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
unit UHelper;

interface
uses
  EasyDB.ConnectionManager.Firebird;

  function Firebird: TFirebirdConnection;

implementation

function Firebird: TFirebirdConnection;
begin
  Result := TFirebirdConnection.Instance;
end;

end.
