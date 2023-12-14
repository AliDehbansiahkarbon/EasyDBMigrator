{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
unit EasyDB.ConnectionManager.Base;

interface

uses
  EasyDB.Logger;

type
  TConnection = class
  public
    function GetConnectionString: string; virtual; abstract;
    function Connect: Boolean; virtual; abstract;
    function Logger: TLogger; virtual; abstract;

    procedure ExecuteAdHocQuery(AScript: string); virtual; abstract;
    procedure ExecuteScriptFile(AScriptPath: string; ADelimiter: string); virtual; abstract;
  end;

implementation
end.
