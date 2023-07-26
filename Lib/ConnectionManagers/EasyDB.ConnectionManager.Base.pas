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

    function ExecuteAdHocQuery(AScript: string): Boolean; virtual; abstract;
    function ExecuteScriptFile(AScriptPath: string; ADelimiter: string): Boolean; virtual; abstract;
  end;

implementation

end.
