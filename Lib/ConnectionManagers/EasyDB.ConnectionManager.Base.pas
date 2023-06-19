unit EasyDB.ConnectionManager.Base;

interface

uses
  EasyDB.Logger;

type
  TConnection = class
  private
    FConnectionString: string;
    FConnectionTimeOut: Integer;// ms
    FCommanTimeOut: Integer; // ms
  public
    function GetConnectionString: string; virtual; abstract;
    function Connect: Boolean; virtual; abstract;
    function ExecuteAdHocQuery(AScript: string): Boolean; virtual; abstract;
    function ActivateLog: TConnection; virtual; abstract;
    function Logger: TLogger; virtual; abstract;
  end;

implementation

end.
