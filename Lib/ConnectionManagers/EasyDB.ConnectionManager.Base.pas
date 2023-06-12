unit EasyDB.ConnectionManager.Base;

interface

type
  IConnection = interface
    ['{38BED704-84A9-43A6-848C-11805C9A077A}']

    function GetConnection: string;
    procedure SetConnectionParam;
    function Connect: Boolean;
  end;

  TConnection = class
  private
    FConnectionString: string;
    FConnectionTimeOut: Integer;// ms
    FCommanTimeOut: Integer; // ms
  public
    function GetConnection: string; virtual; abstract;
    function Connect: Boolean; virtual; abstract;
    function ExecuteAdHocQuery(AScript: string): Boolean; virtual; abstract;
    procedure SetConnectionParam; virtual; abstract;
  end;

implementation

end.
