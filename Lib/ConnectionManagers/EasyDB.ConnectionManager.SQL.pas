unit EasyDB.ConnectionManager.SQL;

interface

uses
  EasyDB.ConnectionManager.Base;

type
  TSQLConnection = class(TConnection)
  private
    FServer: string;
    FUserName: string;
    FPass: string;
  public
    Constructor Create(AServer: string; APort: Integer; AUserName, APass: string);
    Destructor Destroy; override;

    function GetConnection: string; override;
    function Connect: Boolean; override;
    function ExecuteAdHocQuery(AScript: string): Boolean; override;
    procedure SetConnectionParam; override;

    property Server: string read FServer write FServer;
    property UserName: string read FUserName write FUserName;
    property Pass: string read FPass write FPass;
  end;

implementation

{ TSQLConnection }

function TSQLConnection.Connect: Boolean;
begin
//
end;

constructor TSQLConnection.Create(AServer: string; APort: Integer; AUserName, APass: string);
begin
//
end;

destructor TSQLConnection.Destroy;
begin
//
  inherited;
end;

function TSQLConnection.ExecuteAdHocQuery(AScript: string): Boolean;
begin
//
end;

function TSQLConnection.GetConnection: string;
begin
//
end;

procedure TSQLConnection.SetConnectionParam;
begin
  inherited;
//
end;

end.
