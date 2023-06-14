unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections,
  EasyDB.ConnectionManager.SQL;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    ConnectionParams: TConnectionParams;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  EasyDB.Migration.Base, EasyDB.Runner;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  var LvScript: string := 'update Book set price = 1000 where id = 6';
  TSQLConnection.Instance.SetConnectionParam(ConnectionParams).ConnectEx.ExecuteAdHocQuery(LvScript);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  LvUsersMigration: TMigration;
  LvRuner: TRunner;
  LvSQL: TSQLConnection;
begin
  LvRuner := TRunner.Create;
  LvSQL := TSQLConnection.Instance.SetConnectionParam(ConnectionParams).ConnectEx;
  try
    LvRuner.MigrationList.Add(TMigration.Create(202301010001, 'Ali', 'Task number #2701',
    procedure
    begin
      LvSQL.ExecuteAdHocQuery('Alter table users add NewField varchar(50)');
    end,
    procedure
    begin
      LvSQL.ExecuteAdHocQuery('Alter table Users drop column NewField');
    end
    ));


    LvRuner.MigrationList.Add(TMigration.Create(202301010002, 'Ali', 'Task number #2702',
    procedure
    begin
      LvSQL.ExecuteAdHocQuery('Alter table users add NewField2 int');
    end,
    procedure
    begin
      LvSQL.ExecuteAdHocQuery('Alter table Users drop column NewField2');
    end
    ));

    LvRuner.UpgradeDatabase;
  finally
    LvRuner.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with ConnectionParams do
  begin
    Server := '192.168.212.1';
    LoginTimeout := 30000;
    UserName := 'sa';
    Pass := '1';
    DatabaseName := 'Library';
  end;

end;

end.
