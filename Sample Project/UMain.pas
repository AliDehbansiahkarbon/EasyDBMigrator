unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Generics.Collections,
  EasyDB.ConnectionManager.SQL, EasyDB.Migration.Base, EasyDB.Runner;

type
  TForm1 = class(TForm)
    btnDowngradeDatabase: TButton;
    btnUpgradeDatabase: TButton;
    btnAddMigrations: TButton;
    procedure btnDowngradeDatabaseClick(Sender: TObject);
    procedure btnUpgradeDatabaseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddMigrationsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Runner: TRunner;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.btnAddMigrationsClick(Sender: TObject);
begin
  Runner.MigrationList.Add(TMigration.Create('TbUsers', 202301010001, 'Ali', 'Task number #2701',
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('Alter table users add NewField varchar(50)');
  end,
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('Alter table Users drop column NewField');
  end
  ));


  Runner.MigrationList.Add(TMigration.Create('TbUsers', 202301010002, 'Ali', 'Task number #2702',
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('Alter table users add NewField2 int');
  end,
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('Alter table Users drop column NewField2');
  end
  ));
end;

procedure TForm1.btnDowngradeDatabaseClick(Sender: TObject);
begin
  Runner.DownGrade(202301010001);
end;

procedure TForm1.btnUpgradeDatabaseClick(Sender: TObject);
begin
  Runner.UpgradeDatabase;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  LvConnectionParams: TConnectionParams;
begin
  with LvConnectionParams do // Could be loaded from ini, registry or somewhere like that.
  begin
    Server := '192.168.212.1';
    LoginTimeout := 30000;
    UserName := 'sa';
    Pass := '1';
    DatabaseName := 'Library';
  end;

  Runner := TRunner.Create(LvConnectionParams);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Runner);
end;

end.
