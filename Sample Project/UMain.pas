unit UMain;

interface

uses
  System.StrUtils, System.SysUtils, Vcl.Forms, Vcl.Dialogs, System.TypInfo,
  System.Classes, Vcl.StdCtrls, Vcl.Controls,

  EasyDB.ConnectionManager.SQL,
  EasyDB.Migration.Base,
  EasyDB.MSSQLRunner,
  EasyDB.Logger, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    btnDowngradeDatabase: TButton;
    btnUpgradeDatabase: TButton;
    btnAddMigrations: TButton;
    edtVersion: TEdit;
    Label1: TLabel;
    mmoLog: TMemo;
    pbTotal: TProgressBar;
    procedure btnDowngradeDatabaseClick(Sender: TObject);
    procedure btnUpgradeDatabaseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddMigrationsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Runner: TSQLRunner;
    procedure OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
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

//  Runner.MigrationList.Add(TMigration.Create('TbUsers', 202301010002, 'Ali', 'Task number #2702',
//  procedure
//  begin
//    Runner.SQLConnection.ExecuteAdHocQuery('Alter table users add NewField2 int');
//  end,
//  procedure
//  begin
//    Runner.SQLConnection.ExecuteAdHocQuery('Alter table Users drop column NewField2');
//  end
//  ));
end;

procedure TForm1.btnDowngradeDatabaseClick(Sender: TObject);
begin
  Runner.DowngradeDatabase(StrToInt64(edtVersion.Text));
end;

procedure TForm1.btnUpgradeDatabaseClick(Sender: TObject);
begin
  if Runner.MigrationList.Count = 0 then
  begin
    ShowMessage('You should add at least one migration object.');
    Exit;
  end;

  Runner.UpgradeDatabase;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  LvConnectionParams: TConnectionParams;
begin
  with LvConnectionParams do // Could be loaded from ini, registry or somewhere else.
  begin
    Server := '192.168.212.1';
    LoginTimeout := 30000;
    UserName := 'sa';
    Pass := '1';
    DatabaseName := 'Library';
    Schema := 'dbo';
  end;

  {Use this line if you need local log}
  TLogger.Instance.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog; // Logger must be configured befor creating the Runner.

  {Use this line if you don't need local log}
  // TLogger.Instance.OnLog := OnLog;

  Runner := TSQLRunner.Create(LvConnectionParams);
  Runner.LogAllExecutions := True;
  //Runner.UseInternalThread := True;
  Runner.Progressbar := pbTotal;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Runner.Free;
end;

procedure TForm1.OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
begin
  // This method will run anyway if yo u assigne it and ignores LocalLog parameter.
  //...
  //...
  // ShowMessage(AException);
  // Do anything you need here with the log data, log on Graylog, Terlegram, email, etc...

  mmoLog.Lines.Add('========== ' + DateTimeToStr(Now) + ' ==========');
  mmoLog.Lines.Add('Action Type: ' + GetEnumName(TypeInfo(TActionTypes), Ord(AActionType)));
  mmoLog.Lines.Add('Exception: ' + AException);
  mmoLog.Lines.Add('Class Name: ' + IfThen(AClassName.IsEmpty, 'N/A', AClassName));
  mmoLog.Lines.Add('Version: ' + IfThen(AVersion = 0, 'N/A', IntToStr(AVersion)));
  mmoLog.Lines.Add('');
end;

end.
