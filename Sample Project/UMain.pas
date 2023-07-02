unit UMain;

interface

uses
  System.StrUtils, System.SysUtils, Vcl.Forms, Vcl.Dialogs, System.TypInfo,
  System.Classes, Vcl.StdCtrls, Vcl.Controls, Vcl.ComCtrls,

  EasyDB.Core,
  EasyDB.Migration,
  EasyDB.MSSQLRunner,
  EasyDB.Logger;

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
  Runner.MigrationList.Add(TMigration.Create('TbUsers', 202301010001, 'Ali', 'Create table Users, #2701',
  procedure
  var sql: string;
  begin
    sql := 'If Not Exists( Select * From sysobjects Where Name = ''TbUsers'' And xtype = ''U'') ' + #10
     + '    Create Table TbUsers( ' + #10
     + '    	ID Int Primary key Identity(1, 1) Not null, ' + #10
     + '    	UserName Nvarchar(100), ' + #10
     + '    	Pass Nvarchar(50) ' + #10
     + '    );';

    Runner.SQLConnection.ExecuteAdHocQuery(sql);
  end,
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('DROP TABLE TbUsers');
  end
  ));

  //============================================
  Runner.MigrationList.Add(TMigration.Create('TbUsers', 202301010002, 'Ali', 'Task number #2701',
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD NewField2 VARCHAR(50)');
  end,
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP COLUMN NewField2');
  end
  ));

  //============================================

  Runner.MigrationList.Add(TMigration.Create('TbUsers', 202301010003, 'Ali', 'Task number #2702',
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD NewField3 INT');
  end,
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP COLUMN NewField3');
  end
  ));

  //============================================

  Runner.MigrationList.Add(TMigration.Create('TbCustomers', 202301010003, 'Alex', 'Task number #2702',
  procedure
  var sql: string;
  begin
    sql := 'If Not Exists( Select * From sysobjects Where Name = ''TbCustomers'' And xtype = ''U'') ' + #10
     + '    Create Table TbCustomers( ' + #10
     + '    	ID Int Primary key Identity(1, 1) Not null, ' + #10
     + '    	Name Nvarchar(100), ' + #10
     + '    	Family Nvarchar(50) ' + #10
     + '    );';
    Runner.SQLConnection.ExecuteAdHocQuery(sql);
  end,
  procedure
  begin
    Runner.SQLConnection.ExecuteAdHocQuery('DROP TABLE TbCustomers');
  end
  ));
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
  TLogger.Instance.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog; // Logger must be configured before creating the Runner.

  {Use this line if you don't need local log}
  // TLogger.Instance.OnLog := OnLog;

  Runner := TSQLRunner.Create(LvConnectionParams);
  Runner.AddConfig.LogAllExecutions(True).UseInternalThread(True).SetProgressbar(pbTotal).RollBackAllByAnyError(True); //each part This line is Optional
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
