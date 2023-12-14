{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
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
  TfrmMain = class(TForm)
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
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnAddMigrationsClick(Sender: TObject);
begin
  Runner.Clear;
  Runner.Add(TMigration.Create('TbUsers', 202301010001, 'Ali', 'Create table Users, #2701',
  procedure
  var LvScript: string;
  begin
    LvScript := 'If Not Exists( Select * From sysobjects Where Name = ''TbUsers'' And xtype = ''U'') ' + #10
     + '    Create Table TbUsers( ' + #10
     + '    	ID Int Primary key Identity(1, 1) Not null, ' + #10
     + '    	UserName Nvarchar(100), ' + #10
     + '    	Pass Nvarchar(50) ' + #10
     + '    );';

    Runner.SQL.ExecuteAdHocQuery(LvScript);
  end,
  procedure
  begin
    Runner.SQL.ExecuteAdHocQuery('DROP TABLE TbUsers');
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbUsers', 202301010002, 'Ali', 'Task number #2702',
  procedure
  begin
    Runner.SQL.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD NewField2 VARCHAR(50)');
  end,
  procedure
  begin
    Runner.SQL.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP COLUMN NewField2');
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbUsers', 202301010003, 'Ali', 'Task number #2703',
  procedure
  begin
    Runner.SQL.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD NewField3 INT');
  end,
  procedure
  begin
    Runner.SQL.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP COLUMN NewField3');
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbCustomers', 202301010003, 'Alex', 'Task number #2704',
  procedure
  var LvScript: string;
  begin
    LvScript := 'If Not Exists( Select * From sysobjects Where Name = ''TbCustomers'' And xtype = ''U'') ' + #10
     + '    Create Table TbCustomers( ' + #10
     + '    	ID Int Primary key Identity(1, 1) Not null, ' + #10
     + '    	Name Nvarchar(100), ' + #10
     + '    	Family Nvarchar(50) ' + #10
     + '    );';
    Runner.SQL.ExecuteAdHocQuery(LvScript);
  end,
  procedure
  begin
    Runner.SQL.ExecuteAdHocQuery('DROP TABLE TbCustomers');
  end
  ));
end;

procedure TfrmMain.btnDowngradeDatabaseClick(Sender: TObject);
begin
  Runner.DowngradeDatabase(StrToInt64(edtVersion.Text));
end;

procedure TfrmMain.btnUpgradeDatabaseClick(Sender: TObject);
begin
  if Runner.MigrationList.Count = 0 then
  begin
    ShowMessage('You should add at least one migration object.');
    Exit;
  end;

  Runner.UpgradeDatabase;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  LvConnectionParams: TSqlConnectionParams;
begin
  with LvConnectionParams do // The information can be sourced from an ini file, registry or other location.
  begin
    Server := '192.168.212.1';
    LoginTimeout := 30000;
    UserName := 'sa';
    Pass := '1';
    DatabaseName := 'Library';
    Schema := 'dbo';
  end;

  Runner := TSQLRunner.Create(LvConnectionParams);
  Runner.Config
    .LogAllExecutions(True)// Optional
    .UseInternalThread(True)// Optional - executes asynchronously and doesn't block the screen
    .SetProgressbar(pbTotal)// Optional
    .DelayedExecution(100); //Just for test

//    .SetProgressbar(pbTotal, 10)// This will do the real job faster
//    .DelayedExecution(1); //This will do the real job faster

  {Use this line if you don't need local log}
  Runner.AddLogger.OnLog := OnLog;

  {Use this line if you need local log}
  //Runner.AddLogger.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Runner.Free;
end;

procedure TfrmMain.OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
begin
  // This method will run anyway even if you ignore the Local Log file.
  //...
  //...
  // ShowMessage(AException);
  // Do anything you need here with the log data, log on Graylog, Telegram, email, etc...

  mmoLog.Lines.BeginUpdate;
  mmoLog.Lines.Add('========== ' + DateTimeToStr(Now) + ' ==========');
  mmoLog.Lines.Add('Action Type: ' + GetEnumName(TypeInfo(TActionTypes), Ord(AActionType)));
  mmoLog.Lines.Add('Msg: ' + AException);
  mmoLog.Lines.Add('Class Name: ' + IfThen(AClassName.IsEmpty, 'N/A', AClassName));
  mmoLog.Lines.Add('Version: ' + IfThen(AVersion = 0, 'N/A', IntToStr(AVersion)));
  mmoLog.Lines.EndUpdate;
  mmoLog.Lines.Add('');
end;

end.
