unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, System.StrUtils, System.TypInfo,

  EasyDB.Core,
  EasyDB.Logger,
  EasyDB.Migration,
  EasyDB.FirebirdRunner;

type
  TfrmMain = class(TForm)
    Label1: TLabel;
    btnDowngradeDatabase: TButton;
    btnUpgradeDatabase: TButton;
    btnAddMigrations: TButton;
    edtVersion: TEdit;
    mmoLog: TMemo;
    pbTotal: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure btnUpgradeDatabaseClick(Sender: TObject);
    procedure btnDowngradeDatabaseClick(Sender: TObject);
    procedure btnAddMigrationsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Runner: TFirebirdRunner;
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
    LvScript := 'CREATE TABLE TbUsers ( ' + #10
     + '    ID INT NOT NULL PRIMARY KEY, ' + #10
     + '    UserName VARCHAR(100), ' + #10
     + '    Pass VARCHAR(100) ' + #10
     + '    );';

    with Runner.Firebird do
    begin
      if not DoesTbExist('TbUsers') then
        ExecuteAdHocQuery(LvScript);
    end;
  end,
  procedure
  begin
    with Runner.Firebird do
    begin
      if DoesTbExist('TbUsers') then
        ExecuteAdHocQuery('DROP TABLE TbUsers');
    end;
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbUsers', 202301010002, 'Ali', 'Task number #2701',
  procedure
  begin
    Runner.Firebird.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD NewField2 VARCHAR(50)');
  end,
  procedure
  begin
    Runner.Firebird.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP NewField2');
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbUsers', 202301010003, 'Ali', 'Task number #2702',
  procedure
  begin
    Runner.Firebird.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD NewField3 INT');
  end,
  procedure
  begin
    Runner.Firebird.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP NewField3');
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbCustomers', 202301010003, 'Alex', 'Task number #2702',
  procedure
  var LvScript: string;
  begin
    with Runner.Firebird do
    begin
      if not DoesTbExist('TbCustomers') then
      begin
        LvScript := 'CREATE TABLE TbCustomers ( ' + #10
        + ' ID INT NOT NULL PRIMARY KEY, ' + #10
        + ' Name VARCHAR(100), ' + #10
        + ' Family VARCHAR(100) ' + #10
        + ' );';
        ExecuteAdHocQuery(LvScript);
      end;
    end;
  end,
  procedure
  begin
    with Runner.Firebird do
    begin
      if DoesTbExist('TbCustomers') then
        Runner.Firebird.ExecuteAdHocQuery('DROP TABLE TbCustomers');
    end;
  end
  ));
end;

procedure TfrmMain.btnDowngradeDatabaseClick(Sender: TObject);
begin
  Runner.DowngradeDatabase(StrToInt64Def(edtVersion.Text, 0));
end;

procedure TfrmMain.btnUpgradeDatabaseClick(Sender: TObject);
begin
  Runner.UpgradeDatabase;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  LvConnectionParams: TFirebirdConnectionParams;
begin
  with LvConnectionParams do // The information can be sourced from an ini file, registry or other location.
  begin
    Host := 'localhost';
    UserName := 'sysdba';
    Pass := '1';
    Database := 'C:\Temp\Library.fdb';
  end;

  Runner := TFirebirdRunner.Create(LvConnectionParams);
  Runner.Config
    .LogAllExecutions(True) // Optional
    .UseInternalThread(True) //Optional
    .SetProgressbar(pbTotal); //Optional

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
