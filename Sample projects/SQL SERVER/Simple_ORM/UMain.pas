unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,System.StrUtils, System.TypInfo,

  EasyDB.Core,
  EasyDB.Migration,
  EasyDB.MSSQLRunner,
  EasyDB.ORM.Core,
  EasyDB.ORM,
  EasyDB.Logger;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    btnDowngradeDatabase: TButton;
    btnUpgradeDatabase: TButton;
    btnAddMigrations: TButton;
    edtVersion: TEdit;
    mmoLog: TMemo;
    pbTotal: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure btnAddMigrationsClick(Sender: TObject);
    procedure btnUpgradeDatabaseClick(Sender: TObject);
    procedure btnDowngradeDatabaseClick(Sender: TObject);
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
var
  ORM: TORM;
begin
  Runner.Clear;
  ORM := TORM.GetInstance(ttSQLServer);
  Runner.ORM := ORM;

  Runner.Add(TMigration.Create('TbUsers', 202301010001, 'Ali', 'Create table Users, #2701',
  procedure
  begin
    with ORM do
    begin
      Create.Table('TbUsers').WithIdColumn
      .WithColumn('UserName').AsNvarchar(100).Nullable
      .WithColumn('Pass').AsNvarchar(50).Nullable;

      SubmitChanges;
    end;
  end,
  procedure
  begin
    ORM.Delete.Table('TbUsers');
    ORM.SubmitChanges;
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbUsers', 202301010002, 'Ali', 'Task number #2701',
  procedure
  begin
    ORM.Alter.Table('TbUsers').AddColumn('NewField2').AsVarchar(50).Nullable;
    ORM.SubmitChanges;
  end,
  procedure
  begin
    ORM.Alter.Table('TbUsers').DropColumn('NewField2');
    ORM.SubmitChanges;
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbUsers', 202301010003, 'Ali', 'Task number #2702',
  procedure
  begin
    ORM.Alter.Table('TbUsers').AddColumn('NewField3').AsInt.Nullable;
    ORM.SubmitChanges;
  end,
  procedure
  begin
    ORM.Alter.Table('TbUsers').DropColumn('NewField3');
    ORM.SubmitChanges;
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbCustomers', 202301010003, 'Alex', 'Task number #2702',
  procedure
  begin
    with ORM do
    begin
      Create.Table('TbCustomers').WithIdColumn
      .WithColumn('Name').AsNvarchar(100).Nullable
      .WithColumn('Family').AsNvarchar(50).Nullable;

      SubmitChanges;
    end;
  end,
  procedure
  begin
    ORM.Delete.Table('TbCustomers');
    ORM.SubmitChanges;
  end
  ));

  //============================================
  Runner.Add(TMigration.Create('TbCustomers', 202301010004, 'Alexander', 'Task number #2900',
  procedure
  begin
    with ORM do
    begin
      Create.StoredProc('SelectTopTenCustomers')
      .AddParam('ReportData', ctDateTime)
      .AddParam('MarketCode', ctInt);

      SubmitChanges;
    end;
  end,
  procedure
  begin
    ORM.Delete.StoredProc('SelectTopTenCustomers');
    ORM.SubmitChanges;
  end
  ));
end;

procedure TForm1.btnDowngradeDatabaseClick(Sender: TObject);
begin
  Runner.DowngradeDatabase(StrToInt64Def(edtVersion.Text, 0));
end;

procedure TForm1.btnUpgradeDatabaseClick(Sender: TObject);
begin
  Runner.UpgradeDatabase;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  LvConnectionParams: TSqlConnectionParams;
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

  Runner := TSQLRunner.Create(LvConnectionParams);
  Runner.AddConfig.LogAllExecutions(True).UseInternalThread(False).SetProgressbar(pbTotal);// Optional

  {Use this line if you don't need local log}
  Runner.AddLogger.OnLog := OnLog;

  {Use this line if you need local log}
  //Runner.AddLogger.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog;
end;

procedure TForm1.OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
begin
  // This method will run anyway even if you ignore the Local Log file.
  //...
  //...
  // ShowMessage(AException);
  // Do anything you need here with the log data, log on Graylog, Terlegram, email, etc...

  mmoLog.Lines.BeginUpdate;
  mmoLog.Lines.Add('========== ' + DateTimeToStr(Now) + ' ==========');
  mmoLog.Lines.Add('Action Type: ' + GetEnumName(TypeInfo(TActionTypes), Ord(AActionType)));
  mmoLog.Lines.Add('Exception: ' + AException);
  mmoLog.Lines.Add('Class Name: ' + IfThen(AClassName.IsEmpty, 'N/A', AClassName));
  mmoLog.Lines.Add('Version: ' + IfThen(AVersion = 0, 'N/A', IntToStr(AVersion)));
  mmoLog.Lines.EndUpdate;
  mmoLog.Lines.Add('');
end;

end.
