unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.TypInfo,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.StrUtils, Vcl.ComCtrls,
  UCustomers, UUsers, UInvoices,
  EasyDB.Core,
  EasyDB.ConnectionManager.SQL,
  EasyDB.MigrationX, // Do not use "EasyDB.Migration.Base" here if you prefer to use Attributes.
  EasyDB.MSSQLRunner,
  EasyDB.Logger;

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
    procedure btnAddMigrationsClick(Sender: TObject);
    procedure btnUpgradeDatabaseClick(Sender: TObject);
    procedure btnDowngradeDatabaseClick(Sender: TObject);
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
  //Modern way
  Runner.Clear
  .Add(TUsersMgr_202301010001.Create)
  .Add(TUsersMgr_202301010002.Create)
  .Add(TUsersMgr_202301010003.Create)
  .Add(TCustomersMgr_202301010005.Create)
  .Add(TCustomersMgr_202301010010.Create)
  .Add(TInvoicesMgr_202301010005.Create)
  .Add(TInvoicesMgr_202301010010.Create);

  // Classic Way
{
  Runner.Clear;
  Runner.MigrationList.Add(TUsersMgr_202301010001.Create);
  Runner.MigrationList.Add(TUsersMgr_202301010002.Create);
  Runner.MigrationList.Add(TUsersMgr_202301010003.Create);

  Runner.MigrationList.Add(TCustomersMgr_202301010005.Create);
  Runner.MigrationList.Add(TCustomersMgr_202301010010.Create);

  Runner.MigrationList.Add(TInvoicesMgr_202301010005.Create);
  Runner.MigrationList.Add(TInvoicesMgr_202301010010.Create);
}
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
    .LogAllExecutions(True)  // Optional
    .UseInternalThread(True) // Optional
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
