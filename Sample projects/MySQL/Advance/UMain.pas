unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, System.TypInfo, System.StrUtils,
  UCustomers, UUsers, UInvoices,
  EasyDB.Core,
  EasyDB.ConnectionManager.MySQL,
  EasyDB.MigrationX, // Do not use "EasyDB.Migration.Base" here if you prefer Attributes.
  EasyDB.MySQLRunner,
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
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddMigrationsClick(Sender: TObject);
    procedure btnUpgradeDatabaseClick(Sender: TObject);
    procedure btnDowngradeDatabaseClick(Sender: TObject);
  private
    Runner: TMySQLRunner;
    procedure OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TForm4 }

procedure TfrmMain.btnAddMigrationsClick(Sender: TObject);
begin
  //Modern way
  Runner
  .Add(TUsersMgr_202301010001.Create)
  .Add(TUsersMgr_202301010002.Create)
  .Add(TUsersMgr_202301010003.Create)
  .Add(TCustomersMgr_202301010005.Create)
  .Add(TCustomersMgr_202301010010.Create)
  .Add(TInvoicesMgr_202301010005.Create)
  .Add(TInvoicesMgr_202301010010.Create);

  // Classic Way
{ Runner.MigrationList.Add(TUsersMgr_202301010001.Create);
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
  LvConnectionParams: TMySqlConnectionParams;
begin
  with LvConnectionParams do // Could be loaded from ini, registry or somewhere else.
  begin
    Server := '127.0.0.1';
    LoginTimeout := 30000;
    Port := 3306;
    UserName := 'ali';
    Pass := 'Admin123!@#';
    Schema := 'Library';
  end;

  {
   Logger must be configured befor creating the Runner.
   No need to free Logger, it will be destroyed when Runner destroys.
  }

  {Use this line if you don't need local log}
  TLogger.Instance.OnLog := OnLog;

  {Use this line if you need local log}
  //TLogger.Instance.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog;


  Runner := TmySQLRunner.Create(LvConnectionParams);
  Runner.AddConfig.LogAllExecutions(True).UseInternalThread(True).SetProgressbar(pbTotal);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Runner.Free;
end;

procedure TfrmMain.OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
begin
  // This method will run anyway if you assigne it and ignores LocalLog parameter.
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
