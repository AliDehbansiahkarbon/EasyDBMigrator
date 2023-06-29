unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.TypInfo,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.StrUtils, Vcl.ComCtrls, UCustomers, UUsers, UInvoices,
  EasyDB.Core,
  EasyDB.ConnectionManager.SQL,
  EasyDB.MigrationX, // Do not use "EasyDB.Migration.Base" here if you prefer Attributes.
  EasyDB.MSSQLRunner,
  EasyDB.Logger;

type
  TForm2 = class(TForm)
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
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.btnAddMigrationsClick(Sender: TObject);
begin
  Runner.MigrationList.Add(TUsersMgr_202301010001.Create);
  Runner.MigrationList.Add(TUsersMgr_202301010002.Create);
  Runner.MigrationList.Add(TUsersMgr_202301010003.Create);

  Runner.MigrationList.Add(TCustomersMgr_202301010005.Create);
  Runner.MigrationList.Add(TCustomersMgr_202301010010.Create);

  Runner.MigrationList.Add(TInvoicesMgr_202301010005.Create);
  Runner.MigrationList.Add(TInvoicesMgr_202301010010.Create);
end;

procedure TForm2.btnDowngradeDatabaseClick(Sender: TObject);
begin
  Runner.DowngradeDatabase(StrToInt64(edtVersion.Text));
end;

procedure TForm2.btnUpgradeDatabaseClick(Sender: TObject);
begin
  if Runner.MigrationList.Count = 0 then
  begin
    ShowMessage('You should add at least one migration object.');
    Exit;
  end;

  Runner.UpgradeDatabase;
end;

procedure TForm2.FormCreate(Sender: TObject);
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

  {Use this line if you need local log.
   Logger must be configured befor creating the Runner.
   Noneed to free Logger, it will be destroyed when Runner destroys}
  TLogger.Instance.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog;

  {Use this line if you don't need local log}
  // TLogger.Instance.OnLog := OnLog;

  Runner := TSQLRunner.Create(LvConnectionParams);
  Runner.AddConfig.LogAllExecutions(True).UseInternalThread(True).SetProgressbar(pbTotal);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  Runner.Free;
end;

procedure TForm2.OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
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
