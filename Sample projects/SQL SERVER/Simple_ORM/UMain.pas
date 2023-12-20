{***************************************************}
{                                                   }
{   Auhtor: Ali Dehbansiahkarbon(adehban@gmail.com) }
{   GitHub: https://github.com/AliDehbansiahkarbon  }
{                                                   }
{***************************************************}
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
    btnCreateDB: TButton;
    procedure btnAddMigrationsClick(Sender: TObject);
    procedure btnUpgradeDatabaseClick(Sender: TObject);
    procedure btnDowngradeDatabaseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCreateDBClick(Sender: TObject);
  private
    Runner: TSQLRunner;
    procedure InitializeRunner;
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
  InitializeRunner;
  TLogger.Instance.Log(atUpgrade, 'Migrations have been added');

  Runner.Clear;
  ORM := TORM.GetInstance(ttSQLServer);
  Runner.ORM := ORM;

  Runner.Add(TMigration.Create('TbUsers', 202301010001, 'Ali', 'Created table Users(#2701)',
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
  Runner.Add(TMigration.Create('TbUsers', 202301010002, 'Ali', 'Added NewField2 to table Tbusers(#2702)',
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
  Runner.Add(TMigration.Create('TbUsers', 202301010003, 'Ali', 'Added NewField3 to table Tbusers(#2703)',
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
  Runner.Add(TMigration.Create('TbCustomers', 202301010003, 'Alex', 'Created Table TbCustomers and table TbInvoices(#2703)',
  procedure
  begin
    with ORM do
    begin
      Create.Table('TbCustomers')
      .WithColumn('Name').AsNvarchar(100).Nullable
      .WithColumn('Family').AsNvarchar(50).Nullable;

      Create.Table('TbInvoices').WithIdColumn
      .WithColumn('InvoiceNumber').AsBigInt.Nullable
      .WithColumn('InvoiceDate').AsDateTime.Nullable
      .WithColumn('MarketCode').AsInt.Nullable
      .WithColumn('TotalAmount').AsMoney.Nullable;

      SubmitChanges;
    end;
  end,
  procedure
  begin
    ORM.Delete.Table('TbCustomers');
    ORM.Delete.Table('TbInvoices');
    ORM.SubmitChanges;
  end
  ));

  //============================================
  Runner.Add(TMigration.Create('SelectTopTenCustomers', 202301010004, 'Alexander', 'Added SP and function(Task number #2704)',
  procedure
  var LvBody: string;
  begin
    with ORM do
    begin
      LvBody := 'Select * from TbInvoices where TotalAmount > @TotalAmount and MarketCode = @MarketCode and InvoiceDate = @ReportData';

      Create.StoredProc('SelectTopTenCustomers')
      .AddParam('TotalAmount', TDataType.Create(ctMoney))
      .AddParam('ReportData', TDataType.Create(ctDateTime))
      .AddParam('MarketCode', TDataType.Create(ctInt))
      .AddBody(LvBody);


      LvBody := 'Declare @Result Money '+ #10 +
                'Select @Result = Sum(TotalAmount) From TbInvoices where InvoiceDate <= @ReportData' + #10 +
                'Return @Result';

      Create.StoredFunction('GetTotalSum')
      .AddParam('ReportData', TDataType.Create(ctDateTime))
      .ReturnType(TDataType.Create(ctMoney))
      .AddBody(LvBody);

      SubmitChanges;
    end;
  end,
  procedure
  begin
    ORM.Delete.StoredProc('SelectTopTenCustomers');
    ORM.Delete.StoredFunc('GetTotalSum');
    ORM.SubmitChanges;
  end
  ));
end;

procedure TForm1.btnCreateDBClick(Sender: TObject);
var
  LvConnectionParams: TSqlConnectionParams;
  LvRunner: TSQLRunner;
  ORM: TORM;
begin
  with LvConnectionParams do // The information can be sourced from an ini file, registry or other location.
  begin
    Server := '192.168.212.1';
    LoginTimeout := 30000;
    UserName := 'sa';
    Pass := '1';
    DatabaseName := 'Master';
    Schema := 'dbo';
  end;

  LvRunner := TSQLRunner.Create(LvConnectionParams, OnLog);
  LvRunner.Config
    .LogAllExecutions(True)// Optional
    .UseInternalThread(False)// Do not to run with UseInternalThread = True (Only for Database creation)
    .SetProgressbar(pbTotal)// Optional
    .DelayedExecution(500);// Optional

  LvRunner.GetLogger.OnLog := OnLog;

  try
    LvRunner.Clear;
    ORM := TORM.GetInstance(ttSQLServer);
    LvRunner.ORM := ORM;

    LvRunner.Add(TMigration.Create('Library DB', 202301010000, 'GodAdmin!', 'Created the Database',
    procedure
    begin
      with ORM do
      begin
        Create.Database('Library')
        .MdfFileName('C:\Program Files\Microsoft SQL Server\MSSQL15.MSSQLSERVER\MSSQL\DATA\Library.mdf')
        .MdfSize('8192KB')
        .MdfFileGrowth('65536KB')
        .MdfMaxSize('UNLIMITED')
        .LdfFileName('C:\Program Files\Microsoft SQL Server\MSSQL15.MSSQLSERVER\MSSQL\DATA\Library.ldf')
        .LdfSize('8192KB')
        .LdfFileGrowth('65536KB')
        .LdfMaxSize('2048GB')
        .Collation('Latin1_General_CI_AS');

        SubmitChanges;
      end;
    end,
    procedure
    begin
      ORM.Delete.Database('Library');
      ORM.SubmitChanges;
    end
    ));

    LvRunner.UpgradeDatabase;
  finally
    LvRunner.Free;
  end;
end;

procedure TForm1.btnDowngradeDatabaseClick(Sender: TObject);
begin
  if Assigned(Runner) then
    Runner.DowngradeDatabase(StrToInt64Def(edtVersion.Text, 0));
end;

procedure TForm1.btnUpgradeDatabaseClick(Sender: TObject);
begin
  if Assigned(Runner) then
    Runner.UpgradeDatabase;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(Runner) then
    Runner.Free;
end;

procedure TForm1.InitializeRunner;
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

  if Assigned(Runner) then
    Runner.Free;

  Runner := TSQLRunner.Create(LvConnectionParams, OnLog);
  Runner.Config
    .LogAllExecutions(True)// Optional
    .UseInternalThread(True)// Optional
    .SetProgressbar(pbTotal)// Optional
    .DelayedExecution(500);// Optional

  {Use this line if you don't need local log}
  Runner.GetLogger.OnLog := OnLog;

  {Use this line if you need local log}
  //Runner.GetLogger.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog;
end;

procedure TForm1.OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
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
