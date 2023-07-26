unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, System.TypInfo, System.StrUtils,

  EasyDB.Core,
  EasyDB.Migration,
  EasyDB.PostgreSQLRunner,
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
    Runner: TPgRunner;
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
  Runner.Clear;
  Runner.Add(TMigration.Create('TbUsers', 202301010001, 'Ali', 'Create table Users, #2701',
  procedure
  var LvScript: string;
  begin
    LvScript := 'CREATE TABLE IF NOT EXISTS public.tbusers' + #10 +
    '('+ #10 +
    'id integer NOT NULL DEFAULT nextval(''tbusers_id_seq''::regclass),' + #10 +
    'username character varying(100) COLLATE pg_catalog."default",' + #10 +
    'pass character varying(50) COLLATE pg_catalog."default",' + #10 +
    'CONSTRAINT tbusers_pkey PRIMARY KEY (id)' + #10 +
    ');';

    Runner.PG.ExecuteAdHocQuery(LvScript);
  end,
  procedure
  begin
    Runner.PG.ExecuteAdHocQuery('DROP TABLE IF EXISTS public.tbusers;');
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbUsers', 202301010002, 'Ali', 'Task number #2701',
  procedure
  begin
    Runner.PG.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD NewField2 VARCHAR(50)');
  end,
  procedure
  begin
    Runner.PG.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP COLUMN NewField2');
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbUsers', 202301010003, 'Ali', 'Task number #2702',
  procedure
  begin
    Runner.PG.ExecuteAdHocQuery('ALTER TABLE TbUsers ADD NewField3 INT');
  end,
  procedure
  begin
    Runner.PG.ExecuteAdHocQuery('ALTER TABLE TbUsers DROP COLUMN NewField3');
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbCustomers', 202301010003, 'Alex', 'Task number #2702',
  procedure
  var LvScript: string;
  begin
    LvScript := 'CREATE TABLE IF NOT EXISTS public.tbcustomers' + #10 +
    '(' + #10 +
    'id integer NOT NULL DEFAULT nextval(''tbcustomers_id_seq''::regclass),' + #10 +
    'name character varying(100) COLLATE pg_catalog."default",' + #10 +
    'family character varying(50) COLLATE pg_catalog."default",' + #10 +
    'phone character varying(10) COLLATE pg_catalog."default",' + #10 +
    'CONSTRAINT tbcustomers_pkey PRIMARY KEY (id)' + #10 +
    ')';
    Runner.PG.ExecuteAdHocQuery(LvScript);
  end,
  procedure
  begin
    Runner.PG.ExecuteAdHocQuery('DROP TABLE IF EXISTS public.tbcustomers');
  end
  ));
  //============================================
  Runner.Add(TMigration.Create('TbCustomers', 202301010004, 'Tom', 'Task number #2703',
  procedure
  var LvScript: string;
  begin
    LvScript := 'CREATE TABLE IF NOT EXISTS public.tbinvoices' + #10 +
    '(' + #10 +
    'id integer NOT NULL DEFAULT nextval(''tbinvoices_id_seq''::regclass),' + #10 +
    'invoiceid integer,' + #10 +
    'customerid integer,' + #10 +
    'invoicedate timestamp without time zone,' + #10 +
    'totalamount numeric(10,2),' + #10 +
    'CONSTRAINT tbinvoices_pkey PRIMARY KEY (id)' + #10 +
    ')';
    Runner.PG.ExecuteAdHocQuery(LvScript);
  end,
  procedure
  begin
    Runner.PG.ExecuteAdHocQuery('DROP TABLE IF EXISTS public.tbinvoices');
  end
  ));
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
  LvConnectionParams: TPgConnectionParams;
begin
  with LvConnectionParams do // The information can be sourced from an ini file, registry or other location.
  begin
    Server := '192.168.212.1';
    LoginTimeout := 30000;
    Port := 5432;
    UserName := 'postgres';
    Pass := 'Admin123!@#';
    DatabaseName := 'Library';
    Schema := 'public';
  end;

  Runner := TPgRunner.Create(LvConnectionParams);
  Runner.Config
    .LogAllExecutions(True) // Optional
    .UseInternalThread(True) //Optional
    .SetProgressbar(pbTotal); //Optional

  {Use this line if you don't need local log}
  Runner.AddLogger.OnLog := OnLog;

  {Use this line if you need local log}
  //Runner.AddLogger.ConfigLocal(True, 'C:\Temp\EasyDBLog.txt').OnLog := OnLog;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  Runner.Free;
end;

procedure TForm2.OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
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
