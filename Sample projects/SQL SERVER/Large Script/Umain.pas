unit Umain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,System.StrUtils,

  EasyDB.Core,
  EasyDB.Logger,
  EasyDB.MSSQLRunner;

const
  WM_DONE = WM_USER + 1234;

type
  TRichEditHelper = class helper for TRichEdit
  public
    procedure AddColored(AValue: string; AColor: TColor = 0);
  end;

  TfrmMain = class(TForm)
    btnUpgradeDatabase: TButton;
    pbTotal: TProgressBar;
    RichEdit1: TRichEdit;
    chkLogExecutions: TCheckBox;
    btnClear: TButton;
    procedure btnUpgradeDatabaseClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    Runner: TSQLRunner;
    procedure OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
    { Private declarations }
  public
    procedure Done(var Msg: TMessage); message WM_DONE;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  RichEdit1.Lines.Clear;
  RichEdit1.Refresh;
end;

procedure TfrmMain.btnUpgradeDatabaseClick(Sender: TObject);
var
  LvConnectionParams: TSqlConnectionParams;
begin
  with LvConnectionParams do // Could be loaded from ini, registry or somewhere else.
  begin
    Server := '192.168.212.1';
    LoginTimeout := 30000;
    UserName := 'sa';
    Pass := '1';
    DatabaseName := 'AdventureWorks2019';
    Schema := 'dbo';
  end;

  TLogger.Instance.OnLog := OnLog;

  Runner := TSQLRunner.Create(LvConnectionParams);
  Runner.AddConfig.UseInternalThread(True).LogAllExecutions(chkLogExecutions.Checked);
  pbTotal.Style := pbstMarquee;
  Runner.SQLConnection.ExecuteScriptFile('..\..\Script\AdventureWorks2019_Minimal.sql');
end;

procedure TfrmMain.Done(var Msg: TMessage);
begin
  pbTotal.Style := pbstNormal;
  pbTotal.Position := 0;
  Runner.Free;
end;

procedure TfrmMain.OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
begin
  // This method will run anyway if you assigne it and ignores LocalLog parameter.
  //...
  //...
  // ShowMessage(AException);
  // Do anything you need here with the log data, log on Graylog, Terlegram, email, etc...

  RichEdit1.Lines.BeginUpdate;
  RichEdit1.Lines.Add('========== ' + DateTimeToStr(Now) + ' ==========');
  if LeftStr(AException, 5) = 'Error' then
  begin
    RichEdit1.AddColored('Exception:');
    RichEdit1.AddColored(AException);
  end
  else
    RichEdit1.Lines.Add('Message: ' + AException);

  RichEdit1.Lines.EndUpdate;
  RichEdit1.Lines.Add('');

  if AException = 'Done!' then
    PostMessage(Self.Handle, WM_DONE, 0, 0);
end;

{ TRichEditHelper }

procedure TRichEditHelper.AddColored(AValue: string; AColor: TColor);
begin
  if AColor = 0 then
    SelAttributes.Color := clRed
  else
    SelAttributes.Color := AColor;

  Lines.Add(Avalue);
end;

end.
