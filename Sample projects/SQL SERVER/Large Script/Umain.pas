unit Umain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, System.StrUtils,

  EasyDB.Core,
  EasyDB.Consts,
  EasyDB.Logger,
  EasyDB.MSSQLRunner;

type
  TRichEditHelper = class helper for TRichEdit
  public
    procedure AddColored(AValue: string; AColor: TColor = 0);
  end;

  TfrmMain = class(TForm)
    btnUpgradeDatabase: TButton;
    pbTotal: TProgressBar;
    RichEdit1: TRichEdit;
    btnClear: TButton;
    rb_LogAllExecutions: TRadioButton;
    RadioButton1: TRadioButton;
    procedure btnUpgradeDatabaseClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    Runner: TSQLRunner;
    function GetLogStatus: Boolean;
    procedure OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
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
  pbTotal.Style := pbstMarquee;

  with LvConnectionParams do // The information can be sourced from an ini file, registry or other location.
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
  Runner.Config.UseInternalThread(True).LogAllExecutions(GetLogStatus);
  Runner.SQL.ExecuteScriptFile('..\..\Script\AdventureWorks2019_Minimal.sql', 'GO');
end;

procedure TfrmMain.Done(var Msg: TMessage);
begin
  pbTotal.Style := pbstNormal;
  pbTotal.Position := 0;
  Runner.Free;
end;

function TfrmMain.GetLogStatus: Boolean;
begin
  Result := rb_LogAllExecutions.Checked;
end;

procedure TfrmMain.OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
begin
  // This method will run anyway even if you ignore the Local Log file.
  //...
  //...
  // ShowMessage(AException);
  // Do anything you need here with the log data, log on Graylog, Telegram, email, etc...

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
