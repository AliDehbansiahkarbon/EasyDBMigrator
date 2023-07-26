unit UMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, System.StrUtils,

  EasyDB.Core,
  EasyDB.Consts,
  EasyDB.Logger,
  EasyDB.MariaDBRunner;

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
  private
    Runner: TMariaDBRunner;
    function GetLogStatus: Boolean;
    procedure OnLog(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
  public
    procedure Done(var Msg: TMessage); message WM_DONE;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnUpgradeDatabaseClick(Sender: TObject);
var
  LvConnectionParams: TMariaDBConnectionParams;
begin
  with LvConnectionParams do // The information can be sourced from an ini file, registry or other location.
  begin
    Server := '127.0.0.1';
    LoginTimeout := 30000;
    Port := 3306;
    UserName := 'ali';
    Pass := 'Admin123!@#';
    Schema := 'Library';
  end;

  TLogger.Instance.OnLog := OnLog;
  Runner := TMariaDBRunner.Create(LvConnectionParams);
  Runner.Config.UseInternalThread(True).LogAllExecutions(GetLogStatus);
  Runner.MariaDB.ExecuteScriptFile('..\..\Script\DBUpdateScript.sql', ';');
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
