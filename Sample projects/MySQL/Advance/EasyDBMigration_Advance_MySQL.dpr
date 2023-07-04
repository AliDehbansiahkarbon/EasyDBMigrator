program EasyDBMigration_Advance_MySQL;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {frmMain},
  UCustomers in 'Migrations\UCustomers.pas',
  UInvoices in 'Migrations\UInvoices.pas',
  UUsers in 'Migrations\UUsers.pas',
  UHelper in 'UHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  ReportMemoryLeaksOnShutdown := True;
  Application.Run;
end.
