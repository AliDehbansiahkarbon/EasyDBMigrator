program EasyDB_Firebird_Advance;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {frmMain},
  UHelper in 'UHelper.pas',
  UCustomers in 'Migrations\UCustomers.pas',
  UInvoices in 'Migrations\UInvoices.pas',
  UUsers in 'Migrations\UUsers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
