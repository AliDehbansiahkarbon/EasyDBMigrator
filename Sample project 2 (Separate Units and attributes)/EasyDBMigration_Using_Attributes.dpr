program EasyDBMigration_Using_Attributes;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {Form2},
  Customers in 'Migrations\Customers.pas',
  Invoices in 'Migrations\Invoices.pas',
  Users in 'Migrations\Users.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
