program EasyDB_PostgreSQL_Simple;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
