program EasyDBMigration_Simple_SQLServer;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
