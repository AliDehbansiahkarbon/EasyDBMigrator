program EasyDBMigration_Simple_SQLServer;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
