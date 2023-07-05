program EasyDBMigration_Simple_SQLServer;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {frmMain},
  EasyDB.ORM in '..\..\..\Lib\ORM\EasyDB.ORM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
