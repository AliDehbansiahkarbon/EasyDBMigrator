program EasyDB.Oracle.Simple;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {frmMain},
  EasyDB.OracleRunner in '..\..\..\Lib\Runners\EasyDB.OracleRunner.pas',
  EasyDB.ConnectionManager.Oracle in '..\..\..\Lib\ConnectionManagers\EasyDB.ConnectionManager.Oracle.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
