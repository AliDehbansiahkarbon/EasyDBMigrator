program EasyDBMigrationSample;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {Form1},
  UCore in '..\Lib\Core\UCore.pas',
  EasyDB.ConnectionManager.Base in '..\Lib\ConnectionManagers\EasyDB.ConnectionManager.Base.pas',
  EasyDB.ConnectionManager.SQL in '..\Lib\ConnectionManagers\EasyDB.ConnectionManager.SQL.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
