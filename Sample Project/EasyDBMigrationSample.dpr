program EasyDBMigrationSample;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {Form1},
  EasyDB.Core in '..\Lib\Core\EasyDB.Core.pas',
  EasyDB.ConnectionManager.Base in '..\Lib\ConnectionManagers\EasyDB.ConnectionManager.Base.pas',
  EasyDB.ConnectionManager.SQL in '..\Lib\ConnectionManagers\EasyDB.ConnectionManager.SQL.pas',
  EasyDB.Runner in '..\Lib\Runners\EasyDB.Runner.pas',
  EasyDB.Migration.Base in '..\Lib\Core\EasyDB.Migration.Base.pas',
  EasyDB.Attribute in '..\Lib\Core\EasyDB.Attribute.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
