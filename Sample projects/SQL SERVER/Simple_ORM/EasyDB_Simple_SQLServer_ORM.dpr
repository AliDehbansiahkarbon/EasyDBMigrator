program EasyDB_Simple_SQLServer_ORM;

uses
  Vcl.Forms,
  UMain in 'UMain.pas' {Form1},
  EasyDB.ORM.Builder in '..\..\..\Lib\ORM\EasyDB.ORM.Builder.pas',
  EasyDB.ORM.Core in '..\..\..\Lib\ORM\EasyDB.ORM.Core.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  ReportMemoryLeaksOnShutdown := True;
  Application.Run;
end.
