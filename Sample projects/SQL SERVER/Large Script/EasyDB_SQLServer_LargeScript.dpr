program EasyDB_SQLServer_LargeScript;

uses
  Vcl.Forms,
  Umain in 'Umain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
