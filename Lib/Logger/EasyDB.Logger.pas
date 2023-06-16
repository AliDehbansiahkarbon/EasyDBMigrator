unit EasyDB.Logger;

interface

uses
  System.SysUtils, System.TypInfo, System.StrUtils;

type
  TActionTypes = (atUpgrade, atDownGrade, atInitialize, atPreparingMigrations);

  TLogger = class(TObject)
  private
    class var FMustLog: Boolean;
    class var FPath: string;
    class var FInstance: TLogger;
  public
    procedure Log(AActionType: TActionTypes; AException: string; AClassName: string = ''; AVersion: Int64 = 0);
    class function Instance: TLogger;
    class property Path: string read FPath write FPath;
    class property MustLog: Boolean read FMustLog write FMustLog;
  end;

implementation

{ TLogger }

class function TLogger.Instance: TLogger;
begin
  if not Assigned(FInstance) then
    FInstance := TLogger.Create;

  Result := FInstance;
end;

procedure TLogger.Log(AActionType: TActionTypes; AException, AClassName: string; AVersion: Int64);
var
  LvLogFile: TextFile;
begin
  if not MustLog then
    Exit;

  AssignFile(LvLogFile, FPath);

  if FileExists(FPath) then
    Append(LvLogFile)
  else
    Rewrite(LvLogFile);

  try
    Writeln(LvLogFile, '========== ' + DateTimeToStr(Now) + ' ==========');
    Writeln(LvLogFile, 'Action Type: ' + GetEnumName(TypeInfo(TActionTypes), Ord(AActionType)));
    Writeln(LvLogFile, 'Exception: ' + AException);
    Writeln(LvLogFile, 'Class Name: ' + IfThen(AClassName.IsEmpty, 'N/A', AClassName));
    Writeln(LvLogFile, 'Version: ' + IfThen(AVersion = 0, 'N/A', IntToStr(AVersion)));
    Writeln(LvLogFile, '');
  finally
    CloseFile(LvLogFile);
  end;
end;

end.
