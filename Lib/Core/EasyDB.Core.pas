unit EasyDB.Core;

interface
uses
  Vcl.ComCtrls, System.Generics.Collections;

type
  TMigrationBase = class;
  TArrangeMode = (umASC, umDESC);
  TMigrations = TObjectList<TMigrationBase>;
  TMigrationsDic = TObjectDictionary<string, TMigrations>;

  TConnectionParams = record
    Server: string;
    LoginTimeout: Integer;
    UserName: string;
    Pass: string;
    DatabaseName: string;
    Schema: string;
  end;

  TObjListHelper = class helper for TMigrations
  public
    function FindMigration(AMigrationObj: TMigrationBase): Boolean;
  end;

  IRunner = interface
    ['{DECF074C-109F-488F-A97D-4B3C68FB4F35}']

    procedure UpdateVersionInfo(AMigration: TMigrationBase; AInsertMode: Boolean = True);
    procedure DownGradeVersionInfo(AVersionToDownGrade: Int64);
    function GetDatabaseVersion: Int64;
  end;

  TMigrationBase = class
  public
    HiddenAttribDic: TDictionary<string, Variant>;
    HasAttribDic: Boolean;
    procedure CreateHiddenAttribDic(AEntityName: string; AVersion: int64; AAuthor: string; ADescription: string);
    destructor Destroy; override;
  end;

  //Runner.LogAllExecutions(True).UseInternalThread(True).SetProgressbar(pbTotal).RollBackAllByAnyError(True); //each part This line is Optional

  TConfig = class
  private
    FLogAllExecutions: Boolean;
    FUseInternalThread: Boolean;
    FProgressBar: TProgressBar;
    FRollBackAllByAnyError: Boolean;
  public
    constructor Create;
    function LogAllExecutions(AValue: Boolean): TConfig;
    function UseInternalThread(AValue: Boolean): TConfig;
    function SetProgressbar(AProgressbar: TProgressBar): TConfig;
    /// <summary> This option is just for SQL SERVER transactions.
    /// </summary>
    function RollBackAllByAnyError(AValue: Boolean): TConfig;

    property ProgressBar: TProgressBar read FProgressBar;
    property UseThreadStat: Boolean read FUseInternalThread;
    property LogAllExecutionsStat: Boolean read FLogAllExecutions;
    property RollBackAllByAnyErrorStat: Boolean read FRollBackAllByAnyError;
  end;

implementation
{ TMigrationBase }

uses
  EasyDB.Migration, EasyDB.MigrationX;

procedure TMigrationBase.CreateHiddenAttribDic(AEntityName: string; AVersion: int64; AAuthor: string; ADescription: string);
begin
  HiddenAttribDic := TDictionary<string, Variant>.Create;
  HiddenAttribDic.Add('EntityName', AEntityName);
  HiddenAttribDic.Add('Version', AVersion);
  HiddenAttribDic.Add('Author', AAuthor);
  HiddenAttribDic.Add('Description', ADescription);
end;

destructor TMigrationBase.Destroy;
begin
  if Assigned(HiddenAttribDic) then
    HiddenAttribDic.Free;
  inherited;
end;

{ TObjListHelper }

function TObjListHelper.FindMigration(AMigrationObj: TMigrationBase): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Pred(Count) do
  begin
    if AMigrationObj is TMigration then
    begin
      if TMigration(AMigrationObj).Version = TMigration(Items[I]).Version then
      begin
        Result := True;
        Break;
      end;
    end
    else if AMigrationObj is TMigrationX then
    begin
      if TMigrationX(AMigrationObj).AttribVersion = TMigrationX(Items[I]).AttribVersion then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ TConfig }

constructor TConfig.Create;
begin
  FUseInternalThread := False;
  FLogAllExecutions := False;
  FProgressBar := nil;
end;

function TConfig.LogAllExecutions(AValue: Boolean): TConfig;
begin
  FLogAllExecutions := AValue;
  Result := Self;
end;

function TConfig.RollBackAllByAnyError(AValue: Boolean): TConfig;
begin
  FRollBackAllByAnyError := AValue;
  Result := Self;
end;

function TConfig.SetProgressbar(AProgressbar: TProgressBar): TConfig;
begin
  FProgressBar := AProgressbar;
  Result := Self;
end;

function TConfig.UseInternalThread(AValue: Boolean): TConfig;
begin
  FUseInternalThread := AValue;
  Result := Self;
end;

end.
