unit EasyDB.Migration.Base;

interface
uses
  System.SysUtils, System.Generics.Collections;

type
  TMigrationBase = class
  public
    HiddenAttribDic: TDictionary<string, Variant>;
    HasAttribDic: Boolean;
    procedure CreateHiddenAttribDic(AEntityName: string; AVersion: int64; AAuthor: string; ADescription: string);
    destructor Destroy; override;
  end;


  TMigration = class(TMigrationBase) // You don't need class level attribute and you can use anonymouse method to define Upgrade and Downgrade procedures.
  private
    FUp: TProc;
    FDown: TProc;
    FVersion: Int64;
    FAuthor: string;
    FDescription: string;
    FEntityName: string;
  public
    constructor Create(const AEntityName: string; const AVersion: Int64; AAuthor: string; const ADescription: string; const AUp, ADown: TProc);
    procedure Upgrade; virtual;
    procedure Downgrade; virtual;

    property Author: string read FAuthor write FAuthor;
    property Description: string read FDescription write FDescription;
    property EntityName: string read FEntityName;
  published
    property Version: Int64 read FVersion; // published to be accessible in GetPropValue function.
  end;

implementation

{ TMigration }

constructor TMigration.Create(const AEntityName: string; const AVersion: Int64; AAuthor: string; const ADescription: string; const AUp, ADown: TProc);
begin
  FVersion := AVersion;
  FAuthor := AAuthor;
  FDescription := ADescription;
  FEntityName := AEntityName;
  FUp := AUp;
  FDown := ADown;
end;

procedure TMigration.Downgrade;
begin
  if Assigned(FDown) then
    FDown;
end;

procedure TMigration.Upgrade;
begin
  if Assigned(FUp) then
    FUp;
end;

{ TMigrationBase }

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

end.
