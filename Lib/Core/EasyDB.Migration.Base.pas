unit EasyDB.Migration.Base;

interface
uses
  System.SysUtils;

type
  TMigration = class // You don't need class level attribute and you can use anonymouse method to define Upgrade and Downgrade procedures.
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

    property Version: Int64 read FVersion;
    property Author: string read FAuthor write FAuthor;
    property Description: string read FDescription write FDescription;
    property EntityName: string read FEntityName;
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

end.
