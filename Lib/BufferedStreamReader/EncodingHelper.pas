unit EncodingHelper;

interface

uses
  System.SysUtils;

type
  TEncodingHelper = class helper for TEncoding
  public
    function GetCharCount(const Bytes: PByte; const ByteIndex, ByteCount: integer): integer; overload;
    function GetChars(const Bytes: PByte; const ByteIndex, ByteCount: integer): TCharArray; overload;
    function GetString(const Bytes: PByte; const ByteIndex, ByteCount: integer): string; overload;
  end;

implementation

uses
  System.SysConst;

{$POINTERMATH ON}

type
  EncodingAccess = class(TEncoding)
  end;

{ TEncodingHelper }

function TEncodingHelper.GetCharCount(const Bytes: PByte; const ByteIndex, ByteCount: integer): integer;
begin
  result := EncodingAccess(Self).GetCharCount(Bytes + ByteIndex, ByteCount);
end;

function TEncodingHelper.GetChars(const Bytes: PByte; const ByteIndex, ByteCount: integer): TCharArray;
var
  Len: integer;
begin
  Len := EncodingAccess(Self).GetCharCount(Bytes + ByteIndex, ByteCount);

  if (ByteCount > 0) and (Len = 0) then
    raise EEncodingError.CreateRes(@SNoMappingForUnicodeCharacter);

  SetLength(Result, Len);

  EncodingAccess(Self).GetChars(Bytes + ByteIndex, ByteCount, PChar(Result), Len);
end;

function TEncodingHelper.GetString(const Bytes: PByte; const ByteIndex, ByteCount: integer): string;
var
  Len: integer;
begin
  Len := EncodingAccess(Self).GetCharCount(Bytes + ByteIndex, ByteCount);

  if (ByteCount > 0) and (Len = 0) then
    raise EEncodingError.CreateRes(@SNoMappingForUnicodeCharacter);

  SetLength(Result, Len);

  EncodingAccess(Self).GetChars(Bytes + ByteIndex, ByteCount, PChar(Result), Len);
end;

end.
