//   Copyright 2015 Asbjørn Heid
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//       http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.
unit BufStream;

interface

uses
  System.SysUtils, System.Classes;

type
  BufferedStreamOption = (BufferedStreamOwnsSource);
  BufferedStreamOptions = set of BufferedStreamOption;

  /// <summary>
  ///  Provides a read-only buffered stream, where the buffer can be accessed.
  ///  <para>
  ///  It has methods for filling the buffer with more data, consuming bytes
  ///  from the buffer causing the position to be increased accordingly.
  ///  This allows it to be used to extract parts of a forwards-only stream,
  ///  passing the remaining data to other code for processing.
  ///  </para>
  /// </summary>
  BufferedStream = class(TStream)
  strict private
    FSourceStream: TStream;
    FOwnsSourceStream: boolean;
    FBufferChunkSize: integer;
    FBufferFrontIndex: integer;
    FBufferedData: TBytes;
    FBufferedDataLength: integer;
    FPosition: Int64;

    procedure ClearBuffer;
    function GetBufferedData: PByte; inline;
  protected
    {$REGION 'TStream overrides'}
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
    {$ENDREGION}
  public
    /// <summary>
    ///  Constructs a buffered stream instance.
    /// </summary>
    /// <param name="SourceStream">
    ///  Stream to provide buffer for.
    /// </param>
    /// <param name="OwnsSourceStream">
    ///  True if the buffered stream should take ownership of the source stream.
    /// </param>
    /// <param name="BufferChunkSize">
    ///  Number of bytes read per call to FillBuffer.
    /// </param>
    constructor Create(const SourceStream: TStream;
      const Options: BufferedStreamOptions = [];
      const BufferChunkSize: integer = 4096);
    destructor Destroy; override;

    {$REGION 'TStream overrides'}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ENDREGION}

    /// <summary>
    ///  Attempts to fill the buffer with more data.
    /// </summary>
    /// <returns>
    ///  False if less than a full chunk of data could be read from the source
    ///  stream. This may mean that no additional data was read into the buffer.
    ///  True otherwise.
    /// </returns>
    function FillBuffer: boolean;

    /// <summary>
    ///  Removes a given number of bytes from the buffer and advances the
    ///  stream position.
    /// </summary>
    /// <param name="Size">Number of bytes to consume. Can be larger than the
    ///  current buffer size, in which case the entire buffer is discarded and
    //   the stream position is updated to the end of the buffered data.
    /// </param>
    procedure ConsumeBuffer(const Size: integer);

    /// <summary>
    ///  Discards the buffer and syncs position with the source stream.
    ///  Use this if the source stream has been manipulated directly.
    /// </summary>
    procedure DiscardBuffer;

    /// <summary>
    ///  Source stream which is being buffered. If the source stream is
    ///  manipulated directly inbetween calls to the buffered stream, call
    ///  DiscardBuffer to synchronize the buffered stream to the source stream.
    /// </summary>
    property SourceStream: TStream read FSourceStream;
    property OwnsSourceStream: boolean read FOwnsSourceStream;

    /// <summary>
    ///  Pointer to the currently buffered data.
    ///  Use BufferedDataLength to determine the amount of buffered data.
    ///  Note that modifying data in the buffer
    ///  is not propagated to the underlying source stream.
    ///  Use FillBuffer to append more data to the buffer, and ConsumeBuffer
    ///  to remove data from the start of the buffer and update the
    ///  stream position.
    /// </summary>
    property BufferedData: PByte read GetBufferedData;
    /// <summary>
    ///  The size of the buffered data, in bytes.
    /// </summary>
    property BufferedDataLength: integer read FBufferedDataLength;
  end;

implementation

uses
  System.Math;

{$POINTERMATH ON}

{ BufferedStream }

procedure BufferedStream.ClearBuffer;
begin
  FBufferFrontIndex := 0;
  FBufferedDataLength := 0;
  FBufferedData := nil;
end;

procedure BufferedStream.ConsumeBuffer(const Size: integer);
begin
  if (Size >= FBufferedDataLength) then
  begin
    FPosition := FPosition + FBufferedDataLength;
    ClearBuffer;
  end
  else if (FBufferFrontIndex + Size >= FBufferChunkSize) then
  begin
    // we've got a full chunk of unused space in front of the buffer
    // move this
    FPosition := FPosition + Size;
    Move(FBufferedData[FBufferFrontIndex + Size], FBufferedData[0], FBufferedDataLength - Size);
    FBufferFrontIndex := 0;
    FBufferedDataLength := FBufferedDataLength - Size;
    SetLength(FBufferedData, FBufferedDataLength);
  end
  else
  begin
    // just advance front index
    FPosition := FPosition + Size;
    FBufferFrontIndex := FBufferFrontIndex + Size;
    FBufferedDataLength := FBufferedDataLength - Size;
  end;
end;

constructor BufferedStream.Create(const SourceStream: TStream;
  const Options: BufferedStreamOptions;
  const BufferChunkSize: integer);
begin
  inherited Create;

  FSourceStream := SourceStream;  
  FOwnsSourceStream := BufferedStreamOwnsSource in Options;

  FBufferChunkSize := 1;
  if (BufferChunkSize > 1) then  
    FBufferChunkSize := BufferChunkSize;
end;

destructor BufferedStream.Destroy;
begin
  if OwnsSourceStream then
  begin
    FSourceStream.Free;
  end;

  FSourceStream := nil;  

  inherited;
end;

procedure BufferedStream.DiscardBuffer;
begin
  ClearBuffer;
  FPosition := SourceStream.Position;
end;

function BufferedStream.FillBuffer: boolean;
var
  i, len: integer;
begin
  len := Length(FBufferedData);
  SetLength(FBufferedData, len + FBufferChunkSize);

  i := len;

  len := SourceStream.Read(FBufferedData[i], FBufferChunkSize);

  result := True;

  FBufferedDataLength := FBufferedDataLength + len;
  if (len < FBufferChunkSize) then
  begin
    // reduce size of buffer to reflect read data amount
    SetLength(FBufferedData, i + len);
    result := False;
  end;
end;

function BufferedStream.GetBufferedData: PByte;
begin
  result := @FBufferedData[FBufferFrontIndex];
end;

function BufferedStream.GetSize: Int64;
begin
  result := SourceStream.Size;
end;

function BufferedStream.Read(var Buffer; Count: Integer): Longint;
var
  len: integer;
  endOfSource: boolean;
  dest: PByte;
begin
  endOfSource := False;  

  // try using buffer with possibly an additional chunk first
  if (Count > FBufferedDataLength) and (Count <= (FBufferedDataLength + FBufferChunkSize)) then
    endOfSource := not FillBuffer();

  len := Min(FBufferedDataLength, Count);
    
  Move(FBufferedData[FBufferFrontIndex], Buffer, len);
  ConsumeBuffer(len);

  if (len = Count) or (endOfSource) then
  begin
    result := len;
    exit;
  end;

  // buffer didn't fill entire request and the source may have more, 
  // try to get more from underlying stream
  dest := PByte(@Buffer) + len;

  result := SourceStream.Read(dest^, Count-len);

  // since we're here we've consumed the entire buffer so just need to adjust
  // position for the source read
  FPosition := FPosition + result;

  result := result + len;
end;

procedure BufferedStream.SetSize(NewSize: Integer);
begin
  SetSize(Int64(NewSize));
end;

function BufferedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  sourceOffset: Int64;
begin
  if (Origin = soCurrent) and (Offset = 0) then
  begin
    result := FPosition;
    exit;
  end;

  // TODO - more intelligent handling of buffer
  ClearBuffer;

  if (Origin = soCurrent) then
  begin
    sourceOffset := FPosition + Offset - SourceStream.Position;
  end
  else
  begin
    sourceOffset := Offset;
  end;

  FPosition := SourceStream.Seek(sourceOffset, Origin);
  result := FPosition;
end;

procedure BufferedStream.SetSize(const NewSize: Int64);
var
  oldSize: Int64;
begin
  oldSize := Size;
  SourceStream.Size := NewSize;
  if (oldSize > newSize) then
    DiscardBuffer;
end;

function BufferedStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise ENotSupportedException.Create('TBufferedStream does not support writing');
end;

end.
