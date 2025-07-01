unit mMProtocol;

interface

uses
  System.SysUtils, System.Classes,
  mormot.core.base,
  mormot.core.log,
  mormot.net.async;

const
  // Magic marker for our protocol messages
  PROTOCOL_MAGIC = $CAFEBABE;

type
  // Enhanced message structure with command ID
  TCommandMessage = packed record
    Magic: UInt32;        // Magic marker
    Command: Integer;     // Command ID
    DataSize: UInt32;     // Size of data that follows
    // Data follows after this header
  end;

  // Send callback function type
  TSendMessageCallback = procedure(const Message: RawByteString) of object;

  // Helper class for protocol operations
  TCommandProtocol = class
  public
    // Pack a message with magic marker and command (TBytes output)
    class function PackMessage(aCmd: Integer; const Data: TBytes): RawByteString;

    // Send a protocol message via callback (TBytes input)
    class procedure SendMessage(SendCallback: TSendMessageCallback; aCmd: Integer; const Data: TBytes);

    // Try to parse protocol message from buffer (TBytes output)
    class function TryParseMessage(const Buffer: RawByteString; out aCmd: Integer; out Data: TBytes): Boolean;

    // Check if buffer starts with our magic marker
    class function HasMagicMarker(const Buffer: RawByteString): Boolean;
  end;

implementation

{ TCommandProtocol }

class function TCommandProtocol.PackMessage(aCmd: Integer; const Data: TBytes): RawByteString;
var
  Header: TCommandMessage;
begin
  Header.Magic := PROTOCOL_MAGIC;
  Header.Command := aCmd;
  Header.DataSize := Length(Data);

  SetLength(Result, SizeOf(Header) + Length(Data));
  Move(Header, Result[1], SizeOf(Header));
  if Length(Data) > 0 then
    Move(Data[0], Result[SizeOf(Header) + 1], Length(Data));
end;

class procedure TCommandProtocol.SendMessage(SendCallback: TSendMessageCallback; aCmd: Integer; const Data: TBytes);
var
  Message: RawByteString;
begin
  Message := PackMessage(aCmd, Data);
  if Assigned(SendCallback) then
    SendCallback(Message);
end;

class function TCommandProtocol.TryParseMessage(const Buffer: RawByteString; out aCmd: Integer; out Data: TBytes): Boolean;
var
  Header: TCommandMessage;
begin
  Result := False;
  aCmd := 0;
  SetLength(Data, 0);

  // Check minimum size for header
  if Length(Buffer) < SizeOf(Header) then
    Exit;

  // Extract header
  Move(Buffer[1], Header, SizeOf(Header));

  // Check magic marker
  if Header.Magic <> PROTOCOL_MAGIC then
    Exit;

  // Check if we have all the data
  if Length(Buffer) < SizeOf(Header) + Header.DataSize then
    Exit;

  // Extract command and data
  aCmd := Header.Command;
  if Header.DataSize > 0 then
  begin
    SetLength(Data, Header.DataSize);
    Move(Buffer[SizeOf(Header) + 1], Data[0], Header.DataSize);
  end;

  Result := True;
end;

class function TCommandProtocol.HasMagicMarker(const Buffer: RawByteString): Boolean;
begin
  Result := (Length(Buffer) >= 4) and (PUInt32(@Buffer[1])^ = PROTOCOL_MAGIC);
end;

end.
