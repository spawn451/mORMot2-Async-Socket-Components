unit mMClient;

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  mormot.core.base, mormot.core.log, mormot.core.rtti,
  mormot.net.sock, mormot.net.async, mormot.core.buffers,
  mormot.lib.openssl11, mMProtocol;

type
  // Forward declarations
  TmMClient = class;

  // Logging level enumeration for easy component property
  // Maps directly to mORMot2's TSynLogFilter for consistency
  TmMClientLogLevel = (
    llNone,         // No logging (completely disabled)
    llErrors,       // Only errors, exceptions, and critical failures (LOG_FILTER[lfErrors])
    llDebug,        // Debug information including enter/leave (LOG_FILTER[lfDebug])
    llClientServer, // Client/server communication logs (LOG_FILTER[lfClientServer])
    llVerbose       // All logging (LOG_VERBOSE)
  );

  // TLS Provider enumeration
  TmMClientTlsProvider = (
    tpOpenSSL,      // Force OpenSSL (requires conditional defines + DLLs)
    tpSChannel      // Force Windows SChannel (Windows only, no defines)
  );

  // Event types
  TmMClientHandleCommandEvent = function(Sender: TObject; aCmd: Integer; const aData: TBytes): TBytes of object;
  TmMClientConnectEvent = procedure(Sender: TObject) of object;
  TmMClientDisconnectEvent = procedure(Sender: TObject) of object;

  // Internal connection class
  TmMClientConnection = class(TAsyncConnection)
  private
    FOwnerComponent: TmMClient;
    procedure ProcessCommand(aCmd: Integer; const Data: TBytes);
    procedure SendProtocolMessage(const Message: RawByteString);
  protected
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    procedure AfterCreate; override; // Maps to OnConnect event
    procedure OnClose; override; // Maps to OnDisconnect event
  public
    constructor Create(aOwner: TAsyncConnections; const aRemoteIP: TNetAddr); override;

  end;

  // Main client component
  TmMClient = class(TComponent)
  private
    FInitActive: Boolean;
    FClient: TAsyncClient;
    FConnection: TmMClientConnection;
    FLogFamily: TSynLogFamily;
    FLogLevel: TmMClientLogLevel;
    FActive: Boolean;
    FHost: string;
    FPort: Integer;
    FConnectionTimeout: Integer;
    FThreadPoolSize: Integer;

    // TLS Support
    FUseTLS: Boolean;
    FIgnoreCertificateErrors: Boolean;
    FCACertificatesFile: string;
    FTlsProvider: TmMClientTlsProvider;

    // Add EventsUseMainThread
    FEventsUseMainThread: Boolean;

    // Events
    FOnHandleCommand: TmMClientHandleCommandEvent;
    FOnConnect: TmMClientConnectEvent;
    FOnDisconnect: TmMClientDisconnectEvent;

    // Property methods with thread safety
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetHost: string;
    procedure SetHost(const Value: string);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const Value: Integer);
    function GetThreadPoolSize: Integer;
    procedure SetThreadPoolSize(const Value: Integer);
    function GetEventsUseMainThread: Boolean;
    procedure SetEventsUseMainThread(const Value: Boolean);
    function GetLogLevel: TmMClientLogLevel;
    procedure SetLogLevel(const Value: TmMClientLogLevel);

    // TLS property getters/setters
    function GetUseTLS: Boolean;
    procedure SetUseTLS(const Value: Boolean);
    function GetIgnoreCertificateErrors: Boolean;
    procedure SetIgnoreCertificateErrors(const Value: Boolean);
    function GetCACertificatesFile: string;
    procedure SetCACertificatesFile(const Value: string);
    function GetTlsProvider: TmMClientTlsProvider;
    procedure SetTlsProvider(const Value: TmMClientTlsProvider);

    // Internal activation method
    procedure DoActivate(aActivate: Boolean);

  protected
    // Thread safety
    PropertyLock: TCriticalSection;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Connection methods
    procedure Connect;
    procedure Disconnect;

    // SendData with command parameter
    procedure SendData(aCmd: Integer; const Data: TBytes);

    // Properties (read-only at runtime except Active)
    property Connection: TmMClientConnection read FConnection;

  published
    // Properties with proper getters/setters
    property Active: Boolean read GetActive write SetActive default False;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort default 3434;
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout default 10;
    property ThreadPoolSize: Integer read GetThreadPoolSize write SetThreadPoolSize default 1;

    // EventsUseMainThread property
    property EventsUseMainThread: Boolean read GetEventsUseMainThread write SetEventsUseMainThread default True;

    // Logging control
    property LogLevel: TmMClientLogLevel read GetLogLevel write SetLogLevel default llErrors;

    // TLS Properties
    property UseTLS: Boolean read GetUseTLS write SetUseTLS default False;
    property IgnoreCertificateErrors: Boolean read GetIgnoreCertificateErrors write SetIgnoreCertificateErrors default False;
    property CACertificatesFile: string read GetCACertificatesFile write SetCACertificatesFile;
    property TlsProvider: TmMClientTlsProvider read GetTlsProvider write SetTlsProvider default tpOpenSSL;

    // Events
    property OnHandleCommand: TmMClientHandleCommandEvent read FOnHandleCommand write FOnHandleCommand;
    property OnConnect: TmMClientConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TmMClientDisconnectEvent read FOnDisconnect write FOnDisconnect;
  end;

// Global component reference for connection callback
var
  GlobalClientComponent: TmMClient = nil;

implementation

{ TmMClientConnection }

constructor TmMClientConnection.Create(aOwner: TAsyncConnections; const aRemoteIP: TNetAddr);
begin
  inherited Create(aOwner, aRemoteIP);
  FOwnerComponent := GlobalClientComponent;
end;

procedure TmMClientConnection.AfterCreate;
var
  TlsContext: TNetTlsContext;
begin
  inherited AfterCreate;

  // Manual TLS setup for client connections (following THttpAsyncClientConnection pattern)
  if Assigned(FOwnerComponent) and FOwnerComponent.FUseTLS then
  begin
    try
      // Initialize TLS context for client connection
      InitNetTlsContext(TlsContext, {Server=}false, {CertFile=}'', {KeyFile=}'',
        {KeyPassword=}'', FOwnerComponent.FCACertificatesFile);
      TlsContext.IgnoreCertificateErrors := FOwnerComponent.FIgnoreCertificateErrors;

      // Create TLS interface
      fSecure := NewNetTls;

      if fSecure = nil then
        raise ENetSock.Create('TLS is not available - check TLS provider settings', []);

      // Log TLS setup
      if Assigned(FOwnerComponent) then
        FOwnerComponent.FLogFamily.SynLogClass.Add.Log(sllInfo,
          'TLS handshake starting (requested provider: %)',
          [GetEnumName(TypeInfo(TmMClientTlsProvider), Ord(FOwnerComponent.FTlsProvider))], self);

      // Make socket blocking for TLS handshake (following HTTP client pattern)
      fSocket.MakeBlocking;

      // Perform client-side TLS handshake
      fSecure.AfterConnection(fSocket, TlsContext, RawUtf8(FOwnerComponent.FHost));

      // Make socket async again after TLS handshake
      fSocket.MakeAsync;

      // Log successful handshake
      if Assigned(FOwnerComponent) then
        FOwnerComponent.FLogFamily.SynLogClass.Add.Log(sllInfo,
          'Client TLS handshake successful', [], self);

    except
      on E: Exception do
      begin
        fSecure := nil;
        if Assigned(FOwnerComponent) then
          FOwnerComponent.FLogFamily.SynLogClass.Add.Log(sllError,
            'Client TLS handshake failed: %', [E.Message], self);
        // Let the async framework handle the connection cleanup
        raise; // Re-raise to signal connection failure
      end;
    end;
  end;

  // EventsUseMainThread threading model
  if Assigned(FOwnerComponent) then
  begin
    FOwnerComponent.FConnection := Self;
    FOwnerComponent.FActive := True;

    if Assigned(FOwnerComponent.FOnConnect) then
    begin
      if FOwnerComponent.FEventsUseMainThread then
      begin
        // Fire event on main thread
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(FOwnerComponent) and Assigned(FOwnerComponent.FOnConnect) then
            try
              FOwnerComponent.FOnConnect(FOwnerComponent);
            except
              // Swallow event exceptions
            end;
          end);
      end
      else
      begin
        // Fire event on background thread directly
        try
          FOwnerComponent.FOnConnect(FOwnerComponent);
        except
          // Swallow event exceptions
        end;
      end;
    end;
  end;
end;

procedure TmMClientConnection.OnClose;
begin
  // EventsUseMainThread threading model
  if Assigned(FOwnerComponent) then
  begin
    if Assigned(FOwnerComponent.FOnDisconnect) then
    begin
      if FOwnerComponent.FEventsUseMainThread then
      begin
        // Fire event on main thread
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(FOwnerComponent) and Assigned(FOwnerComponent.FOnDisconnect) then
            try
              FOwnerComponent.FOnDisconnect(FOwnerComponent);
            except
              // Swallow event exceptions
            end;
          end);
      end
      else
      begin
        // Fire event on background thread directly
        try
          FOwnerComponent.FOnDisconnect(FOwnerComponent);
        except
          // Swallow event exceptions
        end;
      end;
    end;

    FOwnerComponent.FConnection := nil;
  end;

  inherited OnClose;
end;

function TmMClientConnection.OnRead: TPollAsyncSocketOnReadWrite;
var
  bufferData: RawByteString;
  data: TBytes;
  messageSize: UInt32;
  cmd: Integer;
begin
  result := soContinue;

  try
    // Process all available data in the buffer
    while fRd.Len > 0 do
    begin
      // Convert buffer to string for processing
      SetString(bufferData, PAnsiChar(fRd.Buffer), fRd.Len);

      // Check if buffer starts with our magic marker (protocol message)
      if TCommandProtocol.HasMagicMarker(bufferData) then
      begin
        // Try to parse protocol message
        if TCommandProtocol.TryParseMessage(bufferData, cmd, data) then
        begin
          // Calculate total message size and remove from buffer
          messageSize := SizeOf(TCommandMessage) + Length(data);
          fRd.Remove(messageSize);

          // Process protocol command (maps to OnHandleCommand)
          ProcessCommand(cmd, data);
        end
        else
        begin
          // Incomplete protocol message, wait for more data
          break;
        end;
      end
      else
      begin
        // Invalid data - not our protocol, close connection
        result := soClose;
        break;
      end;
    end;

  except
    on E: Exception do
    begin
      result := soClose;
    end;
  end;
end;

procedure TmMClientConnection.ProcessCommand(aCmd: Integer; const Data: TBytes);
var
  response: TBytes;
begin
  if not Assigned(FOwnerComponent) or not Assigned(FOwnerComponent.fOnHandleCommand) then
    Exit;

  // Pass the data and command to the event handler
  response := FOwnerComponent.fOnHandleCommand(FOwnerComponent, aCmd, Data);

  // Send response back if any
  if Length(response) > 0 then
    TCommandProtocol.SendMessage(SendProtocolMessage, aCmd, response);
end;

procedure TmMClientConnection.SendProtocolMessage(const Message: RawByteString);
begin
  fOwner.WriteString(self, Message);
end;


{ TmMClient }

constructor TmMClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  PropertyLock := TCriticalSection.Create;

  FInitActive := False;
  FActive := False;
  FHost := 'localhost';
  FPort := 3434;
  FConnectionTimeout := 10;
  FThreadPoolSize := 1;
  FConnection := nil;
  FClient := nil;

  FEventsUseMainThread := True;

  // Setup logging
  FLogFamily := TSynLog.Family;
  FLogFamily.PerThreadLog := ptIdentifiedInOnFile;

  // Initialize logging level
  FLogLevel := llNone;
  SetLogLevel(FLogLevel);

  // Initialize TLS settings
  FUseTLS := False;
  FIgnoreCertificateErrors := False;
  FCACertificatesFile := '';
end;

destructor TmMClient.Destroy;
begin
  Disconnect;
  PropertyLock.Free;
  inherited Destroy;
end;

// Loaded method
procedure TmMClient.Loaded;
begin
  inherited Loaded;

  if FInitActive then
    DoActivate(True);
end;

// Property methods with thread safety
function TmMClient.GetActive: Boolean;
begin
  PropertyLock.Acquire;
  try
    Result := FActive;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetActive(const Value: Boolean);
begin
  PropertyLock.Acquire;
  try
    if not(csLoading in ComponentState) then
      DoActivate(Value);

    FInitActive := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetHost: string;
begin
  PropertyLock.Acquire;
  try
    Result := FHost;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetHost(const Value: string);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create('Cannot change Host while connected');

  PropertyLock.Acquire;
  try
    FHost := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetPort: Integer;
begin
  PropertyLock.Acquire;
  try
    Result := FPort;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetPort(const Value: Integer);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create('Cannot change Port while connected');

  PropertyLock.Acquire;
  try
    FPort := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetConnectionTimeout: Integer;
begin
  PropertyLock.Acquire;
  try
    Result := FConnectionTimeout;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetConnectionTimeout(const Value: Integer);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create('Cannot change ConnectionTimeout while connected');

  PropertyLock.Acquire;
  try
    if Value < 1 then
      FConnectionTimeout := 1
    else
      FConnectionTimeout := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetThreadPoolSize: Integer;
begin
  PropertyLock.Acquire;
  try
    Result := FThreadPoolSize;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetThreadPoolSize(const Value: Integer);
begin
  if not(csLoading in ComponentState) then
    if GetActive then
      raise Exception.Create('Cannot change ThreadPoolSize while connected');

  PropertyLock.Acquire;
  try
    if Value < 1 then
      FThreadPoolSize := 1
    else
      FThreadPoolSize := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetEventsUseMainThread: Boolean;
begin
  PropertyLock.Acquire;
  try
    Result := FEventsUseMainThread;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetEventsUseMainThread(const Value: Boolean);
begin
  PropertyLock.Acquire;
  try
    FEventsUseMainThread := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetLogLevel: TmMClientLogLevel;
begin
  PropertyLock.Acquire;
  try
    Result := FLogLevel;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetLogLevel(const Value: TmMClientLogLevel);
begin
  PropertyLock.Acquire;
  try
    FLogLevel := Value;

    // Update the actual TSynLogFamily level using mORMot2's predefined filters
    case FLogLevel of
      llNone:
        FLogFamily.Level := LOG_FILTER[lfNone];         // No logging
      llErrors:
        FLogFamily.Level := LOG_FILTER[lfErrors];       // Only errors and exceptions
      llDebug:
        FLogFamily.Level := LOG_FILTER[lfDebug];        // Debug information
      llClientServer:
        FLogFamily.Level := LOG_FILTER[lfClientServer]; // Client/server communication
      llVerbose:
        FLogFamily.Level := LOG_VERBOSE;                // Everything
    end;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.DoActivate(aActivate: Boolean);
begin
  if aActivate = GetActive then
    Exit;

  if aActivate then
    Connect
  else
    Disconnect;
end;

procedure TmMClient.Connect;
var
  Options: TAsyncConnectionsOptions;
begin
  if FActive then
    Exit;

  if Assigned(FClient) then
  begin
    FClient.Free;
    FClient := nil;
  end;

  try
    GlobalClientComponent := Self;

    // Log TLS provider configuration
    if FUseTLS then
    begin
      FLogFamily.SynLogClass.Add.Log(sllInfo, 'TLS enabled with provider: %',
        [GetEnumName(TypeInfo(TmMClientTlsProvider), Ord(FTlsProvider))], self);
    end;

    // Setup connection options (TLS is handled manually in TmMClientConnection.AfterCreate)
    Options := [];

    // Create TAsyncClient with the same pattern as uClient.pas
    FClient := TAsyncClient.Create(
      FHost,                           // SERVER_IP
      IntToStr(FPort),                 // SERVER_PORT
      1,                               // CONNECTION_COUNT
      FConnectionTimeout,              // CONNECTION_TIMEOUT
      nil, nil,                        // OnStart, OnStop callbacks
      TmMClientConnection,             // Connection class
      'mMClient',                      // PROCESS_NAME
      FLogFamily.SynLogClass,          // Log class
      Options,                         // Options - include TLS if enabled
      FThreadPoolSize                  // THREAD_POOL_COUNT
    );

    // TLS is handled manually in TmMClientConnection.AfterCreate
    // No callback needed

    FActive := True;

  except
    on E: Exception do
    begin
      if Assigned(FClient) then
      begin
        FClient.Free;
        FClient := nil;
      end;
      FActive := False;
      raise;
    end;
  end;
end;


procedure TmMClient.Disconnect;
begin
  if not FActive then
    Exit;

  FActive := False;
  FConnection := nil;

  if Assigned(FClient) then
  begin
    FClient.Free;
    FClient := nil;
  end;
end;

procedure TmMClient.SendData(aCmd: Integer; const Data: TBytes);
begin
  if not FActive or not Assigned(FConnection) then
    raise Exception.Create('Client not connected');

  TCommandProtocol.SendMessage(FConnection.SendProtocolMessage, aCmd, Data);
end;

// *****************************************************************************
// TLS Property Implementations
// *****************************************************************************

function TmMClient.GetUseTLS: Boolean;
begin
  PropertyLock.Acquire;
  try
    Result := FUseTLS;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetUseTLS(const Value: Boolean);
begin
  if GetActive then
    raise Exception.Create('Cannot change UseTLS while connected');
  PropertyLock.Acquire;
  try
    FUseTLS := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetIgnoreCertificateErrors: Boolean;
begin
  PropertyLock.Acquire;
  try
    Result := FIgnoreCertificateErrors;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetIgnoreCertificateErrors(const Value: Boolean);
begin
  if GetActive then
    raise Exception.Create('Cannot change IgnoreCertificateErrors while connected');
  PropertyLock.Acquire;
  try
    FIgnoreCertificateErrors := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetCACertificatesFile: string;
begin
  PropertyLock.Acquire;
  try
    Result := FCACertificatesFile;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetCACertificatesFile(const Value: string);
begin
  if GetActive then
    raise Exception.Create('Cannot change CACertificatesFile while connected');
  PropertyLock.Acquire;
  try
    FCACertificatesFile := Value;
  finally
    PropertyLock.Release;
  end;
end;

function TmMClient.GetTlsProvider: TmMClientTlsProvider;
begin
  PropertyLock.Acquire;
  try
    Result := FTlsProvider;
  finally
    PropertyLock.Release;
  end;
end;

procedure TmMClient.SetTlsProvider(const Value: TmMClientTlsProvider);
begin
  if GetActive then
    raise Exception.Create('Cannot change TlsProvider while connected');
  PropertyLock.Acquire;
  try
    FTlsProvider := Value;
  finally
    PropertyLock.Release;
  end;
end;

// *****************************************************************************
// TLS Methods Implementation
// *****************************************************************************

// TLS setup now happens in TmMClientConnection.AfterCreate following HTTP client pattern

initialization
  // OpenSSL is available due to mormot.lib.openssl11 import
  // Component will handle TLS provider selection via TlsProvider property

end.

