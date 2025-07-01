unit mMServer;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.SyncObjs,
  mormot.core.base, mormot.core.log, mormot.core.rtti,
  mormot.net.sock, mormot.net.async, mormot.core.buffers,
  mormot.lib.openssl11, mMProtocol;

type
  // Type alias to hide mORMot2 internal types from application code
  // This allows applications to use the component without requiring mormot.net.async in their uses clause
  TmMClientHandle = TPollAsyncConnectionHandle;

  // Forward declarations
  TmMServer = class;
  TmMServerClient = class;

  // Logging level enumeration for easy component property
  // Maps directly to mORMot2's TSynLogFilter for consistency
  TmMServerLogLevel = (
    llNone,         // No logging (completely disabled)
    llErrors,       // Only errors, exceptions, and critical failures (LOG_FILTER[lfErrors])
    llDebug,        // Debug information including enter/leave (LOG_FILTER[lfDebug])
    llClientServer, // Client/server communication logs (LOG_FILTER[lfClientServer])
    llVerbose       // All logging (LOG_VERBOSE)
  );

  // TLS Provider enumeration
  TmMServerTlsProvider = (
    tpOpenSSL,      // Force OpenSSL (requires conditional defines + DLLs)
    tpSChannel      // Force Windows SChannel (Windows only, no defines)
  );

  // Event types
  TmMServerHandleCommandEvent = function(Sender: TObject; Client: TmMServerClient; aCmd: Integer; const aData: TBytes): TBytes of object;
  TmMServerConnectEvent = procedure(Sender: TObject; Client: TmMServerClient) of object;
  TmMServerDisconnectEvent = procedure(Sender: TObject; Client: TmMServerClient) of object;

  // Client information class
  TmMServerClient = class
  private
    fConnection: TAsyncConnection;
    fHandle: TPollAsyncConnectionHandle;
    fConnectedAt: TDateTime;
    fServer: TmMServer;
    fPeerIP: string;
    fPeerPort: Integer;
    procedure SendProtocolMessage(const Message: RawByteString);
  public
    constructor Create(AConnection: TAsyncConnection; AServer: TmMServer);
    destructor Destroy; override;

    // SendData method with command parameter
    procedure SendData(aCmd: Integer; const Data: TBytes);

    // Properties
    property Connection: TAsyncConnection read fConnection;
    property PeerIP: string read fPeerIP;
    property PeerPort: Integer read fPeerPort;
    property Handle: TmMClientHandle read fHandle;  // Using type alias
    property ConnectedAt: TDateTime read fConnectedAt;

    // Methods
    procedure Disconnect;
  end;

  // Internal connection class
  TmMServerConnection = class(TAsyncConnection)
  private
    fOwnerComponent: TmMServer;
    fClientObject: TmMServerClient;
    fRemoteAddr: TNetAddr;  // Store the remote address info
    procedure ProcessCommand(aCmd: Integer; const Data: TBytes);
    function GetRemoteIPDisplay: RawUtf8;
    procedure SendProtocolMessage(const Message: RawByteString);
  protected
    function OnRead: TPollAsyncSocketOnReadWrite; override;
    procedure AfterCreate; override; // Maps to OnConnect event
    procedure OnClose; override; // Maps to OnDisconnect event
  public
    constructor Create(aOwner: TAsyncConnections; const aRemoteIP: TNetAddr); override;

    property ClientObject: TmMServerClient read fClientObject;
    property RemoteIPDisplay: RawUtf8 read GetRemoteIPDisplay;
  end;

  // Main server component
  TmMServer = class(TComponent)
  private
    FServer: TAsyncServer;
    FClients: TObjectDictionary<TmMClientHandle, TmMServerClient>;
    FLogFamily: TSynLogFamily;
    FLogLevel: TmMServerLogLevel;
    FActive: Boolean;
    FInitActive: Boolean;
    FPort: Integer;
    FConnectionTimeout: Integer;
    FThreadPoolSize: Integer;

    // TLS Support
    FUseTLS: Boolean;
    FCertificateFile: string;
    FPrivateKeyFile: string;
    FPrivateKeyPassword: string;
    FCACertificatesFile: string;
    FIgnoreCertificateErrors: Boolean;
    FTlsProvider: TmMServerTlsProvider;

    // Add EventsUseMainThread
    FEventsUseMainThread: Boolean;

    // Events
    FOnHandleCommand: TmMServerHandleCommandEvent;
    FOnConnect: TmMServerConnectEvent;
    FOnDisconnect: TmMServerDisconnectEvent;

    // Property methods with thread safety
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetPort: Integer;
    procedure SetPort(const Value: Integer);
    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const Value: Integer);
    function GetThreadPoolSize: Integer;
    procedure SetThreadPoolSize(const Value: Integer);
    function GetEventsUseMainThread: Boolean;
    procedure SetEventsUseMainThread(const Value: Boolean);
    function GetLogLevel: TmMServerLogLevel;
    procedure SetLogLevel(const Value: TmMServerLogLevel);

    // TLS property getters/setters
    function GetUseTLS: Boolean;
    procedure SetUseTLS(const Value: Boolean);
    function GetCertificateFile: string;
    procedure SetCertificateFile(const Value: string);
    function GetPrivateKeyFile: string;
    procedure SetPrivateKeyFile(const Value: string);
    function GetPrivateKeyPassword: string;
    procedure SetPrivateKeyPassword(const Value: string);
    function GetCACertificatesFile: string;
    procedure SetCACertificatesFile(const Value: string);
    function GetIgnoreCertificateErrors: Boolean;
    procedure SetIgnoreCertificateErrors(const Value: Boolean);
    function GetTlsProvider: TmMServerTlsProvider;
    procedure SetTlsProvider(const Value: TmMServerTlsProvider);

    // Internal activation method
    procedure DoActivate(aActivate: Boolean);

  protected
    // Thread safety
    PropertyLock: TCriticalSection;
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Server methods
    procedure Connect;
    procedure Disconnect;

    // Client management methods
    procedure DisconnectClient(Client: TmMServerClient);
    procedure DisconnectAllClients;

    // Properties (read-only at runtime)
    property Clients: TObjectDictionary<TmMClientHandle, TmMServerClient> read FClients;  // Using type alias

  published
    // Properties with proper getters/setters
    property Active: Boolean read GetActive write SetActive default False;
    property Port: Integer read GetPort write SetPort default 3434;
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout default 10;
    property ThreadPoolSize: Integer read GetThreadPoolSize write SetThreadPoolSize default 4;

    // EventsUseMainThread property
    property EventsUseMainThread: Boolean read GetEventsUseMainThread write SetEventsUseMainThread default True;

    // Logging control
    property LogLevel: TmMServerLogLevel read GetLogLevel write SetLogLevel default llErrors;

    // TLS Properties
    property UseTLS: Boolean read GetUseTLS write SetUseTLS default False;
    property CertificateFile: string read GetCertificateFile write SetCertificateFile;
    property PrivateKeyFile: string read GetPrivateKeyFile write SetPrivateKeyFile;
    property PrivateKeyPassword: string read GetPrivateKeyPassword write SetPrivateKeyPassword;
    property CACertificatesFile: string read GetCACertificatesFile write SetCACertificatesFile;
    property IgnoreCertificateErrors: Boolean read GetIgnoreCertificateErrors write SetIgnoreCertificateErrors default False;
    property TlsProvider: TmMServerTlsProvider read GetTlsProvider write SetTlsProvider default tpOpenSSL;

    // Events
    property OnHandleCommand: TmMServerHandleCommandEvent read FOnHandleCommand write FOnHandleCommand;
    property OnConnect: TmMServerConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TmMServerDisconnectEvent read FOnDisconnect write FOnDisconnect;
  end;

// Global server reference for connection callback
var
  GlobalServerComponent: TmMServer = nil;

implementation

{ TmMServerClient }

constructor TmMServerClient.Create(AConnection: TAsyncConnection; AServer: TmMServer);
begin
  inherited Create;
  fConnection := AConnection;
  fServer := AServer;
  fHandle := AConnection.Handle;
  fConnectedAt := Now;
  fPeerIP := '127.0.0.1';
  fPeerPort := 0;
end;

destructor TmMServerClient.Destroy;
begin
  inherited Destroy;
end;

procedure TmMServerClient.SendProtocolMessage(const Message: RawByteString);
begin
  if Assigned(fConnection) then
    TmMServerConnection(fConnection).SendProtocolMessage(Message);
end;

procedure TmMServerClient.SendData(aCmd: Integer; const Data: TBytes);
begin
  TCommandProtocol.SendMessage(SendProtocolMessage, aCmd, Data);
end;

procedure TmMServerClient.Disconnect;
begin
  if not Assigned(fServer) then
    Exit;

  // Use thread-safe approach with PropertyLock
  fServer.PropertyLock.Enter;
  try
    // Double-check connection is still valid and in the clients dictionary
    if Assigned(fConnection) and
       Assigned(fServer.fServer) and
       fServer.FClients.ContainsKey(fHandle) then
    begin
      try
        // Use fHandle instead of fConnection.Handle for consistency
        fServer.fServer.ConnectionRemove(fHandle);
      except
        // If ConnectionRemove fails, force cleanup
        if fServer.FClients.ContainsKey(fHandle) then
          fServer.FClients.Remove(fHandle);
      end;
    end;
  finally
    fServer.PropertyLock.Leave;
  end;
end;

{ TmMServerConnection }

constructor TmMServerConnection.Create(aOwner: TAsyncConnections; const aRemoteIP: TNetAddr);
begin
  inherited Create(aOwner, aRemoteIP);
  fOwnerComponent := GlobalServerComponent;
  fRemoteAddr := aRemoteIP;  // Store the remote address

  // Override the RemoteIP to show localhost properly
  if fRemoteIP = '' then
    aRemoteIP.IP(fRemoteIP, {localasvoid=}false); // Show 127.0.0.1 instead of empty
end;

function TmMServerConnection.GetRemoteIPDisplay: RawUtf8;
begin
  if fRemoteIP = '' then
    Result := '127.0.0.1'
  else
    Result := fRemoteIP;
end;

procedure TmMServerConnection.AfterCreate;
begin
  inherited AfterCreate;

  // EventsUseMainThread threading model
  if Assigned(fOwnerComponent) then
  begin
    fOwnerComponent.PropertyLock.Enter;
    try
      // Create client object
      fClientObject := TmMServerClient.Create(Self, fOwnerComponent);
      fClientObject.fPeerIP := string(RemoteIPDisplay);

      // Set the SERVER port (the port client connected TO), not client's bind port
      fClientObject.fPeerPort := fOwnerComponent.FPort;

      // Add to clients dictionary using Handle as key
      fOwnerComponent.FClients.AddOrSetValue(Handle, fClientObject);

      // Fire OnConnect event with threading control
      if Assigned(fOwnerComponent.FOnConnect) then
      begin
        if fOwnerComponent.FEventsUseMainThread then
        begin
          // Fire event on main thread
          TThread.Queue(nil,
            procedure
            begin
              if Assigned(fOwnerComponent) and Assigned(fOwnerComponent.FOnConnect) and Assigned(fClientObject) then
              try
                fOwnerComponent.FOnConnect(fOwnerComponent, fClientObject);
              except
                // Swallow event exceptions
              end;
            end);
        end
        else
        begin
          // Fire event on background thread directly
          try
            fOwnerComponent.FOnConnect(fOwnerComponent, fClientObject);
          except
            // Swallow event exceptions
          end;
        end;
      end;

    finally
      fOwnerComponent.PropertyLock.Leave;
    end;
  end;
end;

procedure TmMServerConnection.OnClose;
begin
  // EventsUseMainThread threading model
  if Assigned(fOwnerComponent) then
  begin
    fOwnerComponent.PropertyLock.Enter;
    try
      // Fire OnDisconnect event before cleanup with threading control
      if Assigned(fClientObject) and Assigned(fOwnerComponent.FOnDisconnect) then
      begin
        if fOwnerComponent.FEventsUseMainThread then
        begin
          // Fire event on main thread
          TThread.Queue(nil,
            procedure
            begin
              if Assigned(fOwnerComponent) and Assigned(fOwnerComponent.FOnDisconnect) and Assigned(fClientObject) then
              try
                fOwnerComponent.FOnDisconnect(fOwnerComponent, fClientObject);
              except
                // Swallow event exception
              end;
            end);
        end
        else
        begin
          // Fire event on background thread directly
          try
            fOwnerComponent.FOnDisconnect(fOwnerComponent, fClientObject);
          except
            // Swallow event exceptions
          end;
        end;
      end;

      // Remove from clients dictionary
      if fOwnerComponent.FClients.ContainsKey(Handle) then
        fOwnerComponent.FClients.Remove(Handle);

    finally
      fOwnerComponent.PropertyLock.Leave;
    end;
  end;

  inherited OnClose;
end;

function TmMServerConnection.OnRead: TPollAsyncSocketOnReadWrite;
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

procedure TmMServerConnection.ProcessCommand(aCmd: Integer; const Data: TBytes);
var
  response: TBytes;
begin
  if not Assigned(fOwnerComponent) or not Assigned(fOwnerComponent.FOnHandleCommand) or not Assigned(fClientObject) then
    Exit;

  // Pass the data and command to the event handler
  response := fOwnerComponent.FOnHandleCommand(fOwnerComponent, fClientObject, aCmd, Data);

  // Send response back if any
  if Length(response) > 0 then
    TCommandProtocol.SendMessage(SendProtocolMessage, aCmd, response);
end;

procedure TmMServerConnection.SendProtocolMessage(const Message: RawByteString);
begin
  fOwner.WriteString(self, Message);
end;

{ TmMServer }

constructor TmMServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  PropertyLock := TCriticalSection.Create;

  FInitActive := False;
  FActive := False;
  FPort := 3434;
  FConnectionTimeout := 10;
  FThreadPoolSize := 4;
  FServer := nil;

  FEventsUseMainThread := True;

  FClients := TObjectDictionary<TmMClientHandle, TmMServerClient>.Create([doOwnsValues]);

  // Setup logging
  FLogFamily := TSynLog.Family;
  FLogFamily.PerThreadLog := ptIdentifiedInOnFile;

  // Initialize logging level
  FLogLevel := llNone;
  SetLogLevel(FLogLevel);

  // Initialize TLS settings
  FUseTLS := False;
  FCertificateFile := '';
  FPrivateKeyFile := '';
  FPrivateKeyPassword := '';
  FCACertificatesFile := '';
  FIgnoreCertificateErrors := False;
end;

destructor TmMServer.Destroy;
begin
  Disconnect;
  PropertyLock.Free;
  FClients.Free;
  inherited Destroy;
end;

procedure TmMServer.Loaded;
begin
  inherited Loaded;

  if FInitActive then
    DoActivate(True);
end;

procedure TmMServer.DoActivate(aActivate: Boolean);
begin
  if FActive = aActivate then
    Exit;

  if csLoading in ComponentState then
  begin
    FActive := aActivate;
    Exit;
  end;

  if aActivate then
    Connect
  else
    Disconnect;
end;

function TmMServer.GetActive: Boolean;
begin
  PropertyLock.Enter;
  try
    Result := FActive;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetActive(const Value: Boolean);
begin
  DoActivate(Value);
end;

function TmMServer.GetPort: Integer;
begin
  PropertyLock.Enter;
  try
    Result := FPort;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetPort(const Value: Integer);
begin
  if FActive then
    raise Exception.Create('Cannot change Port while server is active');
  PropertyLock.Enter;
  try
    FPort := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetConnectionTimeout: Integer;
begin
  PropertyLock.Enter;
  try
    Result := FConnectionTimeout;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetConnectionTimeout(const Value: Integer);
begin
  if FActive then
    raise Exception.Create('Cannot change ConnectionTimeout while server is active');

  if Value < 1 then
    FConnectionTimeout := 1
  else
    FConnectionTimeout := Value;
end;

function TmMServer.GetThreadPoolSize: Integer;
begin
  PropertyLock.Enter;
  try
    Result := FThreadPoolSize;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetThreadPoolSize(const Value: Integer);
begin
  if FActive then
    raise Exception.Create('Cannot change ThreadPoolSize while server is active');

  if Value < 1 then
    FThreadPoolSize := 1
  else
    FThreadPoolSize := Value;
end;

function TmMServer.GetEventsUseMainThread: Boolean;
begin
  PropertyLock.Enter;
  try
    Result := FEventsUseMainThread;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetEventsUseMainThread(const Value: Boolean);
begin
  PropertyLock.Enter;
  try
    FEventsUseMainThread := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetLogLevel: TmMServerLogLevel;
begin
  PropertyLock.Enter;
  try
    Result := FLogLevel;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetLogLevel(const Value: TmMServerLogLevel);
begin
  PropertyLock.Enter;
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
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.DisconnectClient(Client: TmMServerClient);
begin
  if not Assigned(Client) then
    Exit;

  PropertyLock.Enter;
  try
    // Check if client is still in our dictionary
    if FClients.ContainsKey(Client.Handle) then
    begin
      try
        // Remove from server connection pool
        if Assigned(FServer) then
          FServer.ConnectionRemove(Client.Handle);
      except
        on E: Exception do
        begin
          // Log the actual error instead of swallowing it
          // AddLogMessage('ConnectionRemove failed: ' + E.Message);
        end;
      end;

      // FORCE remove from dictionary regardless
      FClients.Remove(Client.Handle);
    end;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.DisconnectAllClients;
var
  ClientHandles: TArray<TmMClientHandle>;
  Handle: TmMClientHandle;
begin
  PropertyLock.Enter;
  try
    // Get all client handles first to avoid modifying dictionary while iterating
    ClientHandles := FClients.Keys.ToArray;
  finally
    PropertyLock.Leave;
  end;

  // Disconnect each client
  for Handle in ClientHandles do
  begin
    try
      if Assigned(FServer) then
        FServer.ConnectionRemove(Handle);
    except
      // Continue with next client if one fails
    end;
  end;
end;

procedure TmMServer.Connect;
var
  Options: TAsyncConnectionsOptions;
begin
  if FActive then
    Exit;

  if Assigned(FServer) then
  begin
    FServer.Free;
    FServer := nil;
  end;

  try
    GlobalServerComponent := Self;

    // Log TLS provider configuration
    if FUseTLS then
    begin
      FLogFamily.SynLogClass.Add.Log(sllInfo, 'TLS enabled with provider: %',
        [GetEnumName(TypeInfo(TmMServerTlsProvider), Ord(FTlsProvider))], self);
    end;

    // Setup connection options
    Options := [acoReusePort];
    if FUseTLS then
      Include(Options, acoEnableTls);

    // Create TAsyncServer with the same pattern as uServer.pas
    FServer := TAsyncServer.Create(
      IntToStr(FPort),                 // SERVER_PORT
      nil, nil,                        // OnStart, OnStop callbacks
      TmMServerConnection,             // Connection class
      'mMServer',                      // PROCESS_NAME
      FLogFamily.SynLogClass,          // Log class
      Options,                         // Options - include TLS if enabled
      FThreadPoolSize                  // THREAD_POOL_COUNT
    );

    // Wait for server to start first (this initializes the Server socket)
    FServer.WaitStarted(FConnectionTimeout);

    // Configure TLS AFTER server socket is initialized
    if FUseTLS then
    begin
      // Validate certificate files exist
      if not FileExists(FCertificateFile) then
        raise Exception.CreateFmt('Certificate file not found: %s', [FCertificateFile]);
      
      // Only check private key file for OpenSSL (SChannel uses PFX which contains both)
      if (FTlsProvider = tpOpenSSL) and not FileExists(FPrivateKeyFile) then
        raise Exception.CreateFmt('Private key file not found: %s', [FPrivateKeyFile]);

      // Log TLS provider being used
      if FTlsProvider = tpOpenSSL then
        FLogFamily.SynLogClass.Add.Log(sllInfo,
          'Configuring TLS with OpenSSL (Certificate: %, Key: %)',
          [FCertificateFile, FPrivateKeyFile], self)
      else if FTlsProvider = tpSChannel then
        FLogFamily.SynLogClass.Add.Log(sllInfo,
          'Configuring TLS with SChannel (PFX: %)',
          [FCertificateFile], self)
      else
        FLogFamily.SynLogClass.Add.Log(sllInfo,
          'Configuring TLS with provider: %',
          [GetEnumName(TypeInfo(TmMServerTlsProvider), Ord(FTlsProvider))], self);

      FServer.Server.TLS.CertificateFile := RawUtf8(FCertificateFile);
      
      // Only set private key settings for OpenSSL (SChannel uses PFX)
      if FTlsProvider = tpOpenSSL then
      begin
        FServer.Server.TLS.PrivateKeyFile := RawUtf8(FPrivateKeyFile);
        FServer.Server.TLS.PrivatePassword := RawUtf8(FPrivateKeyPassword);
        FServer.Server.TLS.CACertificatesFile := RawUtf8(FCACertificatesFile);
      end
      else if FTlsProvider = tpSChannel then
      begin
        // For SChannel, use PrivatePassword as PFX password
        FServer.Server.TLS.PrivatePassword := RawUtf8(FPrivateKeyPassword);
      end;
      
      FServer.Server.TLS.IgnoreCertificateErrors := FIgnoreCertificateErrors;

      // Activate TLS after configuration
      try
        FServer.Server.DoTlsAfter(mormot.net.sock.cstaBind);
        FLogFamily.SynLogClass.Add.Log(sllInfo, 'TLS configuration successful', [], self);
      except
        on E: Exception do
        begin
          FLogFamily.SynLogClass.Add.Log(sllError,
            'TLS configuration failed: %', [E.Message], self);
          raise;
        end;
      end;
    end;
    FActive := True;

  except
    on E: Exception do
    begin
      if Assigned(FServer) then
      begin
        FServer.Free;
        FServer := nil;
      end;
      FActive := False;
      raise;
    end;
  end;
end;

procedure TmMServer.Disconnect;
begin
  if not FActive then
    Exit;

  FActive := False;

  if Assigned(FServer) then
  begin
    try
      // Properly shutdown the server first
      FServer.Shutdown;
    except
      // Ignore shutdown errors
    end;

    try
      // Then free it
      FServer.Free;
    finally
      FServer := nil;
    end;
  end;

  // Clear clients
  PropertyLock.Enter;
  try
    FClients.Clear;
  finally
    PropertyLock.Leave;
  end;
end;

// *****************************************************************************
// TLS Property Implementations
// *****************************************************************************

function TmMServer.GetUseTLS: Boolean;
begin
  PropertyLock.Enter;
  try
    Result := FUseTLS;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetUseTLS(const Value: Boolean);
begin
  if FActive then
    raise Exception.Create('Cannot change UseTLS while server is active');
  PropertyLock.Enter;
  try
    FUseTLS := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetCertificateFile: string;
begin
  PropertyLock.Enter;
  try
    Result := FCertificateFile;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetCertificateFile(const Value: string);
begin
  if FActive then
    raise Exception.Create('Cannot change CertificateFile while server is active');
  PropertyLock.Enter;
  try
    FCertificateFile := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetPrivateKeyFile: string;
begin
  PropertyLock.Enter;
  try
    Result := FPrivateKeyFile;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetPrivateKeyFile(const Value: string);
begin
  if FActive then
    raise Exception.Create('Cannot change PrivateKeyFile while server is active');
  PropertyLock.Enter;
  try
    FPrivateKeyFile := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetPrivateKeyPassword: string;
begin
  PropertyLock.Enter;
  try
    Result := FPrivateKeyPassword;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetPrivateKeyPassword(const Value: string);
begin
  if FActive then
    raise Exception.Create('Cannot change PrivateKeyPassword while server is active');
  PropertyLock.Enter;
  try
    FPrivateKeyPassword := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetCACertificatesFile: string;
begin
  PropertyLock.Enter;
  try
    Result := FCACertificatesFile;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetCACertificatesFile(const Value: string);
begin
  if FActive then
    raise Exception.Create('Cannot change CACertificatesFile while server is active');
  PropertyLock.Enter;
  try
    FCACertificatesFile := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetIgnoreCertificateErrors: Boolean;
begin
  PropertyLock.Enter;
  try
    Result := FIgnoreCertificateErrors;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetIgnoreCertificateErrors(const Value: Boolean);
begin
  if FActive then
    raise Exception.Create('Cannot change IgnoreCertificateErrors while server is active');
  PropertyLock.Enter;
  try
    FIgnoreCertificateErrors := Value;
  finally
    PropertyLock.Leave;
  end;
end;

function TmMServer.GetTlsProvider: TmMServerTlsProvider;
begin
  PropertyLock.Enter;
  try
    Result := FTlsProvider;
  finally
    PropertyLock.Leave;
  end;
end;

procedure TmMServer.SetTlsProvider(const Value: TmMServerTlsProvider);
begin
  if FActive then
    raise Exception.Create('Cannot change TlsProvider while server is active');
  PropertyLock.Enter;
  try
    FTlsProvider := Value;
  finally
    PropertyLock.Leave;
  end;
end;

initialization
  // OpenSSL is available due to mormot.lib.openssl11 import
  // Component will handle TLS provider selection via TlsProvider property

end.


