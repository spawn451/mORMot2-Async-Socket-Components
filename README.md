# mORMot2 Async Socket Components

A high-performance, easy-to-use Delphi component suite that provides a simplified wrapper around [mORMot2's](https://github.com/synopse/mORMot2) powerful asynchronous socket framework. These components abstract the complexity of mORMot2's low-level networking while preserving its exceptional performance and scalability.

## Overview

The mORMot2 Async Socket Components consist of visual components that can be dropped onto Delphi forms, providing enterprise-grade networking capabilities with minimal code. Built on top of mORMot2's proven async socket architecture, these components are perfect for building high-performance client-server applications, real-time communication systems, and scalable network services.


<p align="center">
  <img src="demo.png" alt="" style="max-width:100%; height:auto;">
</p>

## Key Features

### 🚀 **High Performance**
- Built on mORMot2's battle-tested async socket framework
- Non-blocking I/O with efficient thread pool management
- Handles thousands of concurrent connections with minimal resource usage

### 🛠️ **Developer Friendly**
- Visual components for RAD development
- Event-driven architecture with familiar Delphi patterns
- Command-based messaging system for structured communication
- Comprehensive error handling and logging

### 🔒 **Security**
- Full TLS 1.2/1.3 support
- Multiple TLS providers: OpenSSL and Windows SChannel
- Certificate-based authentication

### 📡 **Protocol Features**
- Binary protocol with magic markers for reliable data framing
- Command-based message routing
- Efficient binary data transmission

## Components

### TmMServer
Server component that accepts and manages multiple client connections.

**Key Properties:**
- `Active`: Enable/disable the server
- `Port`: Listening port (default: 3434)
- `UseTLS`: Enable SSL/TLS encryption
- `ThreadPoolSize`: Number of worker threads
- `ConnectionTimeout`: Client timeout in seconds
- `EventsUseMainThread`: Route events to main thread

**Key Events:**
- `OnConnect`: Fired when a client connects
- `OnDisconnect`: Fired when a client disconnects
- `OnHandleCommand`: Process incoming client commands

### TmMClient
Client component for connecting to mORMot2 servers.

**Key Properties:**
- `Active`: Connect/disconnect
- `Host`: Server hostname or IP
- `Port`: Server port
- `UseTLS`: Enable SSL/TLS encryption

**Key Methods:**
- `SendData(aCmd: Integer; const Data: TBytes)`: Send data to server

**Key Events:**
- `OnConnect`: Fired when connected to server
- `OnDisconnect`: Fired when disconnected
- `OnHandleCommand`: Process incoming server commands

### TmMServerClient
Represents an individual client connection on the server side.

**Key Methods:**
- `SendData(aCmd: Integer; const Data: TBytes)`: Send data to this client
- `Disconnect()`: Disconnect this specific client

**Key Properties:**
- `PeerIP`: Client's IP address
- `ConnectedAt`: Connection timestamp
- `Handle`: Unique connection identifier

## Quick Start

### Installation

1. Open mORMot2Components.dpk
2. On mORMot2Components.bpl → Right click → Install

### Library Path Setup
After installing the component, you must add the component folder to your Delphi library path:

1. **Tools → Options → Language → Delphi → Library**
2. **Library path - Click "..." next to "Library path"**
3. **Add the full path of the component folder**
   - Example: `C:\Library\mORMot2 Async Socket Components\Source`
4. **Click OK to save**

### Basic Server Example

```pascal
// Drop TmMServer on form
procedure TForm1.FormCreate(Sender: TObject);
begin
  mMServer1.Port := 8080;
  mMServer1.Active := True;
end;

// Handle incoming commands
function TForm1.mMServer1HandleCommand(Sender: TObject; Client: TmMServerClient; 
  aCmd: Integer; const aData: TBytes): TBytes;
begin
  case aCmd of
    1: begin
         // Handle login command
         ShowMessage('Client login: ' + StringOf(aData));
       end;
    2: begin
         // Handle chat message
         BroadcastToAllClients(aData);
       end;
  end;
end;

// Client connected
procedure TForm1.mMServer1Connect(Sender: TObject; Client: TmMServerClient);
begin
  ShowMessage('Client connected from: ' + Client.PeerIP);
end;
```

### Basic Client Example

```pascal
// Drop TmMClient on form
procedure TForm1.FormCreate(Sender: TObject);
begin
  mMClient1.Host := 'localhost';
  mMClient1.Port := 8080;
  mMClient1.Active := True;
end;

// Send data to server
procedure TForm1.Button1Click(Sender: TObject);
begin
  mMClient1.SendData(1, BytesOf('Username|Password'));
end;

// Handle server responses
function TForm1.mMClient1HandleCommand(Sender: TObject; aCmd: Integer; const aData: TBytes): TBytes;
begin
  case aCmd of
    10: ShowMessage('Server says: ' + StringOf(aData));
    20: ProcessFileData(aData);
  end;
end;
```

## Bidirectional Communication

Both client and server can send data to each other at any time using the `SendData` method:

- **Client to Server**: `mMClient1.SendData(aCmd, Data)`
- **Server to Client**: `Client.SendData(aCmd, Data)` (where Client is TmMServerClient)

## Command-Based Messaging

The components use a command-based protocol for structured communication:

```pascal
// Server sending to client
Client.SendData(100, BytesOf('Welcome to server'));
Client.SendData(200, UserListData); 

// Client sending to server
mMClient1.SendData(1, BytesOf('CHAT|Hello everyone!'));
mMClient1.SendData(2, ScreenshotData);
```

## TLS/SSL Configuration

### Using OpenSSL

Required Files:

#### Windows x64:
```
libssl-3-x64.dll
libcrypto-3-x64.dll
```

#### Windows x86:
```
libssl-3.dll  
libcrypto-3.dll
```

Add these conditional defines in **Project → Options → Conditional Defines**:
```
USE_OPENSSL
FORCE_OPENSSL
```
```pascal
// SERVER
mMServer1.UseTLS := True;
mMServer1.TlsProvider := tpOpenSSL;
mMServer1.CertificateFile := ExtractFilePath(Application.ExeName) + 'server.pem';
mMServer1.PrivateKeyFile := ExtractFilePath(Application.ExeName) + 'server.key';
mMServer1.IgnoreCertificateErrors := True; //Self-signed certificate
// Optional: specify CA bundle for certificate validation
// mMServer1.CACertificatesFile := 'ca-bundle.pem'; 
```
```pascal
// CLIENT
mMClient1.UseTLS := True;
mMClient1.TlsProvider := tpOpenSSL;
mMClient1.IgnoreCertificateErrors := True;  // For self-signed server cert
// Optional: specify CA bundle for certificate validation
// mMClient1.CACertificatesFile := 'ca-bundle.pem';
```
### Using Windows SChannel (Windows only)

No additional files required - uses Windows built-in TLS.

**IMPORTANT**: Remove ALL OpenSSL conditional defines from project to use SChannel:
- Remove `USE_OPENSSL` 
- Remove `FORCE_OPENSSL`

```pascal
// SERVER
mMServer1.UseTLS := True;
mMServer1.TlsProvider := tpSChannel;
mMServer1.CertificateFile := 'server.pfx';
mMServer1.PrivateKeyPassword := 'certificatepassword';
mMServer1.IgnoreCertificateErrors := True; //Self-signed certificate
```
```pascal
// CLIENT
mMClient1.UseTLS := True;
mMClient1.TlsProvider := tpSChannel;
mMClient1.IgnoreCertificateErrors := True; //Self-signed certificate
```



## Tested on

- **Delphi**: 12.2 Athens
- **Platform**: Windows (x86/x64)

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Your Application                         │
├─────────────────────────────────────────────────────────────┤
│           mORMot2 Async Socket Components                   │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │  TmMServer  │  │  TmMClient  │  │  TCommandProtocol   │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
├─────────────────────────────────────────────────────────────┤
│                     mORMot2 Framework                       │
│         (TAsyncServer, TAsyncClient, TAsyncConnection)      │
├─────────────────────────────────────────────────────────────┤
│                Operating System Socket API                  │
└─────────────────────────────────────────────────────────────┘
```

## Protocol Specification

The components use a binary protocol with the following message structure:

```
┌───────────────────────────────────────────────────────────┐
│                    Message Header                         │
├─────────────┬─────────────┬─────────────┬─────────────────┤
│   Magic     │   Command   │  Data Size  │     Data        │
│  (4 bytes)  │  (4 bytes)  │  (4 bytes)  │   (variable)    │
│ 0xCAFEBABE  │   Integer   │   UInt32    │     TBytes      │
└─────────────┴─────────────┴─────────────┴─────────────────┘
```


## Contributing

Contributions are welcome!


## Credits

- Built on the excellent [mORMot2](https://github.com/synopse/mORMot2) framework by Arnaud Bouchez
- Inspired by the need for easier async socket programming in Delphi

## Support

- **Issues**: Report bugs and request features via GitHub Issues
- **Documentation**: See the demo projects for comprehensive examples
---

<p align="center">Made with ❤️ using Delphi RAD Studio</p>