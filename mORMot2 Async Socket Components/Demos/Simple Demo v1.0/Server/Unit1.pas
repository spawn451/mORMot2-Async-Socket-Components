unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, mMServer, Vcl.ComCtrls,
  Vcl.Menus, Vcl.StdCtrls, Vcl.Samples.Spin, System.Generics.Collections,
  System.StrUtils, System.SyncObjs, Vcl.ExtCtrls, Vcl.Imaging.jpeg, System.ZLib;

type
  // Simple client data - only what we need beyond mORMot2's built-in info
  TClientInfo = record
    Nickname: string;        // Human-readable display name (what users see)
    ClientID: string;        // Optional: Application-level identifier
    ConnectedAt: TDateTime;  // When they connected
    LastActivity: TDateTime; // Last activity timestamp
  end;

type
  TForm1 = class(TForm)
    mMServer1: TmMServer;
    StatusBar1: TStatusBar;
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    S1: TMenuItem;
    S2: TMenuItem;
    G1: TMenuItem;
    LogMemo: TMemo;
    gb1: TGroupBox;
    btnActivate: TButton;
    sedtPort: TSpinEdit;
    btnClearLog: TButton;
    Image1: TImage;
    G2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mMServer1Connect(Sender: TObject; Client: TmMServerClient);
    procedure mMServer1Disconnect(Sender: TObject; Client: TmMServerClient);
    function mMServer1HandleCommand(Sender: TObject; Client: TmMServerClient;
      aCmd: Integer; const aData: TBytes): TBytes;
    procedure AddLogMessage(const Msg: string);
    procedure btnActivateClick(Sender: TObject);
    procedure sedtPortChange(Sender: TObject);
    procedure S1Click(Sender: TObject);
    procedure S2Click(Sender: TObject);
    procedure G1Click(Sender: TObject);
    procedure D1Click(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure G2Click(Sender: TObject);
  private
    // SIMPLE: Only track application data, let mORMot2 handle connections
    FClientInfo: TDictionary<TmMClientHandle, TClientInfo>;
    FClientLock: TCriticalSection;

    // Track which client's screenshot is currently displayed
    FCurrentScreenshotClient: TmMServerClient;

    // Helper methods
    procedure RefreshClientList;
    procedure UpdateStatusBar;
    procedure AddClientToList(Client: TmMServerClient);
    procedure RemoveClientFromList(Client: TmMServerClient);
    procedure UpdateClientInList(Client: TmMServerClient);

    // Simple client operations
    procedure SetClientNickname(Client: TmMServerClient; const Nickname: string);
    function GetClientNickname(Client: TmMServerClient): string;
    function GetSelectedClient: TmMServerClient;
    procedure SendToAllClients(const Message: string = '');
    procedure SendToSelectedClient;
    procedure DisconnectSelectedClient;

    // Screenshot management
    procedure DisplayScreenShot(const aData: TBytes; FromClient: TmMServerClient);
    procedure ClearScreenshot;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Simple initialization
  FClientInfo := TDictionary<TmMClientHandle, TClientInfo>.Create;
  FClientLock := TCriticalSection.Create;
  FCurrentScreenshotClient := nil;

  UpdateStatusBar;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Clear event handlers first
  if Assigned(mMServer1) then
  begin
    mMServer1.OnConnect := nil;
    mMServer1.OnDisconnect := nil;
    mMServer1.OnHandleCommand := nil;
    mMServer1.Active := False;
  end;

  // Cleanup
  FClientLock.Free;
  FClientInfo.Free;
end;

procedure TForm1.RefreshClientList;
var
  Client: TmMServerClient;
  ClientInfo: TClientInfo;
  ListItem: TListItem;
  DisplayName: string;
begin
  // SIMPLE: Use mMServer1.Clients as the source of truth
  ListView1.Clear;

  // Iterate through ACTUAL connected clients (mORMot2 manages this)
  for Client in mMServer1.Clients.Values do
  begin
    ListItem := ListView1.Items.Add;
    ListItem.Data := Pointer(Client); // Store actual client reference

    // Get nickname (client always sends one)
    DisplayName := GetClientNickname(Client);

    // Populate ListView with actual client data
    ListItem.Caption := DisplayName;
    ListItem.SubItems.Add(Client.PeerIP);
    ListItem.SubItems.Add(FormatDateTime('hh:nn:ss', Client.ConnectedAt));
    ListItem.SubItems.Add(IntToStr(NativeInt(Client.Handle)));
  end;

  UpdateStatusBar;
end;

procedure TForm1.UpdateStatusBar;
begin
  // Use mMServer1.Clients.Count - the actual truth
  StatusBar1.Panels[0].Text := Format('Status: %s - Port: %d',
                                      [IfThen(mMServer1.Active, 'Online', 'Offline'), mMServer1.Port]);
  StatusBar1.Panels[1].Text := Format('Clients Connected: %d', [mMServer1.Clients.Count]);
end;

procedure TForm1.AddClientToList(Client: TmMServerClient);
var
  ListItem: TListItem;
begin
  // Add single client to ListView (efficient)
  ListItem := ListView1.Items.Add;
  ListItem.Data := Pointer(Client); // Store client reference

  // Client will send nickname immediately, so start with IP for now
  ListItem.Caption := Client.PeerIP;
  ListItem.SubItems.Add(Client.PeerIP);
  ListItem.SubItems.Add(FormatDateTime('hh:nn:ss', Client.ConnectedAt));
  ListItem.SubItems.Add(IntToStr(NativeInt(Client.Handle)));
end;

procedure TForm1.RemoveClientFromList(Client: TmMServerClient);
var
  i: Integer;
  ListItem: TListItem;
begin
  // Find and remove specific client from ListView (efficient)
  for i := ListView1.Items.Count - 1 downto 0 do
  begin
    ListItem := ListView1.Items[i];
    if TmMServerClient(ListItem.Data) = Client then
    begin
      ListView1.Items.Delete(i);
      Break; // Found and removed, exit
    end;
  end;
end;

procedure TForm1.UpdateClientInList(Client: TmMServerClient);
var
  i: Integer;
  ListItem: TListItem;
begin
  // Update specific client in ListView (efficient)
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    ListItem := ListView1.Items[i];
    if TmMServerClient(ListItem.Data) = Client then
    begin
      // Update with nickname (client always sends one)
      ListItem.Caption := GetClientNickname(Client);
      Break; // Found and updated, exit
    end;
  end;
end;

// *****************************************************************************
// Client Management
// *****************************************************************************

procedure TForm1.SetClientNickname(Client: TmMServerClient; const Nickname: string);
var
  Info: TClientInfo;
begin
  FClientLock.Enter;
  try
    // Get existing info or create new
    if not FClientInfo.TryGetValue(Client.Handle, Info) then
    begin
      Info.ConnectedAt := Client.ConnectedAt;
      Info.LastActivity := Now;
    end;

    Info.Nickname := Nickname;
    Info.LastActivity := Now;

    FClientInfo.AddOrSetValue(Client.Handle, Info);
  finally
    FClientLock.Leave;
  end;
end;

function TForm1.GetClientNickname(Client: TmMServerClient): string;
var
  Info: TClientInfo;
begin
  Result := '';
  FClientLock.Enter;
  try
    if FClientInfo.TryGetValue(Client.Handle, Info) then
      Result := Info.Nickname;
  finally
    FClientLock.Leave;
  end;
end;

function TForm1.GetSelectedClient: TmMServerClient;
begin
  Result := nil;

  if ListView1.Selected <> nil then
  begin
    if Assigned(ListView1.Selected.Data) then
      Result := TmMServerClient(ListView1.Selected.Data);
  end;
end;

procedure TForm1.SendToAllClients(const Message: string = '');
var
  Client: TmMServerClient;
  UserMessage: string;
  Data: TBytes;
begin
  // If no message provided, ask user for input
  if Message = '' then
  begin
    if not InputQuery('Broadcast Message', 'Enter message to send to all clients:', UserMessage) then
      Exit; // User cancelled
    if UserMessage = '' then
      Exit; // Empty message
  end
  else
    UserMessage := Message;

  // Prepare data to send
  Data := BytesOf('ServerMessage|' + UserMessage);

  // Send to all connected clients
  for Client in mMServer1.Clients.Values do
  begin
    try
      Client.SendData(0, Data);
    except
      on E: Exception do
        AddLogMessage('Failed to send to client: ' + E.Message);
    end;
  end;

  // Log the broadcast
  AddLogMessage('Broadcasted: ' + UserMessage);
end;

procedure TForm1.SendToSelectedClient;
var
  Client: TmMServerClient;
  Message: string;
begin
  // Get the selected client
  Client := GetSelectedClient;
  if not Assigned(Client) then
  begin
    ShowMessage('Please select a client from the list');
    Exit;
  end;

  // Get message from user
  if InputQuery('Send Message', 'Enter message to send:', Message) then
  begin
    if Message <> '' then
    begin
      try
        Client.SendData(0, BytesOf('ServerMessage|' + Message));
        AddLogMessage('Sent message to ' + GetClientNickname(Client) + ': ' + Message);
      except
        on E: Exception do
          AddLogMessage('Failed to send message: ' + E.Message);
      end;
    end;
  end;
end;

procedure TForm1.DisconnectSelectedClient;
var
  Client: TmMServerClient;
  ClientName: string;
begin
  Client := GetSelectedClient;
  if Client = nil then
  begin
    ShowMessage('Please select a client from the list');
    Exit;
  end;

  if MessageDlg('Disconnect selected client?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Get client name before disconnecting
    ClientName := GetClientNickname(Client);
    if ClientName = '' then
      ClientName := Client.PeerIP;

    // Check if this client's screenshot is currently displayed and clear it BEFORE disconnecting
    if FCurrentScreenshotClient = Client then
    begin
      ClearScreenshot;
      AddLogMessage('Screenshot cleared - client manually disconnected');
    end;

    // Disconnect the client
    mMServer1.DisconnectClient(Client);

    // Log the disconnection
    AddLogMessage('Disconnected client: ' + ClientName);

    // Refresh UI
    RefreshClientList;
    UpdateStatusBar;
  end;
end;

// *****************************************************************************
// TCP Server Connected
// *****************************************************************************
procedure TForm1.mMServer1Connect(Sender: TObject; Client: TmMServerClient);
begin
  AddLogMessage('Client connected from ' + Client.PeerIP);
  AddClientToList(Client); // Add single client instead of rebuilding entire list
  UpdateStatusBar; // Update client count
end;

// *****************************************************************************
// TCP Server Disconnected
// *****************************************************************************
procedure TForm1.mMServer1Disconnect(Sender: TObject; Client: TmMServerClient);
var
  Nickname: string;
begin
  // Get nickname before cleanup
  Nickname := GetClientNickname(Client);

  // Check if this client's screenshot is currently displayed and clear it
  if FCurrentScreenshotClient = Client then
  begin
    ClearScreenshot;
    AddLogMessage('Screenshot cleared - client disconnected');
  end;

  // Remove from ListView first
  RemoveClientFromList(Client);

  // Clean up our app data
  FClientLock.Enter;
  try
    FClientInfo.Remove(Client.Handle);
  finally
    FClientLock.Leave;
  end;

  if Nickname <> '' then
    AddLogMessage('Client "' + Nickname + '" disconnected')
  else
    AddLogMessage('Client disconnected from ' + Client.PeerIP);

  UpdateStatusBar; // Update client count
end;

// *****************************************************************************
// Handle Command
// *****************************************************************************
function TForm1.mMServer1HandleCommand(Sender: TObject; Client: TmMServerClient;
  aCmd: Integer; const aData: TBytes): TBytes;
var
  sl: TStringList;
  Command,Message,NickName: string;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := '|';
    sl.StrictDelimiter := True;
    sl.DelimitedText := StringOf(aData);

    if sl.Count > 0 then
    begin
      Command := sl[0];

      if Command = 'NewCon' then
      begin
        if sl.Count > 1 then
        begin
          NickName := sl[1];
          SetClientNickname(Client, NickName);
          AddLogMessage('Client registered as: "' + NickName + '"');
          UpdateClientInList(Client); // Update just this client's display
        end;
      end

      else if Command = 'ClientMessage' then
      begin
        Message := sl[1];
        NickName := GetClientNickname(Client);
        AddLogMessage('[' + NickName + '] ' + Message);
      end;

      if sl[0] = 'ScreenShot' then
      begin
        TThread.Queue(nil,
          procedure
          var
            imageData: TBytes;
          begin
            try
              imageData := Copy(aData, 11, Length(aData));
              Form1.DisplayScreenShot(imageData, Client);
            except
              on E: Exception do
              begin
                //
              end;
            end;
          end);
      end

    end;
  finally
    sl.Free;
  end;
end;

// *****************************************************************************
// UI Events
// *****************************************************************************

procedure TForm1.btnActivateClick(Sender: TObject);
begin
  if mMServer1.Active then
  begin
    mMServer1.Active := False;
    btnActivate.Caption := 'Activate';
    AddLogMessage('TCP Server Deactivated');

    // Clear app data when server stops
    FClientLock.Enter;
    try
      FClientInfo.Clear;
    finally
      FClientLock.Leave;
    end;
    RefreshClientList;
  end
  else
  begin
    try

      // Configure TLS with OpenSSL
      //mMServer1.UseTLS := True;
      //mMServer1.TlsProvider := tpOpenSSL;  // Force OpenSSL (instead of Windows SChannel)
      //mMServer1.CertificateFile := ExtractFilePath(Application.ExeName) + 'server.pem';
      //mMServer1.PrivateKeyFile := ExtractFilePath(Application.ExeName) + 'server.key';
      //mMServer1.IgnoreCertificateErrors := True;

      // Configure TLS with  SChannel
      mMServer1.UseTLS := True;
      mMServer1.TlsProvider := tpSChannel;
      mMServer1.CertificateFile := ExtractFilePath(Application.ExeName) + 'server.pfx';
      mMServer1.PrivateKeyPassword := 'test';
      mMServer1.IgnoreCertificateErrors := True;

      mMServer1.Port := sedtPort.Value;
      mMServer1.Active := True;
      btnActivate.Caption := 'Deactivate';
      AddLogMessage('TCP Server Activated on port ' + IntToStr(mMServer1.Port));
      RefreshClientList;
    except
      on E: Exception do
        AddLogMessage('Failed to activate TCP Server: ' + E.Message);
    end;
  end;
end;

procedure TForm1.sedtPortChange(Sender: TObject);
begin
  try
    if not mMServer1.Active then
      mMServer1.Port := sedtPort.Value;
  except
    sedtPort.OnChange := nil;
    try
      sedtPort.Value := mMServer1.Port;
    finally
      sedtPort.OnChange := sedtPortChange;
    end;
    raise;
  end;
end;

procedure TForm1.S1Click(Sender: TObject);
begin
  SendToAllClients; // Will prompt user for message
end;

procedure TForm1.S2Click(Sender: TObject);
begin
  SendToSelectedClient;
end;

procedure TForm1.G1Click(Sender: TObject);
var
  Client: TmMServerClient;
  Info: string;
begin
  // Get the selected client
  Client := GetSelectedClient;
  if not Assigned(Client) then
  begin
    ShowMessage('Please select a client from the list');
    Exit;
  end;

  Info := Format('Client Information:'#13#10 +
                 'Nickname: %s'#13#10 +
                 'IP Address: %s'#13#10 +
                 'Port: %d'#13#10 +
                 'Connected At: %s'#13#10 +
                 'Connected For: %s'#13#10 +
                 'Handle: %d',
                 [GetClientNickname(Client),
                  Client.PeerIP,
                  Client.PeerPort,
                  FormatDateTime('yyyy-mm-dd hh:nn:ss', Client.ConnectedAt),
                  FormatDateTime('hh:nn:ss', Now - Client.ConnectedAt),
                  NativeInt(Client.Handle)]);
  ShowMessage(Info);
end;

procedure TForm1.G2Click(Sender: TObject);
var
  Client: TmMServerClient;
begin
  // Get the selected client
  Client := GetSelectedClient;
  if not Assigned(Client) then
  begin
    ShowMessage('Please select a client from the list');
    Exit;
  end;

    begin
      try
        Client.SendData(0, BytesOf('GetScreenShot|'));
      except
        on E: Exception do
          AddLogMessage('Failed to send message: ' + E.Message);
      end;
    end;
end;

procedure TForm1.D1Click(Sender: TObject);
begin
  DisconnectSelectedClient;
end;

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  LogMemo.Clear;
end;

// *****************************************************************************
// Memo Log
// *****************************************************************************
procedure TForm1.AddLogMessage(const Msg: string);
var
  TimeStr: string;
begin
  TimeStr := FormatDateTime('hh:nn:ss.zzz', Now);
  LogMemo.Lines.Add('[' + TimeStr + '] ' + Msg);

  // Auto-scroll to bottom
  LogMemo.SelStart := Length(LogMemo.Text);
  LogMemo.SelLength := 0;
  SendMessage(LogMemo.Handle, EM_SCROLLCARET, 0, 0);

  // Limit log size
  if LogMemo.Lines.Count > 1000 then
    LogMemo.Lines.Delete(0);

  Application.ProcessMessages;
end;

// *****************************************************************************
// Screenshot Management
// *****************************************************************************

procedure TForm1.DisplayScreenShot(const aData: TBytes; FromClient: TmMServerClient);
var
  MS: TMemoryStream;
  jpgImage: TJPEGImage;
begin
  // Create streams for JPEG loading
  MS := TMemoryStream.Create;
  jpgImage := TJPEGImage.Create;
  try
    // Load JPEG data directly into stream
    MS.WriteBuffer(aData[0], Length(aData));
    MS.Position := 0;

    // Load JPEG from data stream
    jpgImage.LoadFromStream(MS);

    // Display in TImage component
    if Assigned(Form1) and Assigned(Form1.Image1) then
    begin
      Form1.Image1.Picture.Assign(jpgImage);
      Form1.Image1.Stretch := True; // Auto-fit to component size
    end;

    // Update current screenshot client
    FCurrentScreenshotClient := FromClient;
  finally
    MS.Free;
    jpgImage.Free;
  end;
end;

procedure TForm1.ClearScreenshot;
begin
  // Clear the current screenshot
  if Assigned(Form1) and Assigned(Form1.Image1) then
    Form1.Image1.Picture.Assign(nil);

  // Clear the current screenshot client
  FCurrentScreenshotClient := nil;
end;

end.

