unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, mMClient, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.Samples.Spin, Vcl.ExtCtrls, Vcl.Imaging.jpeg, System.ZLib;

type
  TForm1 = class(TForm)
    mMClient1: TmMClient;
    btnActivate: TButton;
    gb1: TGroupBox;
    edtHost: TEdit;
    sedtPort: TSpinEdit;
    gb2: TGroupBox;
    btnSend: TButton;
    edtText: TEdit;
    LogMemo: TMemo;
    btnClearLog: TButton;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mMClient1Connect(Sender: TObject);
    procedure mMClient1Disconnect(Sender: TObject);
    function mMClient1HandleCommand(Sender: TObject; aCmd: Integer; const aData: TBytes): TBytes;
    procedure AddLogMessage(const Msg: string);
    procedure btnActivateClick(Sender: TObject);
    procedure sedtPortChange(Sender: TObject);
    procedure edtHostChange(Sender: TObject);
    procedure edtTextEnter(Sender: TObject);
    procedure edtTextExit(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    // Auto-reconnect state management
    FAutoReconnect: Boolean;        // Auto-reconnect enabled
    FShouldBeConnected: Boolean;    // User wants to be connected
    FReconnectAttempts: Integer;    // Count attempts (for logging)
    FReconnectInterval: Integer;    // Seconds between attempts
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function CaptureScreenshot: TBytes;
var
  DC: HDC; // Device context for screen
  ScreenBMP: TBitmap; // Bitmap for screen capture
  jpgImage: TJPEGImage; // JPEG for compression
  MS: TMemoryStream; // Memory stream for processing
begin
  Result := nil;

  // Create required objects for screen capture
  ScreenBMP := TBitmap.Create;
  jpgImage := TJPEGImage.Create;
  MS := TMemoryStream.Create;
  try
    // Set bitmap dimensions to match screen resolution
    ScreenBMP.Width := Screen.Width;
    ScreenBMP.Height := Screen.Height;

    // Get device context for the entire screen (0 = desktop window)
    DC := GetDC(0);
    try
      // Copy screen pixels to bitmap using BitBlt
      // SRCCOPY = direct copy of source pixels
      BitBlt(ScreenBMP.Canvas.Handle, 0, 0, Screen.Width,
        Screen.Height, DC, 0, 0, SRCCOPY);
    finally
      // Always release the device context
      ReleaseDC(0, DC);
    end;

    // Convert to JPEG with 30% quality (already compressed)
    jpgImage.Assign(ScreenBMP);
    jpgImage.CompressionQuality := 30;
    jpgImage.SaveToStream(MS);

    // Return JPEG data directly (no additional compression needed)
    MS.Position := 0;
    SetLength(Result, MS.Size);
    if MS.Size > 0 then
      MS.ReadBuffer(Result[0], MS.Size);

  finally
    // Clean up resources
    ScreenBMP.Free;
    jpgImage.Free;
    MS.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize auto-reconnect settings
  FAutoReconnect := False;
  FShouldBeConnected := False;
  FReconnectAttempts := 0;
  FReconnectInterval := 5; // 5 seconds between attempts

  // Setup timer for auto-reconnect
  Timer1.Enabled := False;
  Timer1.Interval := FReconnectInterval * 1000; // Convert to milliseconds

  // Setup checkbox for auto-reconnect
  CheckBox1.Checked := FAutoReconnect;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Stop auto-reconnect
  FShouldBeConnected := False;
  Timer1.Enabled := False;

  if Assigned(mMClient1) then
  begin
    mMClient1.OnConnect := nil;
    mMClient1.OnDisconnect := nil;
    mMClient1.OnHandleCommand := nil;
  end;

  if Assigned(mMClient1) then
    mMClient1.Active := False;

  // Other cleanup...
end;

// *****************************************************************************
// Start/Stop Main Client
// *****************************************************************************
procedure TForm1.btnActivateClick(Sender: TObject);
begin
  if mMClient1.Active or FShouldBeConnected then
  begin
    // Deactivate the TCP client and stop auto-reconnect
    FShouldBeConnected := False;
    Timer1.Enabled := False;
    mMClient1.Active := False;
    btnActivate.Caption := 'Activate';
    FReconnectAttempts := 0;
    AddLogMessage('TCP Client Deactivated');
  end
  else
  begin
    // Check if the host field is blank
    if Trim(edtHost.Text) = '' then
    begin
      AddLogMessage('Host field cannot be blank.');
      Exit; // Exit the procedure if the host field is blank
    end;

    // Set user intention to be connected
    FShouldBeConnected := True;
    FReconnectAttempts := 0;

    // Configure TLS with OpenSSL
    //mMClient1.UseTLS := True;
    //mMClient1.TlsProvider := tpOpenSSL;
    //mMClient1.IgnoreCertificateErrors := True;  // For self-signed server cert

    mMClient1.UseTLS := True;
    mMClient1.TlsProvider := tpSChannel;
    mMClient1.IgnoreCertificateErrors := True;

    // Set the host from the text field
    mMClient1.Host := edtHost.Text;
    mMClient1.Port := sedtPort.Value;

    // Attempt to activate the TCP client
    try
      mMClient1.Active := True;
      AddLogMessage('Connection attempt started...');
    except
      on E: Exception do
      begin
        AddLogMessage('Failed to activate TCP Client: ' + E.Message);

        // If auto-reconnect is enabled, start trying immediately
        if FAutoReconnect then
        begin
          btnActivate.Caption := 'Stop Reconnect';
          AddLogMessage('Auto-reconnect enabled - attempting to reconnect...');
          Timer1.Enabled := True;
        end
        else
        begin
          FShouldBeConnected := False; // Reset if not auto-reconnecting
          btnActivate.Caption := 'Activate'; // Reset button if not auto-reconnecting
        end;
        Exit;
      end;
    end;

    // If auto-reconnect is enabled and we don't get OnConnect within a reasonable time, start auto-reconnect
    if FAutoReconnect then
    begin
      // Use a short timer to check if connection succeeded
      TThread.Queue(nil,
        procedure
        begin
          Sleep(1000); // Wait 1 second for connection

          // If still not connected after 1 second, assume connection failed
          if not (mMClient1.Active and Assigned(mMClient1.Connection)) then
          begin
            AddLogMessage('Connection timeout - starting auto-reconnect...');
            btnActivate.Caption := 'Stop Reconnect';
            Timer1.Enabled := True;
          end;
        end);
    end;
  end;
end;

// *****************************************************************************
// Change host (server)
// *****************************************************************************
procedure TForm1.edtHostChange(Sender: TObject);
begin
  try
    mMClient1.Host := edtHost.Text;
  except
    edtHost.OnChange := nil;
    try
      edtHost.Text := mMClient1.Host;
    finally
      edtHost.OnChange := edtHostChange;
    end;
    raise;
  end;
end;

// *****************************************************************************
// Change Main Client port
// *****************************************************************************
procedure TForm1.sedtPortChange(Sender: TObject);
begin
  try
    mMClient1.Host := edtHost.Text;
  except
    edtHost.OnChange := nil;
    try
      edtHost.Text := mMClient1.Host;
    finally
      edtHost.OnChange := edtHostChange;
    end;
    raise;
  end;
end;

// *****************************************************************************
// Text to send
// *****************************************************************************
procedure TForm1.edtTextEnter(Sender: TObject);
begin
  btnSend.Default := True;
end;

procedure TForm1.edtTextExit(Sender: TObject);
begin
btnSend.Default := False;
end;

procedure TForm1.btnSendClick(Sender: TObject);
begin
  // Check if the data field is blank
  if Trim(edtText.Text) = '' then
  begin
    AddLogMessage('Cannot send - Data field cannot be blank.');
    Exit; // Exit the procedure if the data field is blank
  end;

  try
    // Send the text data
    mMClient1.SendData(0, BytesOf('ClientMessage|' + edtText.Text));

    AddLogMessage('Data sent: ' + edtText.Text);
  except
    on E: Exception do
      AddLogMessage('Failed to send data: ' + E.Message);
  end;
end;


// *****************************************************************************
// TCP Client Connected
// *****************************************************************************
procedure TForm1.mMClient1Connect(Sender: TObject);
var
  NickName: String;
begin
  // Stop auto-reconnect timer - we're connected!
  Timer1.Enabled := False;

  btnActivate.Caption := 'Deactivate';

  // Generate nickname and send identification
  NickName := 'Client' + IntToStr(GetTickCount64);

  // Send client identification using protocol message
  mMClient1.SendData(0, BytesOf('NewCon|' + NickName));

  // Log successful connection
  if FReconnectAttempts > 0 then
    AddLogMessage(Format('Reconnected to %s:%d as %s after %d attempts',[mMClient1.Host, mMClient1.Port, NickName, FReconnectAttempts]))
  else
    AddLogMessage(Format('Connected to %s:%d as %s',[mMClient1.Host, mMClient1.Port, NickName]));

  // Reset reconnect counter
  FReconnectAttempts := 0;

  //mMClient1.SendData(1, BytesOf('WAAAAAZA'));
  mMClient1.SendData(1, BytesOf('Welcome to server')); //--> Incompatible type Integer and procedure, untyped pointer or untyped parameter

end;

// *****************************************************************************
// TCP Client Disconnected
// *****************************************************************************
procedure TForm1.mMClient1Disconnect(Sender: TObject);
begin
  AddLogMessage('Disconnected');

  // IMPORTANT: Force the Active property to False since we're disconnected
  // This ensures our timer logic works correctly
  TThread.Queue(nil,
    procedure
    begin
      // Start auto-reconnect if enabled and user still wants to be connected
      if FShouldBeConnected and FAutoReconnect then
      begin
              btnActivate.Caption := 'Stop Reconnect';  // Show user can stop the reconnection
      AddLogMessage('Auto-reconnect enabled - attempting to reconnect...');
      Timer1.Enabled := True;
      end
      else
      begin
        btnActivate.Caption := 'Activate';  // Normal disconnected state
      end;
    end);
end;

// *****************************************************************************
// Handle Command
// *****************************************************************************
function TForm1.mMClient1HandleCommand(Sender: TObject; aCmd: Integer; const aData: TBytes): TBytes;
var
  sl: TStringList;
  Command,Message: String;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := '|';
    sl.StrictDelimiter := True;
    sl.DelimitedText := StringOf(aData);

    if sl.Count > 0 then
    begin
    Command := sl[0];

    if Command = 'ServerMessage' then
    begin
      Message := sl[1];
      AddLogMessage('[SERVER]'+ Message);
    end;

    if Command = 'GetScreenShot' then
      begin
        TThread.Queue(nil,
          procedure
          var
            ScreenshotData: TBytes;
          begin
            // Capture screenshot
            ScreenshotData := CaptureScreenshot;

            if Length(ScreenshotData) > 0 then
            begin
            // Send compressed image to server

            mMClient1.SendData(0, BytesOf('ScreenShot|') + ScreenshotData);
            end;
          end);
      end

     //...
    end;

  finally
    sl.Free;
  end;
end;

procedure TForm1.btnClearLogClick(Sender: TObject);
begin
  LogMemo.Clear;
end;

// *****************************************************************************
// Auto-Reconnect Timer
// *****************************************************************************
procedure TForm1.Timer1Timer(Sender: TObject);
var
  IsConnected: Boolean;
begin
  // Check if we're really connected by looking at the connection object
  IsConnected := mMClient1.Active and Assigned(mMClient1.Connection);

  // Only attempt reconnect if we should be connected, we're not really connected, and auto-reconnect is enabled
  if FShouldBeConnected and not IsConnected and FAutoReconnect then
  begin
    try
      Inc(FReconnectAttempts);
      AddLogMessage('Reconnect attempt #' + IntToStr(FReconnectAttempts) + '...');

      // Update button to show we're trying
      btnActivate.Caption := 'Stop Reconnect';

      // Ensure we're disconnected first
      if mMClient1.Active then
        mMClient1.Active := False;

      // Set connection parameters
      mMClient1.Host := edtHost.Text;
      mMClient1.Port := sedtPort.Value;

      // Attempt to connect - mORMot2 handles all socket management
      mMClient1.Active := True;

      // If we get here, connection succeeded
      // Timer will be stopped in OnConnect event

    except
      on E: Exception do
      begin
        AddLogMessage('Reconnect attempt #' + IntToStr(FReconnectAttempts) + ' failed: ' + E.Message);
        // Timer continues running for next attempt
        btnActivate.Caption := 'Stop Reconnect';  // Keep showing reconnect state
      end;
    end;
  end
  else
  begin
    // Stop timer if conditions are no longer met
    Timer1.Enabled := False;
    if not IsConnected then
      btnActivate.Caption := 'Activate';
  end;
end;

// *****************************************************************************
// Auto-Reconnect Enable/Disable
// *****************************************************************************
procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  FAutoReconnect := CheckBox1.Checked;

  if FAutoReconnect then
  begin
    AddLogMessage('Auto-reconnect enabled');

    // If we should be connected but aren't, start trying
    if FShouldBeConnected and not mMClient1.Active then
    begin
      AddLogMessage('Starting auto-reconnect attempts...');
      Timer1.Enabled := True;
    end;
  end
  else
  begin
    AddLogMessage('Auto-reconnect disabled');
    Timer1.Enabled := False;
  end;
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

  Application.ProcessMessages;
end;

end.


