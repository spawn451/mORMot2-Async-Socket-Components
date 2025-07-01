unit mORMot2Register;

interface

procedure Register;

implementation

uses
  Classes, MmClient, MmServer;

// Include the resource file that contains component bitmaps
{$R mORMot2Components.res}

procedure Register;
begin
  // Register components
  RegisterComponents('mORMot2 Async Socket Components', [TmMClient, TmMServer]);
end;

end.
