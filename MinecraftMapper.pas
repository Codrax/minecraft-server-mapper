{
  Minecraft Server Port Mapper
  Copyright 2024 Petculescu Codrut

  Made with Indy Internet Direct(c)
  All rights reserved
}

unit MinecraftMapper;

interface

uses
  Classes,
  IdAssignedNumbers,
  IdContext,
  IdCustomTCPServer,   Cod.Types,
  Cod.Console, System.RegularExpressions,
  IdGlobal, IdStack, IdTCPConnection, IdTCPServer, IdYarn, SysUtils;

type
  TMinecraftPortTCP = class;

  // Record
  TMinecraftMapping = record
    Address: string; // The adress to be mapped
    Host: string; // The host to redirect data packets to
    Port: word; // The port used
    Expression: string;

    function IsEqual(ATo: TMinecraftMapping) : boolean;
  end;
  TMinecraftMappings = array of TMinecraftMapping;

  // Context

  { TMinecraftPortContext }

  TMinecraftPortContext = class(TIdServerContext)
  private
    function GetConnectionTimeout: Integer;
  protected
    FOutboundClient: TIdTCPConnection;//was TIdTCPClient
    FReadList: TIdSocketList;
    FDataAvailList: TIdSocketList;
    FNetData: TIdBytes;
    FServerMc : TMinecraftPortTCP;
    FOutboundConnected: boolean;

    FAddress: string;
    FHost: string; // found after the Handshake has been established
    FPort: word;
    FUsesDefault: boolean; // uses the default mappint
    //
    procedure ReadHandshakeData;
    //
    procedure CheckForData(DoRead: Boolean); virtual;
    procedure HandleLocalClientData; virtual;
    procedure HandleOutboundClientData; virtual;
    procedure OutboundConnect; virtual;
    procedure InitiateClient;
  public
    constructor Create(
      AConnection: TIdTCPConnection;
      AYarn: TIdYarn;
      AList: TIdContextThreadList = nil
      ); override;
    destructor Destroy; override;
    //
    property Address: string read FAddress;
    property Host: string read FHost;
    property Port: word read FPort;
    property UsesDefault: boolean read FUsesDefault;
    //
    property  Server : TMinecraftPortTCP Read FServerMc write FServerMc;
    property  ConnectTimeOut: Integer read GetConnectionTimeout;
    property  NetData: TIdBytes read FNetData write FNetData;
    property  OutboundClient: TIdTCPConnection read FOutboundClient write FOutboundClient;
  end;//TMinecraftPortContext
  TMinecraftPortList = array of TMinecraftPortContext;

  { TMinecraftPortTCP }

  TMinecraftPortTCP = class(TIdCustomTCPServer)
  private
    procedure DisconnectDefault;

    function GetConnectionCount: integer;
    procedure SetDefaultAllow(AValue: boolean);
  protected
    FOnBeforeConnect: TIdServerThreadEvent;

    // Mappings
    FAllowDefault: boolean;
    FDefaultMapping: TMinecraftMapping;
    FMappings: TMinecraftMappings;

    // Connections
    FActiveConnections: TMinecraftPortList;
    FConnectionTimeout: integer;

    //AThread.Connection.Server & AThread.OutboundClient
    FOnOutboundConnect,
    FOnOutboundData,
    FOnOutboundDisConnect,

    FOnMap,
    FOnReject,
    FOnFailSignature,
    FOnFailOutboundConnect: TIdServerThreadEvent;
    //
    function HasAddresBinding(var AAddress: string; out AHost: string;
      out APort: word; out Default: boolean): boolean;

    procedure DoMap(AContext: TIdContext);
    procedure DoReject(AContext: TIdContext);
    procedure DoFailSignature(AContext: TIdContext);
    procedure DoFailOutboundConnect(AContext: TIdContext);
    //
    procedure ContextCreated(AContext:TIdContext); override;
    procedure DoBeforeConnect(AContext: TIdContext); virtual;
    procedure DoConnect(AContext: TIdContext); override;
    function  DoExecute(AContext: TIdContext): boolean; override;
    procedure DoDisconnect(AContext: TIdContext); override; //DoLocalClientDisconnect
    procedure DoLocalClientConnect(AContext: TIdContext); virtual;
    procedure DoLocalClientData(AContext: TIdContext); virtual;//APR: bServer

    procedure DoOutboundClientConnect(AContext: TIdContext); virtual;
    procedure DoOutboundClientData(AContext: TIdContext); virtual;
    procedure DoOutboundDisconnect(AContext: TIdContext); virtual;
    function  GetOnConnect: TIdServerThreadEvent;
    function  GetOnExecute: TIdServerThreadEvent;
    procedure SetOnConnect(const Value: TIdServerThreadEvent);
    procedure SetOnExecute(const Value: TIdServerThreadEvent);
    function  GetOnDisconnect: TIdServerThreadEvent;
    procedure SetOnDisconnect(const Value: TIdServerThreadEvent);
    procedure InitComponent; override;
  public
    // Proc
    procedure AddMapping(AAddress, AHost: string; APort: word); overload;
    procedure AddMapping(Mapping: TMinecraftMapping); overload;
    procedure RemoveMapping(Index: integer; Disconnect: boolean = true);
    procedure SetDefaultMapping(ANewMapping: TMinecraftMapping; Disconnect: boolean = true);

    // Notify
    property  OnBeforeConnect: TIdServerThreadEvent read FOnBeforeConnect write FOnBeforeConnect;

    property  OnMap: TIdServerThreadEvent read FOnMap write FOnMap;
    property  OnReject: TIdServerThreadEvent read FOnReject write FOnReject;
    property  OnFailSignature: TIdServerThreadEvent read FOnFailSignature write FOnFailSignature;
    property  OnFailOutboundConnect: TIdServerThreadEvent read FOnFailOutboundConnect write FOnFailOutboundConnect;

    property  OnConnect: TIdServerThreadEvent read GetOnConnect write SetOnConnect; //OnLocalClientConnect
    property  OnOutboundConnect: TIdServerThreadEvent read FOnOutboundConnect write FOnOutboundConnect;

    property  OnExecute: TIdServerThreadEvent read GetOnExecute write SetOnExecute;//OnLocalClientData
    property  OnOutboundData: TIdServerThreadEvent read FOnOutboundData write FOnOutboundData;

    property  OnDisconnect: TIdServerThreadEvent read GetOnDisconnect write SetOnDisconnect;//OnLocalClientDisconnect
    property  OnOutboundDisconnect: TIdServerThreadEvent read FOnOutboundDisconnect write FOnOutboundDisconnect;

    // Mappings
    property DefaultAllow: boolean read FAllowDefault write SetDefaultAllow;
    property DefaultMapping: TMinecraftMapping read FDefaultMapping write FDefaultMapping;
    property Mappings: TMinecraftMappings read FMappings write FMappings;

    property ConnectionTimeout: integer read FConnectionTimeout write FConnectionTimeout;

    property ActiveConnections: TMinecraftPortList read FActiveConnections write FActiveConnections;
    property ConnectionCount: integer read GetConnectionCount;
  end;//TIdMappedPortTCP

  // Utils
  function MappingEqual(Map1, Map2: TMinecraftMapping): boolean;
  function MatchExpression(const Expression, InputString: string): string;

Implementation

uses
  IdException,
  IdIOHandler, IdIOHandlerSocket, IdResourceStrings,IdStackConsts, IdTCPClient;

function MatchExpression(const Expression, InputString: string): string;
var
  Dat: string;
begin
  if Expression = '' then
    Exit(InputString);

  Result := InputString;
  const Sets = Expression.Split(['|'], TStringSplitOptions.ExcludeEmpty);

  for var I := 0 to High(Sets) do
    begin
      Dat := Sets[I].Remove(0, 1);
      case Sets[I][1] of
        // Delete element following
        'd': Result := StringReplace(Result, Dat, '', [TReplaceFlag.rfReplaceAll]);

        // Replace element following
        'r': begin
          const R = Dat.Split([':']);
          Result := StringReplace(Result, R[0], R[1], [TReplaceFlag.rfReplaceAll]);
        end;

        // Delete from end
        '<': Result := Result.Substring(0, Length(Result)-Dat.ToInteger);

        // Delete from start
        '>': Result := Result.Substring(Length(Result)-Dat.ToInteger, Length(Result)-Dat.ToInteger);
      end;
    end;

  Result := Lowercase(Trim(Result));
end;

function MappingEqual(Map1, Map2: TMinecraftMapping): boolean;
begin
  Result := (Map1.Address = Map2.Address) and (Map1.Expression = Map2.Expression) and (Map1.Host = Map2.Host) and (Map1.Port = Map2.Port);
end;

procedure TMinecraftPortTCP.InitComponent;
begin
  inherited InitComponent;
  FContextClass := TMinecraftPortContext;
  SetDefaultPort(25565);
  ConnectionTimeout := -1;
end;

procedure TMinecraftPortTCP.RemoveMapping(Index: integer; Disconnect: boolean);
var
  I: Integer;
begin
  // like before, both should be lowercase

  if Disconnect then
    for I := 0 to High(FActiveConnections) do
      if FActiveConnections[I].Address = Mappings[Index].Address then
        FActiveConnections[I].Connection.Disconnect;

  // Remove
  for I := Index to High(FMappings)-1 do
    FMappings[I] := FMappings[I+1];

  SetLength(FMappings, High(FMappings));
end;

procedure TMinecraftPortTCP.AddMapping(AAddress, AHost: string; APort: word);
var
  Map: TMinecraftMapping;
begin
  with Map do
    begin
      Address := AAddress;
      Host := AHost;
      Port := APort;
    end;

  AddMapping(Map);
end;

procedure TMinecraftPortTCP.AddMapping(Mapping: TMinecraftMapping);
var
  Index: integer;
  I: Integer;
begin
  Mapping.Address := LowerCase(Mapping.Address); // make lowercase

  // Check for duplicates
  for I := 0 to High(Mappings) do
    if Mappings[I].Address = Mapping.Address then
      Exit;

  Index := Length(FMappings);
  SetLength(FMappings, Index+1);

  FMappings[Index] := Mapping;
end;

procedure TMinecraftPortTCP.ContextCreated(AContext: TIdContext);
begin
  TMinecraftPortContext(AContext).Server := Self;
end;

procedure TMinecraftPortTCP.DoBeforeConnect(AContext: TIdContext);
begin
  if Assigned(FOnBeforeConnect) then begin
    FOnBeforeConnect(AContext);
  end;
end;

procedure TMinecraftPortTCP.DoLocalClientConnect(AContext: TIdContext);
begin
  if Assigned(FOnConnect) then begin
    FOnConnect(AContext);
  end;
end;

procedure TMinecraftPortTCP.DoOutboundClientConnect(AContext: TIdContext);
begin
  if Assigned(FOnOutboundConnect) then begin
    FOnOutboundConnect(AContext);
  end;
end;

procedure TMinecraftPortTCP.DoLocalClientData(AContext: TIdContext);
begin
  if Assigned(FOnExecute) then begin
    FOnExecute(AContext);
  end;
end;

procedure TMinecraftPortTCP.DoMap(AContext: TIdContext);
begin
  if Assigned(FOnMap) then begin
    FOnMap(AContext);
  end;
end;

procedure TMinecraftPortTCP.DoOutboundClientData(AContext: TIdContext);
begin
  if Assigned(FOnOutboundData) then begin
    FOnOutboundData(AContext);
  end;
end;

procedure TMinecraftPortTCP.DoDisconnect(AContext: TIdContext);
var
  Index, I: integer;
begin
  inherited DoDisconnect(AContext);
  //check for loop
  if Assigned(TMinecraftPortContext(AContext).FOutboundClient) and
    TMinecraftPortContext(AContext).FOutboundClient.Connected then
  begin
    TMinecraftPortContext(AContext).FOutboundClient.Disconnect;
  end;

  // Remove connection
  Index := -1;
  for I := 0 to High(FActiveConnections) do
    if FActiveConnections[I] = AContext then
      begin
        Index := I;
        Break;
      end;
  if Index = -1 then
    Exit;
  for I := Index to High(FActiveConnections)-1 do
    FActiveConnections[I] := FActiveConnections[I+1];

  SetLength(FActiveConnections, High(FActiveConnections));
end;

procedure TMinecraftPortTCP.DoOutboundDisconnect(AContext: TIdContext);
begin
  if Assigned(FOnOutboundDisconnect) then begin
    FOnOutboundDisconnect(AContext);
  end;
  AContext.Connection.Disconnect; //disconnect local
end;

procedure TMinecraftPortTCP.DoReject(AContext: TIdContext);
begin
  if Assigned(FOnReject) then begin
    FOnReject(AContext);
  end;
end;

procedure TMinecraftPortTCP.DoConnect(AContext: TIdContext);
var
  LContext: TMinecraftPortContext;
  Index: integer;
begin
  DoBeforeConnect(AContext);

  LContext := TMinecraftPortContext(AContext);

  //WARNING: Check TIdTCPServer.DoConnect and synchronize code. Don't call inherited!=> OnConnect in OutboundConnect    {Do not Localize}
  //LContext.OutboundConnect;
  LContext.InitiateClient;

  //cache
  LContext.FReadList.Clear;
  LContext.FReadList.Add(AContext.Binding.Handle);

  // Add connection
  Index := Length(FActiveConnections);
  SetLength(FActiveConnections, Index+1);
  FActiveConnections[Index] := TMinecraftPortContext(AContext);
end;

function TMinecraftPortTCP.DoExecute(AContext: TIdContext): boolean;
var
  LContext: TMinecraftPortContext;
begin
  LContext := TMinecraftPortContext(AContext);
  try
    LContext.CheckForData(True);
  finally
    if LContext.FOutboundConnected and not LContext.FOutboundClient.Connected then begin
      Result := False;
      DoOutboundDisconnect(AContext); //&Connection.Disconnect
    end else begin;
      Result := AContext.Connection.Connected;
    end;
  end;
end;

procedure TMinecraftPortTCP.DoFailOutboundConnect(AContext: TIdContext);
begin
  if Assigned(FOnFailOutboundConnect) then begin
    FOnFailOutboundConnect(AContext);
  end;
end;

procedure TMinecraftPortTCP.DoFailSignature(AContext: TIdContext);
begin
  if Assigned(FOnFailSignature) then begin
    FOnFailSignature(AContext);
  end;
end;

function TMinecraftPortTCP.GetOnConnect: TIdServerThreadEvent;
begin
  Result := FOnConnect;
end;

function TMinecraftPortTCP.GetOnExecute: TIdServerThreadEvent;
begin
  Result := FOnExecute;
end;

procedure TMinecraftPortTCP.DisconnectDefault;
var
  I: integer;
begin
  for I := 0 to High(FActiveConnections) do
    if FActiveConnections[I].UsesDefault then
      FActiveConnections[I].Connection.Disconnect;
end;

function TMinecraftPortTCP.GetConnectionCount: integer;
begin
  Result := Length(FActiveConnections);
end;

procedure TMinecraftPortTCP.SetDefaultAllow(AValue: boolean);
begin
  if FAllowDefault=AValue then Exit;

  // Disconnect
  if not AValue then
    DisconnectDefault;

  // Set
  FAllowDefault:=AValue;
end;

function TMinecraftPortTCP.HasAddresBinding(var AAddress: string; out AHost: string;
  out APort: word; out Default: boolean): boolean;
var
  I: integer;
  AddressMatch: string;
begin
  Result := false;
  Default := false;

  // at this moment, both the Server and Context should have their respective address lowercase
  AAddress := Trim(AAddress);

  // Mappings
  for I := 0 to High(Mappings) do
    begin
      AddressMatch := MatchExpression(Mappings[I].Expression, AAddress);

      if AddressMatch = Mappings[I].Address then
        begin
          AAddress := AddressMatch;
          AHost := Mappings[I].Host;
          APort := Mappings[I].Port;

          Exit(true);
        end;
    end;


  // Default
  if FAllowDefault then
    begin
      AHost := DefaultMapping.Host;
      APort := DefaultMapping.Port;

      Default := true;
      Result := true;
    end;
end;

function TMinecraftPortTCP.GetOnDisconnect: TIdServerThreadEvent;
begin
  Result := FOnDisconnect;
end;

procedure TMinecraftPortTCP.SetDefaultMapping(ANewMapping: TMinecraftMapping; Disconnect: boolean);
begin
  // Disconnect
  if Disconnect then
    DisconnectDefault;

  FDefaultMapping := ANewMapping;
end;

procedure TMinecraftPortTCP.SetOnConnect(const Value: TIdServerThreadEvent);
begin
  FOnConnect := Value;
end;

procedure TMinecraftPortTCP.SetOnExecute(const Value: TIdServerThreadEvent);
begin
  FOnExecute := Value;
end;

procedure TMinecraftPortTCP.SetOnDisconnect(const Value: TIdServerThreadEvent);
begin
  FOnDisconnect := Value;
end;


{ TIdMappedPortContext }

constructor TMinecraftPortContext.Create(
  AConnection: TIdTCPConnection;
  AYarn: TIdYarn;
  AList: TIdContextThreadList = nil
  );
begin
  inherited Create(AConnection, AYarn, AList);
  FReadList := TIdSocketList.CreateSocketList;
  FDataAvailList := TIdSocketList.CreateSocketList;
end;

destructor TMinecraftPortContext.Destroy;
begin
  FreeAndNil(FOutboundClient);
  FreeAndNIL(FReadList);
  FreeAndNIL(FDataAvailList);
  inherited Destroy;
end;

procedure TMinecraftPortContext.CheckForData(DoRead: Boolean);
begin
  if DoRead and Connection.IOHandler.InputBufferIsEmpty and (not FOutboundConnected or FOutboundClient.IOHandler.InputBufferIsEmpty) then
  begin
    if FReadList.SelectReadList(FDataAvailList, IdTimeoutInfinite) then
    begin
      //1.LConnectionHandle
      if FDataAvailList.ContainsSocket(Connection.Socket.Binding.Handle) then
      begin
        Connection.IOHandler.CheckForDataOnSource(0);
      end;
      //2.LOutBoundHandle
      if FOutboundConnected and FDataAvailList.ContainsSocket(FOutboundClient.Socket.Binding.Handle) then
      begin
        FOutboundClient.IOHandler.CheckForDataOnSource(0);
      end;
    end;
  end;
  if not Connection.IOHandler.InputBufferIsEmpty then
  begin
    HandleLocalClientData;
  end;
  if FOutboundConnected and not FOutboundClient.IOHandler.InputBufferIsEmpty then
  begin
    HandleOutboundClientData;
  end;
  Connection.IOHandler.CheckForDisconnect;
  if FOutboundConnected then
    FOutboundClient.IOHandler.CheckForDisconnect;
end;

procedure TMinecraftPortContext.HandleLocalClientData;
begin
  SetLength(FNetData, 0);
  Connection.IOHandler.InputBuffer.ExtractToBytes(FNetData);
  Server.DoLocalClientData(Self);

  // Create outbound connection
  if not FOutboundConnected then
    begin
      ReadHandshakeData;

      // Connect
      OutboundConnect;
    end;

  // Write to outbound
  FOutboundClient.IOHandler.Write(FNetData);
end;

procedure TMinecraftPortContext.HandleOutboundClientData;
begin
  SetLength(FNetData, 0);
  FOutboundClient.IOHandler.InputBuffer.ExtractToBytes(FNetData);
  Server.DoOutboundClientData(Self);
  Connection.IOHandler.Write(FNetData);
end;

procedure TMinecraftPortContext.InitiateClient;
begin
  CheckForData(False);
end;

procedure TMinecraftPortContext.OutboundConnect;
var
  LServer: TMinecraftPortTCP;
  LClient: TIdTCPClient;
begin
  FOutboundClient := TIdTCPClient.Create(nil);
  LServer := TMinecraftPortTCP(Server);
  try
    LClient := TIdTCPClient(FOutboundClient);

    LClient.Port := FPort;
    LClient.Host := FHost;

    LServer.DoLocalClientConnect(Self);

    LClient.ConnectTimeout := ConnectTimeOut;
    LClient.Connect;

    LServer.DoOutboundClientConnect(Self);

    // Success
    FOutboundConnected := true;

    //APR: buffer can contain data from prev (users) read op.
    CheckForData(False);
  except
    on E: Exception do
    begin
      Server.DoFailOutboundConnect(Self);

      DoException(E);
      Connection.Disconnect; //req IdTcpServer with "Stop this thread if we were disconnected"
      raise;
    end;
  end;

  // cache
  FReadList.Add(FOutboundClient.Socket.Binding.Handle);
end;

function TMinecraftPortContext.GetConnectionTimeout: Integer;
begin
  Result := Server.ConnectionTimeout;
end;

procedure TMinecraftPortContext.ReadHandshakeData;
function ExtractServerAddress(AddressStart: integer): boolean;
var
  AddressLength: integer;
begin
  Result := false;
  // Validate
  if AddressStart >= Length(FNetData) then
    Exit;

  // Length
  AddressLength := FNetData[AddressStart];

  // Validate
  if AddressStart+AddressLength >= Length(FNetData) then
    Exit;

  // Extract
  for var I := AddressStart+1 to AddressStart+AddressLength do
    FAddress := FAddress + Char(FNetData[I]);

  WriteLn(FAddress);

  // Format
  FAddress := LowerCase(Trim(FAddress)); // make lowercase
  Result := true;
end;
begin
  // Parse handshake
  try
    // For above 1.8.9
    if not ExtractServerAddress(4) then
    // For 1.8.9 and earlier
    if not ExtractServerAddress(3) then
      // Fail
      raise Exception.Create('Invalid data.');
  except
    Server.DoFailSignature(Self);
    Connection.Disconnect;
    raise Exception.Create('Could not read server handshake.');
  end;

  // Check mapping
  if not Server.HasAddresBinding(FAddress, FHost, FPort, FUsesDefault) then
    begin
      Server.DoReject(Self);
      Connection.Disconnect;
      raise Exception.Create('No mapping for adress.');
    end;

  // Approve
  Server.DoMap(Self);
end;

{ TMinecraftMapping }

function TMinecraftMapping.IsEqual(ATo: TMinecraftMapping): boolean;
begin
  Result := (Address = ATo.Address) and (Expression = ATo.Expression) and (Port = ATo.Port) and (Host = ATo.Host);
end;

end.
