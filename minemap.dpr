program minemap;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, Classes, Cod.SysUtils, IOUtils, Types,
  Cod.ArrayHelpers, Math, JSON, Cod.StringUtils, Cod.Console,
  System.Generics.Collections, Cod.MesssageConst,
  System.RegularExpressions,

  // Mapper
  MinecraftMapper,

  // Indy
  IdHTTP, IdSocketHandle, IdStack, IdContext, IdTCPClient, IdMappedPortTCP,
  IdCustomTCPServer;

type
  TErrorException = class(Exception);

  TMapperManager = class(TObject)
  strict private
    // Data
    /// <summary>
    /// This function loads ALL servers into a TServer array
    /// </summary>
    function LoadMappings: TMinecraftMappings;
    function LoadDefaultMapping: TMinecraftMapping;

    function ObjectToMapping(JSON: TJSONObject): TMinecraftMapping;

    // Notification events
    procedure OnBeforeConnect(AContext: TIdContext);
    procedure OnMap(AContext: TIdContext);
    procedure OnDisconnect(AContext: TIdContext);
    procedure OnReject(AContext: TIdContext);
    procedure OnFailSignature(AContext: TIdContext);
    procedure OnFailOutboundConnect(AContext: TIdContext);

  public
    Mapper: TMinecraftPortTCP;

    //  Proc
    /// <summary> Load new configuration </summary>
    procedure LoadConfigurations;
    /// <summary>
    /// This function checks for new/deleted mappings and adds /
    /// removes them from index. And disconnects removed mappings.
    /// </summary>
    procedure ProcessMappingList;
    /// <summary> Check for changes in default mapping </summary>
    procedure ProcessDefaultMapping;

    // Constructors
    constructor Create;
    destructor Destroy; override;
  end;


  TSystemThread = class(TThread)
  protected
    LastModConfig,
    LastModDefaultMapping,
    LastModMappings: TDateTime;

    NowModConfig,
    NowModDefaultMapping,
    NowModMappings: TDateTime;

    procedure LoadModificationDates;
    procedure WriteModificationDates;

    procedure Execute; override;
  public
    MonitorPaused: boolean;

    // Constructors
    constructor Create;
  end;

const
  // Data
  UPDATE_DELAY = 1000;
  LINE_SEPARATOR = '----------------';
  VERSION = '1.0.2';

var
  Thread: TSystemThread = nil;

  RUN_DIRECTORY: string = '';
  LOG_FILE: string = 'logfile.log';

  // Settings
  UseScreenEcho,
  UseLogFile: boolean;
  UseServiceMode: boolean;

  // Manager
  Manager: TMapperManager;

  // Monitor
  MonitorDelay: cardinal = UPDATE_DELAY;

  // Files
  ConfigFile: string = 'config.json';
  DefaultMappingFile: string = 'mapping-default.json';
  MappingsFile: string = 'mappings.json';

  // Util
  ReadIOInput: char;

function LoadJSON(FilePath: string): TJSONValue;
begin
  Result := nil;
  if not TFile.Exists(FilePath) then
    Exit;

  try
    Result := TJSONObject.ParseJSONValue( TFile.ReadAllText(FilePath) );
  except
    Result := nil;
  end;
end;

procedure WriteJSON(FilePath: string; JSON: TJSONValue);
var
  Data: string;
begin
  // Format
  Data := JSON.Format(4);

  // Write
  const Stream = TStringStream.Create(Data);
  Stream.SaveToFile(FilePath);
  //TFile.WriteAllText(FilePath, Data, TEncoding.UTF8);
end;

procedure WriteLog(AString: string; Echo: boolean=true);
var
  AFile: TextFile;
begin
  const Log = Format('[%S] %S', [TimeToStr(Now), AString]);
  if UseScreenEcho and Echo then
    begin
      TConsole.Write := Log+#$A;
      TConsole.GoLineStart;
    end;

  if UseLogFile then
    begin
      AssignFile(AFile, LOG_FILE);
      if TFile.Exists(LOG_FILE) then
        Append(AFile)
      else
        Rewrite(AFile);

      WriteLn(AFile, Log);
      CloseFile(AFile);
    end;
end;

procedure WriteError(AString: string);
begin
  WriteLog('ERROR: '+AString, false);

  if UseScreenEcho then
    begin
      TConsole.BackgroundColor := TConsoleColor.Red;
      TConsole.Write := 'ERROR';
      TConsole.ResetStyle;
      TConsole.Write := ' '+AString+#$A;
      TConsole.GoLineStart;
    end;
end;

function SizeInString(Size: int64; MaxDecimals: cardinal): string;
var
  Decim: integer;
  DivValue: integer;
begin
  Decim := Trunc( Power( 10, MaxDecimals ) );

  // Get Div Value
  case Abs( size ) of
    0..1023: DivValue := 0; // B
    1024..1048575: DivValue := 1; // KB
    1048576..1073741823: DivValue := 2; // MB
    else DivValue := 3;
  end;

  // Div
  Result := FloatToStr( Trunc(Size / Power( 1024, DivValue) * Decim ) / Decim ) ;

  // Measurement
  case DivValue of
    0: Result := Concat( Result, ' ', 'B' );
    1: Result := Concat( Result, ' ', 'KB' );
    2: Result := Concat( Result, ' ', 'MB' );
    3: Result := Concat( Result, ' ', 'GB' );
  end;
end;

function StrMemoryToMB(Str: string): cardinal;
begin
  try
    if Str.IndexOf('g') <> -1 then
      begin
        Result := round(strtofloat(Str.Replace('g', '').Replace(',', '.'))*1024);
      end
    else
      Result := strtoint(Str);
  except
    Result := 0;
  end;
end;

{ TSystemThread }
constructor TSystemThread.Create;
begin
  inherited Create(false); // create running
  MonitorPaused := true;
end;

procedure TSystemThread.Execute;
begin
  inherited;

  // Proc
  while not Terminated do
    begin
      // Wait for local sync processing time
      while ReadIOInput <> '' do
        Sleep(1);

      // Paused
      if MonitorPaused then
        begin
          Sleep( UPDATE_DELAY );
          Continue;
        end;

      try
        // Load dates
        LoadModificationDates;

        // Analise changes
        if LastModConfig <> NowModConfig then
          begin
            WriteLog('The config file has changed. Loading updated version...');
            try
              Manager.LoadConfigurations;
            except
              WriteError('An error occured loading the configuration file. The previous configuration will be kept');
            end;
          end;

        if LastModDefaultMapping <> NowModDefaultMapping then
          begin
            WriteLog('The default mapping has changed. Loading updated version...');
            try
              Manager.ProcessDefaultMapping;
            except
              WriteError('An error occured loading the default mapping file. The previous configuration will be kept');
            end;
          end;

        if LastModMappings <> NowModMappings then
          begin
            WriteLog('The mappings file has changed. Loading updated version...');
            try
              Manager.ProcessMappingList;
            except
              WriteError('An error occured loading the mappings file. The previous configuration will be kept');
            end;
          end;

        // Reset dates
        WriteModificationDates;

      except
        on E: Exception do
          begin
            WriteLog('An exception occured in the file monitor thread. The thread will now auto-disable.');
            WriteLog( E.ClassName+': '+E.Message );
            MonitorPaused := true;
          end;
      end;

      // Slep (no grammar errors here)
      Sleep( MonitorDelay );
    end;
end;


procedure TSystemThread.LoadModificationDates;
begin
  NowModConfig := TFile.GetLastWriteTime( ConfigFile );
  NowModDefaultMapping := TFile.GetLastWriteTime( DefaultMappingFile );
  NowModMappings := TFile.GetLastWriteTime( MappingsFile );
end;

procedure TSystemThread.WriteModificationDates;
begin
  LastModConfig := NowModConfig;
  LastModDefaultMapping := NowModDefaultMapping;
  LastModMappings := NowModMappings;
end;

function ReadInput(Question: string=''; Default: string=''): string;
begin
  WriteLn('// Input value');
  WriteLn(Question);
  Write(' >>');
  ReadLn(Result);
  if Result = '' then
    Result := Default;
end;

function ReadNumber(Question: string=''; Default: integer=0): integer;
var
  Read: string;
  Valid: integer;
begin
  WriteLn('// Input number value');
  WriteLn(Question);
  Write(' >>');
  ReadLn(Read);
  if Read = '' then
    Exit(Default);
  Val(Read, Result, Valid);

  while Valid <> 0 do
    begin
      WriteLn('Please enter a valid integer value.');
      Write(' >>');
      ReadLn(Read);
      if Read = '' then
        Exit(Default);
      Val(Read, Result, Valid)
    end;
end;

function ReadYesNo(Question: string=''): boolean;
var
  Read: string;
begin
  WriteLn(Question);
  WriteLn('Type YES or NO than press enter.');
  Write(' >>');
  ReadLn(Read);
  Read := LowerCase(Read);

  while (Read <> STRING_YES) and (Read <> STRING_NO) do
    begin
      WriteLn('Please type YES or NO.');
      Write(' >>');
      ReadLn(Read);
      Read := LowerCase(Read);
    end;

  Result := Read = STRING_YES;
end;

{ TMapperManager }

constructor TMapperManager.Create;
begin
  Mapper := TMinecraftPortTCP.Create(nil);

  Mapper.OnBeforeConnect := OnBeforeConnect;
  Mapper.OnMap := OnMap;
  Mapper.OnDisconnect := OnDisconnect;
  Mapper.OnReject := OnReject;
  Mapper.OnFailSignature := OnFailSignature;
  Mapper.OnFailOutboundConnect := OnFailOutboundConnect;
end;

destructor TMapperManager.Destroy;
begin
  Mapper.Free;
  inherited;
end;

procedure TMapperManager.LoadConfigurations;
procedure RaiseError;
begin
  raise TErrorException.Create('Could not load configuration file.');
end;
var
  JSON: TJSONObject;
  Value: integer;
begin
  if not fileexists(ConfigFile) then
    raise TErrorException.Create('Missing configuration file. Use '+PARAM_PREFIX+'create to create It');

  JSON := LoadJSON(ConfigFile) as TJSONObject;
  if JSON = nil then
    RaiseError;
  try
    try
      // Mapper
      Mapper.DefaultAllow := JSON.GetValue<boolean>('allow-default-mapping');
      Value := JSON.GetValue<word>('listen-port');
      if (Value <> Mapper.DefaultPort) or not Mapper.Active then
        begin
          Mapper.Active := false;
          Mapper.DefaultPort := Value;

          WriteLog( Format('Connecting on port: %D', [Mapper.DefaultPort]) );
          Mapper.Active := true;
          WriteLog('Listener connected!');
        end;

      // File monitor
      Value := JSON.GetValue<word>('file-monitor-delay');
      Thread.MonitorPaused := true;
      if Value > 0 then
        begin
          MonitorDelay := Value;
          Thread.MonitorPaused := false;
        end;
    finally
      JSON.Free;
    end;
  except
    RaiseError;
  end;
end;

function TMapperManager.LoadDefaultMapping: TMinecraftMapping;
procedure RaiseError;
begin
  raise TErrorException.Create('Could not load default mapping.');
end;
var
  JSON: TJSONObject;
begin
  if not fileexists(DefaultMappingFile) then
    raise TErrorException.Create('Missing default mapping file. Use '+PARAM_PREFIX+'create to create It');
  JSON := LoadJSON(DefaultMappingFile) as TJSONObject;
  if JSON = nil then
    RaiseError;
  try
    try
      Result := ObjectToMapping(JSON);
    finally
      JSON.Free;
    end;
  except
    RaiseError;
  end;
end;

function TMapperManager.LoadMappings: TMinecraftMappings;
procedure RaiseError;
begin
  raise TErrorException.Create('Could not load server mappings.');
end;
var
  JSON: TJSONArray;
begin
  if not fileexists(MappingsFile) then
    raise TErrorException.Create('Missing mappings file. Use '+PARAM_PREFIX+'create to create It');
  JSON := LoadJSON(MappingsFile) as TJSONArray;
  if JSON = nil then
    RaiseError;
  try
    try
      SetLength(Result, JSON.Count);
      for var I := 0 to JSON.Count-1 do
        Result[I] := ObjectToMapping( JSON.Items[I] as TJSONObject );
    finally
      JSON.Free;
    end;
  except
    RaiseError;
  end;
end;

function TMapperManager.ObjectToMapping(JSON: TJSONObject): TMinecraftMapping;
begin
  with Result do
    begin
      // Try get address (default mapping has no address)
      if not JSON.TryGetValue<string>('address', Address) then
        Address := '';
      if not JSON.TryGetValue<string>('expression-match', Expression) then
        Expression := '';

      // Get host & port
      Host := JSON.GetValue<string>('host');
      Port := JSON.GetValue<word>('port');
    end;
end;

procedure TMapperManager.OnBeforeConnect(AContext: TIdContext);
begin
  WriteLog(
    Format('New connection from: "%S:%D".', [AContext.Binding.PeerIP, AContext.Binding.Port])
    );
end;

procedure TMapperManager.OnDisconnect(AContext: TIdContext);
begin
  WriteLog(
    Format('Disconnected from: "%S:%D", server: "%S".', [AContext.Binding.PeerIP,
      AContext.Binding.Port, TMinecraftPortContext(AContext).Host])
    );
end;

procedure TMapperManager.OnFailOutboundConnect(AContext: TIdContext);
begin
  WriteLog(
    Format('Could not connect "%S" to outbound server "%S:%D".', [AContext.Binding.PeerIP,
    TMinecraftPortContext(AContext).Host, TMinecraftPortContext(AContext).Port])
    );
end;

procedure TMapperManager.OnFailSignature(AContext: TIdContext);
begin
  WriteLog(
    Format('Rejected "%S", signature reading failed.', [AContext.Binding.PeerIP])
    );
end;

procedure TMapperManager.OnMap(AContext: TIdContext);
begin
  WriteLog(
    Format('Mapped: "%S, server "%S" to "%S:%D".', [AContext.Binding.PeerIP,
      TMinecraftPortContext(AContext).Address,
      TMinecraftPortContext(AContext).Host, TMinecraftPortContext(AContext).Port])
    );
end;

procedure TMapperManager.OnReject(AContext: TIdContext);
begin
  WriteLog(
    Format('Rejected: "%S:%D". No server named "%S" was found.', [AContext.Binding.PeerIP,
      AContext.Binding.Port, TMinecraftPortContext(AContext).Address])
    );
end;

procedure TMapperManager.ProcessDefaultMapping;
var
  NewMapping: TMinecraftMapping;
begin
  NewMapping := LoadDefaultMapping;

  if not Mapper.DefaultMapping.IsEqual(NewMapping) then
    Mapper.SetDefaultMapping( NewMapping );
end;

procedure TMapperManager.ProcessMappingList;
var
  NewList: TMinecraftMappings;
  Found: boolean;
  I, J: integer;
begin
  NewList := LoadMappings;

  // Check deleted
  for I := High(Mapper.Mappings) downto 0 do
    begin
      Found := false;
      for J := 0 to High(NewList) do
        if Mapper.Mappings[I].IsEqual(NewList[J]) then
          begin
            Found := true;
            Break;
          end;

      // Delete
      if not Found then
        begin
          WriteLog( Format('Deleted mapping "%S"', [Mapper.Mappings[I].Address]) );
          Mapper.RemoveMapping( I );
        end;
    end;

  // Check new servers
  for I := 0 to High(NewList) do
    begin
      Found := false;
      for J := 0 to High(Mapper.Mappings) do
        if NewList[I].IsEqual(Mapper.Mappings[J]) then
          begin
            Found := true;
            Break;
          end;

      // Add new item
      if not Found then
        begin
          Mapper.AddMapping( NewList[I] );
        end;
    end;
end;

begin
  // Logfile
  RUN_DIRECTORY := IncludeTrailingPathDelimiter(ExtractFileDir( ParamStr(0) ));
  LOG_FILE := RUN_DIRECTORY+LOG_FILE;

  // Append executable location
  ConfigFile := RUN_DIRECTORY+ConfigFile;
  DefaultMappingFile := RUN_DIRECTORY+DefaultMappingFile;
  MappingsFile := RUN_DIRECTORY+MappingsFile;

  if HasParameter('quit', 'q') then
    Exit;
  UseScreenEcho := not HasParameter('no-echo', 'n');
  UseLogFile := HasParameter('logfile', 'l');
  UseServiceMode := HasParameter('service-mode', 's');
  if HasParameter('config-file', 'k') then
    ConfigFile := GetParameterValue('config-file', 'k');
  if HasParameter('default-mappings-file', 'd') then
    DefaultMappingFile := GetParameterValue('default-mappings-file', 'd');
  if HasParameter('mappings-file', 'm') then
    MappingsFile := GetParameterValue('mappings-file', 'm');
  if HasParameter('open-dir', 'o') then begin
    const OpenDirectory = GetParameterValue('open-dir', 'o');
    if not TDirectory.Exists(OpenDirectory) then begin
      WriteError('Invalid mapping file! Quitting...');
      Exit;
    end;
    ChDir( OpenDirectory );
  end;

  // Version
  if HasParameter('version', 'v') then
    begin
      WriteLn('Minecraft Server Mapper');
      WriteLn('Version '+VERSION);

      // Done
      Exit;
    end;

  // Setup mode
  if HasParameter('create', 'c') then
    begin
      var JSON: TJSONValue;
      WriteLn('Configuration mode');
      WriteLn(LINE_SEPARATOR);
      WriteLn('In this mode you can create the configuration files that MineMapper uses');
      WriteLn('');
      WriteLn('Default mapping file');
      WriteLn(DefaultMappingFile);
      if TFile.Exists(DefaultMappingFile) then
        WriteLn('[i] A file with this name already exists.');
      if ReadYesNo('Do you want to create this file') then
        begin
          JSON := TJSONObject.Create;
          try
            WriteLn('Default mapping configuration');
            WriteLn(LINE_SEPARATOR);
            with JSON as TJSONObject do
              begin
                AddPair('host',
                  ReadInput('Enter the hostname. (default: localhost)', 'localhost')
                );
                AddPair('port',
                  ReadNumber('Enter the port number. (default: 0)', 0)
                );
              end;

            WriteJSON(DefaultMappingFile, JSON);
          finally
            JSON.Free;
          end;
        end;
      WriteLn('');

      WriteLn('Primary mappings file');
      WriteLn(MappingsFile);
      if TFile.Exists(MappingsFile) then
        WriteLn('[i] A file with this name already exists.');
      if ReadYesNo('Do you want to create this file') then
        begin
          JSON := TJSONArray.Create;
          try
            if ReadYesNo('Do you want to create an template example file in this file?') then
              begin
                const Item = TJSONObject.Create;
                Item.AddPair('address', 'subdomain.website.com');
                Item.AddPair('host', 'localhost');
                Item.AddPair('port', 25570);

                (JSON as TJSONArray).AddElement( Item );
              end;

            WriteJSON(MappingsFile, JSON);
          finally
            JSON.Free;
          end;
        end;
      WriteLn('');

      WriteLn('Configurations file');
      WriteLn(ConfigFile);
      if TFile.Exists(ConfigFile) then
        WriteLn('[i] A file with this name already exists.');
      if ReadYesNo('Do you want to create this file') then
        begin
          JSON := TJSONObject.Create;
          try
            with JSON as TJSONObject do
              begin
                AddPair('listen-port',
                  ReadNumber('Enter the listening port number. (default: 25565)', 25565)
                );
                AddPair('file-monitor-delay',
                  ReadNumber('Enter the file monitor delay, which is the time the thread sleeps for in milliseconds. (default: 2000)', 2000)
                );
                AddPair('allow-default-mapping',
                  ReadYesNo('Do you want to enable the default port mapping?')
                );
                AddPair('expression-match', '');
              end;

            WriteJSON(ConfigFile, JSON);
          finally
            JSON.Free;
          end;
        end;
      WriteLn('');

      // Done
      Exit;
    end;

  // Help
  if HasParameter('help', 'h') then
    begin
      WriteLn('Minecraft indexer');
      WriteLn(LINE_SEPARATOR);
      WriteLn('Version '+VERSION);
      WriteLn('');
       WriteLn('Parameters');
       WriteLn(LINE_SEPARATOR);
      WriteLn('-l '+PARAM_PREFIX+'logfile -> Echo log statements to log file');
      WriteLn('-n '+PARAM_PREFIX+'no-echo -> Do not echo to the console');
      WriteLn('-s '+PARAM_PREFIX+'service-mode -> Start application in service mode. Which disables input reading');
      WriteLn('-k '+PARAM_PREFIX+'config-file <path> -> Provide custom location for config file');
      WriteLn('-d '+PARAM_PREFIX+'default-mappings-file <path> -> Provide custom location for default mapping file');
      WriteLn('-m '+PARAM_PREFIX+'mappings-file <path> -> Provide custom location for the mappings file');
      WriteLn('-c '+PARAM_PREFIX+'create -> Start application in setup mode. Which lets you create files and manage settings.');
      WriteLn('-o '+PARAM_PREFIX+'open-dir -> The directory where the mapper will open to');
      WriteLn('-h '+PARAM_PREFIX+'help -> Show this help dialog');
      WriteLn('-v '+PARAM_PREFIX+'version -> Output version');
      WriteLn('Role');
      WriteLn(LINE_SEPARATOR);
      WriteLn('This application completes the following tasks:');
      WriteLn('- map certain server adressed to specific ports');
      WriteLn('- proxy packets from clients to server');
      WriteLn('- return status requests on offline or maintanance');

      // Done
      Exit;
    end;

  // Server
  WriteLn('Starting mapper...');

  if UseServiceMode then
    WriteLog('Application started in Service Mode. Use Ctrl+C to quit.');

  WriteLog('Creating mapping manager...');
  Manager := TMapperManager.Create;

  WriteLog('Creating file monitor...');
  Thread := TSystemThread.Create;
  try
    // Load servers
    Manager.ProcessMappingList;
    Manager.ProcessDefaultMapping;

    // Reset file monitor
    Thread.LoadModificationDates;
    Thread.WriteModificationDates;

    // Load configuration & start server
    Manager.LoadConfigurations;

    // Input command
    var I: integer;
    repeat
      ReadIOInput := #0;

      // Read input
      if UseServiceMode then
        Sleep(10000)
      else
        begin
          // Read
          ReadIOInput := TConsole.WaitUntillKeyCharPressed;

          //WriteLn(' >> '+ReadIOInput);
        end;

      // Handle
      if not CharInSet(ReadIOInput, [#0, #13]) then
      case ReadIOInput of
        // Quit
        'q': begin
          WriteLn('Exiting program...');
          Break;
        end;

        'a': begin
          WriteLn( Format('Local address: %S', [GStack.LocalAddress]) );
        end;

        'h': begin
          WriteLn('Help');
          WriteLn(LINE_SEPARATOR);
          WriteLn('t - Echo the total connections');
          WriteLn('c - List conencted clients');
          WriteLn('m - List all registered mappings with their configuration');
          WriteLn('a - Echo the local adress');
          WriteLn('h - List help');
          WriteLn('v - Show version number');
          WriteLn('0 <index> - Disconnect the specified connection by index');
          WriteLn('q - Quit mine mapper');
        end;

        't': begin
          WriteLog( Format('There are %D currently active connections', [Manager.Mapper.ConnectionCount]) );
        end;

        'c': begin
          WriteLn('Connected clients');
          WriteLn(LINE_SEPARATOR);
          for I := 0 to Manager.Mapper.ConnectionCount-1 do
            with Manager.Mapper.ActiveConnections[I] do
              begin
                WriteLn( Format('Connection no. %D', [I]) );
                WriteLn( Format(' -> IP: %S', [Binding.PeerIP]) );
                WriteLn( Format(' -> Bound to: %S:%D', [Host, Port]) );
              end;
        end;

        'v': begin
          WriteLn('Minecraft mapper');
          WriteLn(LINE_SEPARATOR);
          WriteLn('Version '+VERSION);
          WriteLn('Copyright (c) 2024 Codrut Software');
        end;

        '0': begin
          const Index = ReadNumber('Enter the index of the connection to kill');
          if InRange(Index, 0, Manager.Mapper.ConnectionCount-1) then
            begin
              Manager.Mapper.ActiveConnections[0].Binding.CloseSocket;
              WriteLog('Closed socket');
              Break;
            end;

          WriteLog('Could not find connection index');
        end;

        'm': begin
          WriteLn('Registered mappings');
          WriteLn(LINE_SEPARATOR);
          for I := 0 to High(Manager.Mapper.Mappings) do
            with Manager.Mapper.Mappings[I] do
              begin
                WriteLn( Format('Mapping no. %D', [I]) );
                WriteLn( Format(' -> Address: %S', [Address]) );
                WriteLn( Format(' -> Destination: %S:%D', [Host, Port]) );
              end;
        end;

        else begin
          WriteLn('Unknown command');
          WriteLn('Press "h" for a list of avalabile commands.');
        end;
      end;
    until false;
  except
    on E: Exception do
      begin
        if E.ClassName = 'EControlC' then
          begin
            TConsole.GoLineStart;
            WriteLn('Ctrl+C pressed. Now exiting...');
          end
        else
        if E.ClassName = 'TErrorException' then
          WriteError(E.Message)
        else
          WriteLog(E.ClassName+': '+E.Message);
      end;
  end;

  // Mark avalabile
  ReadIOInput := #0;

  // Stop thraed
  WriteLog('Freeing mapper from memory...');
  Manager.Free;
  WriteLog('Waiting for file monitor to close...');
  Thread.Terminate;
  Thread.WaitFor;
end.
