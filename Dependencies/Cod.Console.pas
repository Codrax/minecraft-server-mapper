unit Cod.Console;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.TLHelp32,
  Winapi.PsAPI,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Termios,
  {$ENDIF}
  SysUtils,
  Classes,
  Math,
  Types;

  const
    SEPARATOR_LENGTH = 10;
    {$IFDEF MSWINDOWS}
    SEPARATOR_CHAR = '=';
    {$ELSE}
    SEPARATOR_CHAR = '-';
    {$ENDIF}

  type
    // Types
    {$IFDEF MSWINDOWS}
    // renamed "Purple" to "Magenta" for better compatability
    TConsoleColor = (Black, Blue, Green, Cyan{Aqua}, Red, Magenta, Yellow, White,
      LightBlack, LightBlue, LightGreen, LightCyan{LightAqua}, LightRed, LightMagenta,
      LightYellow, LightWhite);
    {$ENDIF}
    {$IFDEF POSIX}
    TConsoleColor = (Black, Red, Green, Brown, Blue, Magenta, Cyan, LightGray,
      DarkGray, LightRed, LightGreen, Yellow, LightBlue, LightMagenta,
      LightCyan, White);
    {$ENDIF}

    // System Console Class
    /// <summary>
    /// This class allows UI application executed from the console to have
    /// access to console output and parent process. Most Cod.Console utils
    // will work after connecting.
    /// </summary>
    {$IFDEF MSWINDOWS}
    sysconsole = class
    public
      /// <summary>
      /// Outpus a boolean if the application is called from a console.
      /// </summary>
      class function ConsoleCalled: boolean;
      /// <summary>
      /// Get parent console process.
      /// </summary>
      class function GetParentProcessName: String;
      /// <summary>
      /// Conenct to parent console (optional).
      /// </summary>
      class function Connect(force: boolean = false): boolean;
      /// <summary>
      /// Disconnect from parent console (optional).
      /// </summary>
      class procedure Disconnect;
      /// <summary>
      /// Output to console using WriteLn()
      /// </summary>
      class procedure QuickOutput(text: string);
    end;
    {$ENDIF}

    // Console class utils
    TConsole = class
    private
      {$IFDEF MSWINDOWS}
      class function GetTitle: string; static;
      class procedure SetTitle(const Value: string); static;
      class function GetCurPos: TPoint; static;
      {$ENDIF}
      class procedure SetCurPos(const Value: TPoint); static;
      {$IFDEF MSWINDOWS}
      class function GetCursorVisible: boolean; static;
      class procedure SetCursorVisible(const Value: boolean); static;
      class function GetAttrib: word; static;
      class procedure SetAttrib(const Value: word); static;
      {$ENDIF}
      class procedure WriteConsole(const Value: string); static;
      class procedure WriteLineConsole(const Value: string); static;
      {$IFDEF MSWINDOWS}
      class function GetTextColor: TConsoleColor; static;
      {$ENDIF}
      class procedure SetTextColor(const Value: TConsoleColor); static;
      {$IFDEF MSWINDOWS}
      class function GetBackgroundColor: TConsoleColor; static;
      {$ENDIF}
      class procedure SetBackgroundColor(const Value: TConsoleColor); static;
      class procedure OverWriteConsole(const Value: string); static;
      class procedure WriteLineClearConsole(const Value: string); static;
      class function ReadLineConsole: string; static;

    public
      /// <summary> Console handle </summary>
      {$IFDEF MSWINDOWS}
      class function Handle: THandle;
      {$ENDIF}

      // Console Settings
      {$IFDEF MSWINDOWS}
      class property Title: string read GetTitle write SetTitle;
      class property CursorVisible: boolean read GetCursorVisible write SetCursorVisible;
      {$ENDIF}
      class property CursorPos: TPoint{$IFDEF MSWINDOWS} read GetCurPos{$ENDIF} write SetCurPos;

      // Style
      {$IFDEF MSWINDOWS}
      class property TextAttrib: word read GetAttrib write SetAttrib;
      {$ENDIF}
      class property TextColor: TConsoleColor{$IFDEF MSWINDOWS} read GetTextColor{$ENDIF} write SetTextColor;
      class property BackgroundColor: TConsoleColor{$IFDEF MSWINDOWS} read GetBackgroundColor{$ENDIF} write SetBackgroundColor;

      class procedure SetStyle(AText, ABackground: TConsoleColor);
      class procedure ResetStyle;

      // Console write
      /// <summary> Write text to the console </summary>
      class property Write: string write WriteConsole;
      /// <summary> Write text to the console, go to next line </summary>
      class property WriteLine: string write WriteLineConsole;
      /// <summary> Read a string input from the console, go to next line </summary>
      class property ReadLine: string read ReadLineConsole;
      /// <summary> Write text to the console, reset the style, go to next line. Usefull when drawing colored lines </summary>
      class property WriteLineClear: string write WriteLineClearConsole;
      /// <summary> Override current line contents </summary>
      class property OverWrite: string write OverWriteConsole;
      /// <summary> Write a character a certain number of times on a line </summary>
      class procedure WriteSeparatorLine(Length: cardinal=SEPARATOR_LENGTH; Character: char=SEPARATOR_CHAR; ResetStyle: boolean=false);

      // Console Read
      {$IFDEF MSWINDOWS}
      class function ReadConsole(AFrom: integer; ACount: integer): string;
      class function ReadLastOutput: string;

      class function ReadConsoleLine(AIndex: integer): string;
      class function ReadConsoleOutput: string;
      class function ReadConsoleLines: TArray<string>;
      {$ENDIF}

      // Console Size
      {$IFDEF MSWINDOWS}
      class function GetWidth: integer;
      class function GetHeight: integer;

      class function GetConsoleRect: TRect;
      {$ENDIF}

      // Utils
      /// <summary> Go the the beggining of a line </summary>
      class procedure GoLineStart;
      /// <summary> Write a sLineBreak that moves the cursor to the next line </summary>
      class procedure NextLine;
      {$IFDEF MSWINDOWS}
      class procedure ClearLine;
      {$ENDIF}
      class procedure ClearAll;

      class function WaitUntillKeyCharPressed: char;
      {$IFDEF MSWINDOWS}
      class function WaitUntillKeyPressed: word;
      class function WaitForWindowFocus: boolean;
      {$ENDIF}

      // Other
      {$IFDEF MSWINDOWS}
      class function GetRec: TTextRec;
      class function GetScreenBuffer: TConsoleScreenBufferInfo;
      {$ENDIF}
    end;

  // System
  {$IFDEF MSWINDOWS}
  function AttachConsole(dwProcessID: Integer): Boolean; stdcall; external 'kernel32.dll';
  function FreeConsole(): Boolean; stdcall; external 'kernel32.dll';
  {$ENDIF}

const
  {$IFDEF POSIX}
  CSI = #27'[';
  CSBold =   '1';
  CSNormal = '0';
  CSFGColor = '3';
  CSBGColor = '4';
  {$ENDIF}

  // Def
  {$IFDEF MSWINDOWS}
  DEFAULT_STYLE = $0F;
  {$ENDIF}

implementation
{$IFDEF MSWINDOWS}
var
  connected: boolean;
{$ENDIF}

{ sysconsole }
{$IFDEF MSWINDOWS}
class function sysconsole.GetParentProcessName: String;
const
  BufferSize = 4096;
var
  HandleSnapShot: THandle;
  EntryParentProc: TProcessEntry32;
  CurrentProcessId: THandle;
  HandleParentProc: THandle;
  ParentProcessId: THandle;
  ParentProcessFound: Boolean;
  ParentProcPath: String;
begin
  ParentProcessFound:=False;
  HandleSnapShot:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  if HandleSnapShot<>INVALID_HANDLE_VALUE then
  begin
    EntryParentProc.dwSize:=SizeOf(EntryParentProc);
    if Process32First(HandleSnapShot,EntryParentProc) then
    begin
      CurrentProcessId:=GetCurrentProcessId();
      repeat
        if EntryParentProc.th32ProcessID=CurrentProcessId then
        begin
          ParentProcessId:=EntryParentProc.th32ParentProcessID;
          HandleParentProc:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,False,ParentProcessId);
          if HandleParentProc<>0 then
          begin
            ParentProcessFound:=True;
            SetLength(ParentProcPath,BufferSize);
            GetModuleFileNameEx(HandleParentProc,0,PChar(ParentProcPath),BufferSize);
            ParentProcPath:=PChar(ParentProcPath);
            CloseHandle(HandleParentProc);
          end;
          Break;
        end;
      until not Process32Next(HandleSnapShot,EntryParentProc);
    end;
    CloseHandle(HandleSnapShot);
  end;
  if ParentProcessFound then Result:=ParentProcPath
  else Result:='';
end;

class procedure sysconsole.QuickOutput(text: string);
begin
  if not connected then
    if not Connect then
      Exit;
  WriteLn(text);
end;

class function sysconsole.Connect(force: boolean): boolean;
begin
  Result := false;
  if connected then Exit;

  if ConsoleCalled or force then begin
    AttachConsole(-1);
    Result := true;
    connected := true;
  end;
end;

class function sysconsole.ConsoleCalled: boolean;
var
  ParentName: string;
begin
  ParentName:=GetParentProcessName().ToLower;
  Delete(ParentName,1,ParentName.LastIndexOf('\')+1);
  if (ParentName='cmd.exe') or (ParentName='powershell.exe') then
    Result := true
  else
    Result := false;
end;

class procedure sysconsole.Disconnect;
begin
  connected := false;

  FreeConsole();
end;
{$ENDIF}

{ TConsole }

class procedure TConsole.ClearAll;
begin
  {$IFDEF MSWINDOWS}
  var I: integer;
  for I := GetHeight-1 downto 0 do
    begin
      CursorPos := Point(0, I);

      ClearLine;
    end;
  {$ELSE}
  System.Write(CSI, '2J');
  SetCurPos(Point(0, 0));
  BackgroundColor := TConsoleColor.Black;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
class function TConsole.GetAttrib: word;
begin
  Result := GetScreenBuffer.wAttributes;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class function TConsole.GetBackgroundColor: TConsoleColor;
var
  I: integer;
begin
  I := TextAttrib and $F0 shr 4;

  Result := TConsoleColor(I);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class function TConsole.GetCursorVisible: boolean;
var
  D: TConsoleCursorInfo;
begin
  Winapi.Windows.GetConsoleCursorInfo(Handle, D);

  Result := D.bVisible;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class function TConsole.GetRec: TTextRec;
begin
  Result := TTextRec(Output);
end;

class function TConsole.GetScreenBuffer: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), Result);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class function TConsole.GetTextColor: TConsoleColor;
var
  I: integer;
begin
  I := TextAttrib and $0F;

  Result := TConsoleColor(I);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class function TConsole.GetTitle: string;
var
  P: PWideChar;
begin
  P := '';
  if Succeeded(Winapi.Windows.GetConsoleTitle(p, SizeOf(PWideChar)*255)) then
    Result := P;
end;

class procedure TConsole.SetTitle(const Value: string);
begin
  SetConsoleTitle(PChar(Value));
end;

class function TConsole.GetCurPos: TPoint;
var
  C: TCoord;
begin
  C := GetScreenBuffer.dwCursorPosition;
  Result := Point(C.X, C.Y);
end;
{$ENDIF}

class procedure TConsole.SetCurPos(const Value: TPoint);
begin
  {$IFDEF MSWINDOWS}
  var C: TCoord;
  C.X := Value.X;
  C.Y := Value.Y;
  SetConsoleCursorPosition(Handle, C);
  {$ELSE}
  // On Linux, position indexing starts from 1, so 0 is equivalent to 1
  System.Write(CSI, Value.Y+1, ';', Value.X+1, 'H');
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
class function TConsole.GetWidth: integer;
begin
  Result := GetScreenBuffer.dwMaximumWindowSize.X;
end;

class function TConsole.GetHeight: integer;
begin
  Result := GetScreenBuffer.dwMaximumWindowSize.Y;
end;

class function TConsole.GetConsoleRect: TRect;
var
  R: TSmallRect;
begin
  R := GetScreenBuffer.srWindow;

  Result := Rect(R.Left, R.Top, R.Right, R.Bottom);
end;
{$ENDIF}

class procedure TConsole.GoLineStart;
begin
  {$IFDEF MSWINDOWS}
  CursorPos := Point(0, CursorPos.Y);
  {$ELSE}
  System.Write(#$D); // back to line start
  {$ENDIF}
end;
class procedure TConsole.NextLine;
begin
  System.Write( sLineBreak );
end;

{$IFDEF MSWINDOWS}
class procedure TConsole.ClearLine;
begin
  {$IFDEF MSWINDOWS}
  var I, C: integer;
  var S: string;
  C := GetWidth;
  S := '';
  for I := 1 to C do
    S := S + ' ';

  GoLineStart;
  Write := S;
  GoLineStart;
  {$ELSE}
  System.Write(CSI, '3J'); // does not work...
  GoBeginLine;
  BackgroundColor := TConsoleColor.Black;
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class function TConsole.Handle: THandle;
begin
  Result := GetRec.Handle;
end;

{$ENDIF}

class procedure TConsole.OverWriteConsole(const Value: string);
begin
  GoLineStart;
  System.Write(Value);
end;

class function TConsole.ReadLineConsole: string;
begin
  ReadLn( Result );
end;

{$IFDEF MSWINDOWS}
class function TConsole.ReadConsole(AFrom, ACount: integer): string;
begin
  Result := Copy(ReadConsoleOutput, AFrom, ACount);
end;

class function TConsole.ReadLastOutput: string;
var
  R: TTextRec;
  I: Integer;
begin
  R := GetRec;

  Result := '';
  for I := 0 to R.BufSize-1 do
    Result := Result + char(R.Buffer[I]);
end;

class function TConsole.ReadConsoleLine(AIndex: integer): string;
var
  Coord: TCoord;
  NumChars: DWORD;
  Text: array of Char;
begin
  Coord.X := 0;
  Coord.Y := AIndex;

  SetLength(Text, GetWidth);

  ReadConsoleOutputCharacter(Handle, @Text[0], Length(Text), Coord, NumChars);

  Result := Trim(string(Text));
end;

class function TConsole.ReadConsoleOutput: string;
var
  Lines: TArray<string>;
  I: integer;
begin
  Result := '';
  Lines := ReadConsoleLines;

  for I := 0 to High(Lines) do
    Result := Result + Lines[I] + #$D#$A;

  Result := Trim(Result);
end;

class function TConsole.ReadConsoleLines: TArray<string>;
var
  ConsoleOutput: THandle;
  Coord: TCoord;
  NumChars: DWORD;

  Text: array of char;

  I, Index: Integer;
begin
  ConsoleOutput := Handle;

  // Prepare size
  const AWid = GetWidth;
  const AHei = GetHeight;

  for I := 0 to AHei-1 do
    begin
      // Pos
      Coord.X := 0;
      Coord.Y := I;

      // Prep
      NumChars := 0;
      SetLength(Text, AWid);

      // Read
      if not ReadConsoleOutputCharacter(ConsoleOutput, @Text[0], Length(Text), Coord, NumChars) then
        RaiseLastOSError;

      // New
      Index := Length(Result);
      SetLength(Result, Index + 1);

      // Parse
      const ResString = Trim(string(Text));

      // Set
      Result[Index] := ResString;
    end;

  // Size
  const ATotal = High(Result);
  for I := ATotal downto 0 do
    if Result[I] = '' then
      SetLength(Result, I)
    else
      Break;
end;
{$ENDIF}

class procedure TConsole.ResetStyle;
begin
  {$IFDEF MSWINDOWS}
  SetAttrib( DEFAULT_STYLE );
  {$ELSE}
  BackgroundColor := TConsoleColor.Black;
  TextColor := TConsoleColor.White;
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
class procedure TConsole.SetAttrib(const Value: word);
begin
  SetConsoleTextAttribute(Handle, Value);
end;
{$ENDIF}

class procedure TConsole.SetBackgroundColor(const Value: TConsoleColor);
begin
  {$IFDEF MSWINDOWS}
  var I: integer;
  I := TextAttrib and $FF0F;
  I := I or (Word(Value) shl 4);
  TextAttrib := I;
  {$ELSE}
  System.Write(CSI);
  if Value > TConsoleColor.LightGray then
    System.Write(CSBold)
  else
    System.Write(CSNormal);
  System.Write(';', CSBGColor, (byte(Value) and $07), 'm');
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
class procedure TConsole.SetCursorVisible(const Value: boolean);
var
  D: TConsoleCursorInfo;
begin
  Winapi.Windows.GetConsoleCursorInfo(Handle, D);

  D.bVisible := Value;

  Winapi.Windows.SetConsoleCursorInfo(Handle, D);
end;
{$ENDIF}
class procedure TConsole.SetStyle(AText, ABackground: TConsoleColor);
begin
  TextColor := AText;
  BackgroundColor := ABackground;
end;

class procedure TConsole.SetTextColor(const Value: TConsoleColor);
begin
  {$IFDEF MSWINDOWS}
  var I: integer;
  I := TextAttrib and $FFF0;
  I := I or Integer(Value);
  TextAttrib := I;
  {$ELSE}
  System.Write(CSI);
  if Value > TConsoleColor.LightGray then
    System.Write(CSBold)
  else
    System.Write(CSNormal);
  System.Write(';', CSFGColor, (byte(Value) and $07), 'm');
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
class function TConsole.WaitForWindowFocus: boolean;
var
  InputRecord: TInputRecord;
  EventsRead: DWORD;
begin
  Result := false;
  while not Result do
  begin
    ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), InputRecord, 1, EventsRead);

    if (EventsRead > 0) and (InputRecord.EventType = FOCUS_EVENT) and InputRecord.Event.FocusEvent.bSetFocus then
    begin
      Result := true;
    end;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
class function TConsole.WaitUntillKeyPressed: word;
var
  InputRecord: TInputRecord;
  EventsRead: DWORD;
begin
  while true do
  begin
    ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), InputRecord, 1, EventsRead);

    if (EventsRead > 0) and (InputRecord.EventType = KEY_EVENT) and InputRecord.Event.KeyEvent.bKeyDown then
    begin
      Result := InputRecord.Event.KeyEvent.wVirtualKeyCode;
      Break
    end;
  end;
end;
{$ENDIF}

class function TConsole.WaitUntillKeyCharPressed: char;
begin
  {$IFDEF MSWINDOWS}
  var InputRecord: TInputRecord;
  var EventsRead: DWORD;
  Result := #0;
  while Result = #0 do
  begin
    ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), InputRecord, 1, EventsRead);

    if (EventsRead > 0) and (InputRecord.EventType = KEY_EVENT) and InputRecord.Event.KeyEvent.bKeyDown then
    begin
      Result := InputRecord.Event.KeyEvent.UnicodeChar;
    end;
  end;
  {$ELSE}
  var New, Old: Posix.Termios.termios;
  tcgetattr(TTextRec(Input).Handle, Old);
  tcgetattr(TTextRec(Input).Handle, New);
  cfmakeraw(New);
  tcsetattr(TTextRec(Input).Handle, TCSANOW, New);
  //Flush(Input);
  Read(Input, Result);
  tcsetattr(TTextRec(Input).Handle, TCSANOW, Old);
  {$ENDIF}
end;

class procedure TConsole.WriteConsole(const Value: string);
begin
  System.Write(Value);
end;

class procedure TConsole.WriteLineClearConsole(const Value: string);
begin
  System.Write( Value );
  ResetStyle;
  NextLine; // writes sLineBreak
end;

class procedure TConsole.WriteLineConsole(const Value: string);
begin
  System.WriteLn(Value);
end;

class procedure TConsole.WriteSeparatorLine(Length: cardinal; Character: char; ResetStyle: boolean);
begin
  var S: string; S := '';
  for var I := 1 to Length do
    S := S + Character;

  if ResetStyle then
    WriteLineClear := S
  else
    WriteLine := S;
end;

end.
