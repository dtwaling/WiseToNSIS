unit WSE.Flags;

{******************************************************************************
  WiseToNSIS Converter - Binary Flag Interpreter

  Interprets Wise Install binary flag strings and converts them to meaningful
  values for NSIS generation.
******************************************************************************}

interface

uses
  System.SysUtils;

type
  { If/While comparison operators }
  TWseComparisonOp = (
    wcoEquals,          // ==
    wcoNotEquals,       // !=
    wcoGreaterThan,     // >
    wcoContains,        // String contains
    wcoNotContains,     // String not contains
    wcoLessThan,        // <
    wcoFileExists,      // File exists check
    wcoFileNotExists    // File not exists check
  );

  { Message box button types }
  TWseMessageBoxType = (
    wmbOK,
    wmbOKCancel,
    wmbYesNo,
    wmbYesNoCancel,
    wmbRetryCancel,
    wmbAbortRetryIgnore
  );

  { Message box icon types }
  TWseMessageBoxIcon = (
    wmiNone,
    wmiInformation,
    wmiWarning,
    wmiError,
    wmiQuestion
  );

  { File operation flags }
  TWseFileFlags = record
    Overwrite: Boolean;
    SelfRegister: Boolean;
    Shared: Boolean;
    Recursive: Boolean;
    CompressFiles: Boolean;
    NoUninstall: Boolean;
    ReplaceInUse: Boolean;
    CreateBackup: Boolean;
  end;

  { Registry operation flags }
  TWseRegistryFlags = record
    CreateKey: Boolean;
    DeleteKey: Boolean;
    DeleteValue: Boolean;
    IsDWORD: Boolean;
    IsExpandSz: Boolean;
    IsBinary: Boolean;
  end;

  { Set Variable flags }
  TWseVariableFlags = record
    AppendValue: Boolean;
    ConvertToLongPath: Boolean;
    ConvertToShortPath: Boolean;
    EnvironmentVariable: Boolean;
    Uppercase: Boolean;
    Lowercase: Boolean;
  end;

  { Check exists flags }
  TWseCheckExistsFlags = record
    CheckDirectory: Boolean;
    NegateResult: Boolean;
    CheckWriteAccess: Boolean;
  end;

  { Parse String operation types }
  TWseParseOp = (
    wpoSplitFirstPattern,    // Split at first occurrence of pattern
    wpoSplitLastPattern,     // Split at last occurrence of pattern
    wpoSplitPositionLeft,    // Split at position from left
    wpoSplitPositionRight,   // Split at position from right
    wpoExtractBetween,       // Extract between delimiters
    wpoExtractNumeric        // Extract numeric portion
  );

  { Parse String flags }
  TWseParseStringFlags = record
    Operation: TWseParseOp;
    TrimSpaces: Boolean;
    IgnoreCase: Boolean;
    IsPositionBased: Boolean;  // Pattern is a position number, not a string
  end;

  { Get System Info types }
  TWseSystemInfoType = (
    wsiWindowsVersion,       // Windows version string
    wsiWindowsMajor,         // Windows major version number
    wsiWindowsMinor,         // Windows minor version number
    wsiPhysicalMemoryKB,     // Physical memory in KB
    wsiDiskSpaceKB,          // Disk space in KB
    wsiScreenWidth,          // Screen width in pixels
    wsiScreenHeight,         // Screen height in pixels
    wsiColorDepth,           // Color depth in bits
    wsiProcessorType,        // Processor type
    wsiProcessorCount,       // Number of processors
    wsiIs64Bit,              // Is 64-bit OS
    wsiServicePack,          // Service pack string
    wsiComputerName,         // Computer name
    wsiUserName,             // Current user name
    wsiSystemDirectory,      // Windows system directory
    wsiUnknown               // Unknown/unsupported
  );

  { Delete file flags }
  TWseDeleteFlags = record
    RebootOnClose: Boolean;   // Delete on reboot if in use
    Recursive: Boolean;       // Recursive directory delete
    ConfirmDelete: Boolean;   // Prompt user before delete
  end;

  { Service operation flags }
  TWseServiceFlags = record
    IsStop: Boolean;          // Stop (vs Start)
    WaitForCompletion: Boolean;
    Timeout: Integer;         // Timeout in seconds
    IgnoreErrors: Boolean;
  end;

{ Flag parsing functions }
function ParseComparisonFlags(const AFlags: string): TWseComparisonOp;
function ParseMessageBoxTypeFlags(const AFlags: string): TWseMessageBoxType;
function ParseMessageBoxIconFlags(const AFlags: string): TWseMessageBoxIcon;
function ParseFileFlags(const AFlags: string): TWseFileFlags;
function ParseRegistryFlags(const AFlags: string): TWseRegistryFlags;
function ParseVariableFlags(const AFlags: string): TWseVariableFlags;
function ParseCheckExistsFlags(const AFlags: string): TWseCheckExistsFlags;
function ParseParseStringFlags(const AFlags: string): TWseParseStringFlags;
function ParseSystemInfoFlags(const AFlags: string): TWseSystemInfoType;
function ParseDeleteFlags(const AFlags: string): TWseDeleteFlags;
function ParseServiceFlags(const AFlags: string): TWseServiceFlags;

{ NSIS output conversion }
function ComparisonOpToNsis(AOp: TWseComparisonOp): string;
function MessageBoxTypeToNsis(AType: TWseMessageBoxType): string;
function MessageBoxIconToNsis(AIcon: TWseMessageBoxIcon): string;

{ Utility functions }
function GetFlagBit(const AFlags: string; ABitIndex: Integer): Boolean;
function GetFlagNibble(const AFlags: string; ANibbleIndex: Integer): Integer;
function FlagsToInteger(const AFlags: string): Int64;

implementation

{ Utility functions }

function GetFlagBit(const AFlags: string; ABitIndex: Integer): Boolean;
var
  CharIndex: Integer;
begin
  // Flags are stored as binary strings, rightmost bit is index 0
  // e.g., "00010101" - bit 0 is the rightmost '1'
  CharIndex := Length(AFlags) - ABitIndex;
  if (CharIndex >= 1) and (CharIndex <= Length(AFlags)) then
    Result := AFlags[CharIndex] = '1'
  else
    Result := False;
end;

function GetFlagNibble(const AFlags: string; ANibbleIndex: Integer): Integer;
var
  StartIndex, i: Integer;
  NibbleStr: string;
begin
  // Each nibble is 4 bits (characters)
  StartIndex := Length(AFlags) - ((ANibbleIndex + 1) * 4) + 1;

  if StartIndex < 1 then
  begin
    Result := 0;
    Exit;
  end;

  NibbleStr := '';
  for i := 0 to 3 do
  begin
    if (StartIndex + i >= 1) and (StartIndex + i <= Length(AFlags)) then
      NibbleStr := NibbleStr + AFlags[StartIndex + i]
    else
      NibbleStr := NibbleStr + '0';
  end;

  Result := 0;
  for i := 1 to 4 do
    if NibbleStr[i] = '1' then
      Result := Result or (1 shl (4 - i));
end;

function FlagsToInteger(const AFlags: string): Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(AFlags) do
  begin
    Result := Result shl 1;
    if AFlags[i] = '1' then
      Result := Result or 1;
  end;
end;

{ Flag parsing functions }

function ParseComparisonFlags(const AFlags: string): TWseComparisonOp;
var
  OpBits: Integer;
begin
  // Bits 0-2 determine comparison operator
  OpBits := 0;
  if GetFlagBit(AFlags, 0) then OpBits := OpBits or 1;
  if GetFlagBit(AFlags, 1) then OpBits := OpBits or 2;
  if GetFlagBit(AFlags, 3) then OpBits := OpBits or 4; // Bit 3 for contains/not contains

  case OpBits of
    0: Result := wcoEquals;           // 00000000 - equals
    1: Result := wcoNotEquals;        // 00000001 - not equals
    2: Result := wcoGreaterThan;      // 00000010 - greater than
    3: Result := wcoContains;         // 00000011 - contains
    4: Result := wcoLessThan;         // 00000100 - less than (inferred)
    5: Result := wcoFileExists;       // 00000101 - file exists
    8, 11: Result := wcoNotContains;  // 00001000/00001011 - not contains
  else
    Result := wcoEquals;
  end;
end;

function ParseMessageBoxTypeFlags(const AFlags: string): TWseMessageBoxType;
var
  TypeBits: Integer;
begin
  // Bits 0-1 determine button type
  TypeBits := 0;
  if GetFlagBit(AFlags, 0) then TypeBits := TypeBits or 1;
  if GetFlagBit(AFlags, 1) then TypeBits := TypeBits or 2;

  case TypeBits of
    0: Result := wmbOK;
    1: Result := wmbOKCancel;
    2: Result := wmbYesNo;
    3: Result := wmbYesNoCancel;
  else
    Result := wmbOK;
  end;
end;

function ParseMessageBoxIconFlags(const AFlags: string): TWseMessageBoxIcon;
var
  IconBits: Integer;
begin
  // Bits 4-5 determine icon type
  IconBits := 0;
  if GetFlagBit(AFlags, 4) then IconBits := IconBits or 1;
  if GetFlagBit(AFlags, 5) then IconBits := IconBits or 2;

  case IconBits of
    0: Result := wmiNone;
    1: Result := wmiInformation;
    2: Result := wmiError;
    3: Result := wmiWarning;
  else
    Result := wmiNone;
  end;
end;

function ParseFileFlags(const AFlags: string): TWseFileFlags;
begin
  // Parse file operation flags
  Result.Overwrite := GetFlagBit(AFlags, 1);
  Result.SelfRegister := GetFlagBit(AFlags, 2);
  Result.Shared := GetFlagBit(AFlags, 3);
  Result.Recursive := GetFlagBit(AFlags, 12);
  Result.CompressFiles := GetFlagBit(AFlags, 4);
  Result.NoUninstall := GetFlagBit(AFlags, 8);
  Result.ReplaceInUse := GetFlagBit(AFlags, 9);
  Result.CreateBackup := GetFlagBit(AFlags, 10);
end;

function ParseRegistryFlags(const AFlags: string): TWseRegistryFlags;
begin
  Result.CreateKey := not GetFlagBit(AFlags, 0);  // Bit 0 = delete key
  Result.DeleteKey := GetFlagBit(AFlags, 0);
  Result.DeleteValue := GetFlagBit(AFlags, 1);
  Result.IsDWORD := GetFlagBit(AFlags, 2);
  Result.IsExpandSz := GetFlagBit(AFlags, 3);
  Result.IsBinary := GetFlagBit(AFlags, 4);
end;

function ParseVariableFlags(const AFlags: string): TWseVariableFlags;
begin
  Result.AppendValue := GetFlagBit(AFlags, 0);
  Result.ConvertToLongPath := GetFlagBit(AFlags, 4);
  Result.ConvertToShortPath := GetFlagBit(AFlags, 5);
  Result.EnvironmentVariable := GetFlagBit(AFlags, 3);
  Result.Uppercase := GetFlagBit(AFlags, 6);
  Result.Lowercase := GetFlagBit(AFlags, 7);
end;

function ParseCheckExistsFlags(const AFlags: string): TWseCheckExistsFlags;
begin
  Result.CheckDirectory := GetFlagBit(AFlags, 2);
  Result.NegateResult := GetFlagBit(AFlags, 7);
  Result.CheckWriteAccess := GetFlagBit(AFlags, 4);
end;

{ NSIS output conversion }

function ComparisonOpToNsis(AOp: TWseComparisonOp): string;
begin
  case AOp of
    wcoEquals:        Result := '==';
    wcoNotEquals:     Result := '!=';
    wcoGreaterThan:   Result := '>';
    wcoContains:      Result := 'contains';  // Special handling needed
    wcoNotContains:   Result := '!contains'; // Special handling needed
    wcoLessThan:      Result := '<';
    wcoFileExists:    Result := 'FileExists';
    wcoFileNotExists: Result := '!FileExists';
  else
    Result := '==';
  end;
end;

function MessageBoxTypeToNsis(AType: TWseMessageBoxType): string;
begin
  case AType of
    wmbOK:              Result := 'MB_OK';
    wmbOKCancel:        Result := 'MB_OKCANCEL';
    wmbYesNo:           Result := 'MB_YESNO';
    wmbYesNoCancel:     Result := 'MB_YESNOCANCEL';
    wmbRetryCancel:     Result := 'MB_RETRYCANCEL';
    wmbAbortRetryIgnore: Result := 'MB_ABORTRETRYIGNORE';
  else
    Result := 'MB_OK';
  end;
end;

function MessageBoxIconToNsis(AIcon: TWseMessageBoxIcon): string;
begin
  case AIcon of
    wmiNone:        Result := '';
    wmiInformation: Result := 'MB_ICONINFORMATION';
    wmiWarning:     Result := 'MB_ICONEXCLAMATION';
    wmiError:       Result := 'MB_ICONSTOP';
    wmiQuestion:    Result := 'MB_ICONQUESTION';
  else
    Result := '';
  end;
end;

function ParseParseStringFlags(const AFlags: string): TWseParseStringFlags;
var
  OpBits: Integer;
begin
  // Default values
  Result.Operation := wpoSplitFirstPattern;
  Result.TrimSpaces := False;
  Result.IgnoreCase := False;
  Result.IsPositionBased := False;

  if AFlags = '' then
    Exit;

  // Bits 0-2: Operation type
  // Based on observed patterns from .wse files:
  // 00000000 = Split at first pattern
  // 00000001 = Split at last pattern
  // 00000010 = Split at position from left
  // 00000011 = Split at position from right
  // 00000100 = Extract between delimiters
  OpBits := 0;
  if GetFlagBit(AFlags, 0) then OpBits := OpBits or 1;
  if GetFlagBit(AFlags, 1) then OpBits := OpBits or 2;
  if GetFlagBit(AFlags, 2) then OpBits := OpBits or 4;

  case OpBits of
    0: Result.Operation := wpoSplitFirstPattern;
    1: Result.Operation := wpoSplitLastPattern;
    2: begin
         Result.Operation := wpoSplitPositionLeft;
         Result.IsPositionBased := True;
       end;
    3: begin
         Result.Operation := wpoSplitPositionRight;
         Result.IsPositionBased := True;
       end;
    4: Result.Operation := wpoExtractBetween;
    5: Result.Operation := wpoExtractNumeric;
  else
    Result.Operation := wpoSplitFirstPattern;
  end;

  // Bit 3: Trim spaces
  Result.TrimSpaces := GetFlagBit(AFlags, 3);

  // Bit 4: Ignore case
  Result.IgnoreCase := GetFlagBit(AFlags, 4);
end;

function ParseSystemInfoFlags(const AFlags: string): TWseSystemInfoType;
var
  TypeBits: Integer;
begin
  // Based on observed patterns and Wise documentation:
  // The flags field encodes what system information to retrieve
  // Bits 0-4 determine the information type

  if AFlags = '' then
  begin
    Result := wsiUnknown;
    Exit;
  end;

  TypeBits := 0;
  if GetFlagBit(AFlags, 0) then TypeBits := TypeBits or 1;
  if GetFlagBit(AFlags, 1) then TypeBits := TypeBits or 2;
  if GetFlagBit(AFlags, 2) then TypeBits := TypeBits or 4;
  if GetFlagBit(AFlags, 3) then TypeBits := TypeBits or 8;
  if GetFlagBit(AFlags, 4) then TypeBits := TypeBits or 16;

  // Mapping based on observed usage and Wise documentation
  case TypeBits of
    0:  Result := wsiWindowsVersion;      // Windows version string
    1:  Result := wsiWindowsMajor;        // Major version number
    2:  Result := wsiWindowsMinor;        // Minor version number
    3:  Result := wsiServicePack;         // Service pack string
    4:  Result := wsiPhysicalMemoryKB;    // Physical memory KB
    5:  Result := wsiDiskSpaceKB;         // Disk space KB
    6:  Result := wsiScreenWidth;         // Screen width
    7:  Result := wsiScreenHeight;        // Screen height
    8:  Result := wsiColorDepth;          // Color depth
    9:  Result := wsiProcessorType;       // Processor type
    10: Result := wsiProcessorCount;      // Processor count
    11: Result := wsiIs64Bit;             // Is 64-bit OS
    12: Result := wsiComputerName;        // Computer name
    13: Result := wsiUserName;            // User name
    14: Result := wsiSystemDirectory;     // System directory
  else
    Result := wsiUnknown;
  end;
end;

function ParseDeleteFlags(const AFlags: string): TWseDeleteFlags;
begin
  Result.RebootOnClose := False;
  Result.Recursive := False;
  Result.ConfirmDelete := False;

  if AFlags = '' then
    Exit;

  // Bit 0: Reboot on close (delete on reboot if file is in use)
  Result.RebootOnClose := GetFlagBit(AFlags, 0);

  // Bit 2: Recursive delete for directories
  Result.Recursive := GetFlagBit(AFlags, 2);

  // Bit 4: Confirm delete with user
  Result.ConfirmDelete := GetFlagBit(AFlags, 4);
end;

function ParseServiceFlags(const AFlags: string): TWseServiceFlags;
begin
  Result.IsStop := False;
  Result.WaitForCompletion := True;
  Result.Timeout := 30;
  Result.IgnoreErrors := False;

  if AFlags = '' then
    Exit;

  // Bit 0: Stop (1) vs Start (0)
  Result.IsStop := GetFlagBit(AFlags, 0);

  // Bit 1: Wait for completion
  Result.WaitForCompletion := not GetFlagBit(AFlags, 1);

  // Bit 2: Ignore errors
  Result.IgnoreErrors := GetFlagBit(AFlags, 2);

  // Bits 4-7: Timeout value (0-15, multiplied by 10 for seconds)
  Result.Timeout := GetFlagNibble(AFlags, 1) * 10;
  if Result.Timeout = 0 then
    Result.Timeout := 30;  // Default 30 seconds
end;

end.
