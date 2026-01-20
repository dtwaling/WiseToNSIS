unit CLI.Options;

{******************************************************************************
  WiseToNSIS Converter - CLI Option Parsing

  Parses command-line arguments for the CLI application.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  { CLI command options }
  TCLIOptions = class
  private
    FInputFiles: TStringList;
    FOutputFile: string;
    FOutputDirectory: string;
    FQuiet: Boolean;
    FVerbose: Boolean;
    FBatchMode: Boolean;
    FNoPrompt: Boolean;
    FReportFile: string;
    FShowHelp: Boolean;
    FShowVersion: Boolean;
    FOverwrite: Boolean;
    FNoUninstaller: Boolean;
    FNoComments: Boolean;

    FParseError: string;
    FParseSuccess: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Parse(const AArgs: array of string): Boolean;
    function ParseCommandLine: Boolean;

    property InputFiles: TStringList read FInputFiles;
    property OutputFile: string read FOutputFile;
    property OutputDirectory: string read FOutputDirectory;
    property Quiet: Boolean read FQuiet;
    property Verbose: Boolean read FVerbose;
    property BatchMode: Boolean read FBatchMode;
    property NoPrompt: Boolean read FNoPrompt;
    property ReportFile: string read FReportFile;
    property ShowHelp: Boolean read FShowHelp;
    property ShowVersion: Boolean read FShowVersion;
    property Overwrite: Boolean read FOverwrite;
    property NoUninstaller: Boolean read FNoUninstaller;
    property NoComments: Boolean read FNoComments;

    property ParseError: string read FParseError;
    property ParseSuccess: Boolean read FParseSuccess;
  end;

const
  VERSION_STRING = '1.0.0';
  PROGRAM_NAME = 'WiseToNSISCLI';

function GetHelpText: string;
function GetVersionText: string;

implementation

uses
  System.IOUtils;

function GetHelpText: string;
begin
  Result :=
    'WiseToNSIS Converter - Convert Wise Install scripts to NSIS' + sLineBreak +
    '' + sLineBreak +
    'Usage: ' + PROGRAM_NAME + ' [options] <input.wse> [output.nsi]' + sLineBreak +
    '' + sLineBreak +
    'Options:' + sLineBreak +
    '  -h, --help         Show this help message' + sLineBreak +
    '  -v, --version      Show version information' + sLineBreak +
    '  -q, --quiet        Suppress all output except errors' + sLineBreak +
    '  --verbose          Verbose output' + sLineBreak +
    '  -o, --output <dir> Output directory for generated files' + sLineBreak +
    '  -b, --batch        Batch mode (process multiple files)' + sLineBreak +
    '  --no-prompt        Skip ambiguous items (mark as TODO)' + sLineBreak +
    '  --report <file>    Generate conversion report' + sLineBreak +
    '  --overwrite        Overwrite existing output files' + sLineBreak +
    '  --no-uninstaller   Do not generate uninstaller section' + sLineBreak +
    '  --no-comments      Do not generate comments in output' + sLineBreak +
    '' + sLineBreak +
    'Examples:' + sLineBreak +
    '  ' + PROGRAM_NAME + ' Setup.wse' + sLineBreak +
    '      Convert Setup.wse to Setup.nsi in the same directory' + sLineBreak +
    '' + sLineBreak +
    '  ' + PROGRAM_NAME + ' Setup.wse Output.nsi' + sLineBreak +
    '      Convert Setup.wse to Output.nsi' + sLineBreak +
    '' + sLineBreak +
    '  ' + PROGRAM_NAME + ' -o output_dir -b *.wse' + sLineBreak +
    '      Batch convert all .wse files to output_dir' + sLineBreak +
    '' + sLineBreak +
    '  ' + PROGRAM_NAME + ' --verbose --report report.txt Setup.wse' + sLineBreak +
    '      Convert with verbose output and generate report';
end;

function GetVersionText: string;
begin
  Result := PROGRAM_NAME + ' version ' + VERSION_STRING + sLineBreak +
    'WiseToNSIS Converter - Wise Install to NSIS Script Converter' + sLineBreak +
    'Copyright (c) 2026 ECI Software Solutions';
end;

{ TCLIOptions }

constructor TCLIOptions.Create;
begin
  inherited Create;
  FInputFiles := TStringList.Create;
  FOutputFile := '';
  FOutputDirectory := '';
  FQuiet := False;
  FVerbose := False;
  FBatchMode := False;
  FNoPrompt := False;
  FReportFile := '';
  FShowHelp := False;
  FShowVersion := False;
  FOverwrite := False;
  FNoUninstaller := False;
  FNoComments := False;
  FParseError := '';
  FParseSuccess := False;
end;

destructor TCLIOptions.Destroy;
begin
  FInputFiles.Free;
  inherited Destroy;
end;

function TCLIOptions.Parse(const AArgs: array of string): Boolean;
var
  i: Integer;
  Arg, NextArg: string;
  ExpectOutput: Boolean;
  ExpectReport: Boolean;
begin
  FInputFiles.Clear;
  FParseError := '';
  ExpectOutput := False;
  ExpectReport := False;

  i := 0;
  while i <= High(AArgs) do
  begin
    Arg := AArgs[i];

    // Handle expected values from previous option
    if ExpectOutput then
    begin
      FOutputDirectory := Arg;
      ExpectOutput := False;
      Inc(i);
      Continue;
    end;

    if ExpectReport then
    begin
      FReportFile := Arg;
      ExpectReport := False;
      Inc(i);
      Continue;
    end;

    // Parse options
    if (Arg = '-h') or (Arg = '--help') then
      FShowHelp := True
    else if (Arg = '-v') or (Arg = '--version') then
      FShowVersion := True
    else if (Arg = '-q') or (Arg = '--quiet') then
      FQuiet := True
    else if Arg = '--verbose' then
      FVerbose := True
    else if (Arg = '-o') or (Arg = '--output') then
      ExpectOutput := True
    else if (Arg = '-b') or (Arg = '--batch') then
      FBatchMode := True
    else if Arg = '--no-prompt' then
      FNoPrompt := True
    else if Arg = '--report' then
      ExpectReport := True
    else if Arg = '--overwrite' then
      FOverwrite := True
    else if Arg = '--no-uninstaller' then
      FNoUninstaller := True
    else if Arg = '--no-comments' then
      FNoComments := True
    else if Arg.StartsWith('-') then
    begin
      FParseError := 'Unknown option: ' + Arg;
      FParseSuccess := False;
      Exit(False);
    end
    else
    begin
      // Input or output file
      if FInputFiles.Count = 0 then
        FInputFiles.Add(Arg)
      else if (FOutputFile = '') and not FBatchMode then
        FOutputFile := Arg
      else
        FInputFiles.Add(Arg);
    end;

    Inc(i);
  end;

  // Validate
  if ExpectOutput then
  begin
    FParseError := 'Output directory expected after -o/--output';
    FParseSuccess := False;
    Exit(False);
  end;

  if ExpectReport then
  begin
    FParseError := 'Report file expected after --report';
    FParseSuccess := False;
    Exit(False);
  end;

  // If no input files and not showing help/version, that's an error
  if (FInputFiles.Count = 0) and not FShowHelp and not FShowVersion then
  begin
    FParseError := 'No input file specified';
    FParseSuccess := False;
    Exit(False);
  end;

  FParseSuccess := True;
  Result := True;
end;

function TCLIOptions.ParseCommandLine: Boolean;
var
  Args: array of string;
  i: Integer;
begin
  SetLength(Args, ParamCount);
  for i := 1 to ParamCount do
    Args[i - 1] := ParamStr(i);

  Result := Parse(Args);
end;

end.
