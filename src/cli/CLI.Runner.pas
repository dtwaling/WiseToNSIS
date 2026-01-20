unit CLI.Runner;

{******************************************************************************
  WiseToNSIS Converter - CLI Runner

  Main runner for command-line interface.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes,
  Conversion.Types, Converter.Engine, CLI.Options, CLI.Reporter;

type
  { CLI Runner }
  TCLIRunner = class
  private
    FOptions: TCLIOptions;
    FReporter: TCLIReporter;
    FEngine: TConverterEngine;
    FExitCode: Integer;

    procedure HandleProgress(Sender: TObject; APhase: TConversionPhase;
      AProgress: Integer; const AMessage: string);
    procedure HandleIssue(Sender: TObject; AIssue: TConversionIssue);

    function ConvertSingleFile(const AInputFile: string; const AOutputFile: string): Boolean;
    function ConvertBatch: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Run: Integer;

    property ExitCode: Integer read FExitCode;
  end;

{ Main entry point }
function RunCLI: Integer;

implementation

uses
  System.IOUtils, System.StrUtils;

function RunCLI: Integer;
var
  Runner: TCLIRunner;
begin
  Runner := TCLIRunner.Create;
  try
    Result := Runner.Run;
  finally
    Runner.Free;
  end;
end;

{ TCLIRunner }

constructor TCLIRunner.Create;
begin
  inherited Create;
  FOptions := TCLIOptions.Create;
  FReporter := TCLIReporter.Create;
  FEngine := TConverterEngine.Create;
  FExitCode := 0;

  FEngine.OnProgress := HandleProgress;
  FEngine.OnIssue := HandleIssue;
end;

destructor TCLIRunner.Destroy;
begin
  FEngine.Free;
  FReporter.Free;
  FOptions.Free;
  inherited Destroy;
end;

procedure TCLIRunner.HandleProgress(Sender: TObject; APhase: TConversionPhase;
  AProgress: Integer; const AMessage: string);
begin
  FReporter.Progress(AProgress, AMessage);
end;

procedure TCLIRunner.HandleIssue(Sender: TObject; AIssue: TConversionIssue);
begin
  if FReporter.OutputLevel = olVerbose then
    FReporter.ReportIssue(AIssue);
end;

function TCLIRunner.Run: Integer;
var
  InputFile, OutputFile: string;
begin
  // Parse command line
  if not FOptions.ParseCommandLine then
  begin
    FReporter.Error(FOptions.ParseError);
    FReporter.BlankLine;
    WriteLn(GetHelpText);
    Exit(1);
  end;

  // Handle help
  if FOptions.ShowHelp then
  begin
    WriteLn(GetHelpText);
    Exit(0);
  end;

  // Handle version
  if FOptions.ShowVersion then
  begin
    WriteLn(GetVersionText);
    Exit(0);
  end;

  // Set output level
  if FOptions.Quiet then
    FReporter.OutputLevel := olQuiet
  else if FOptions.Verbose then
    FReporter.OutputLevel := olVerbose
  else
    FReporter.OutputLevel := olNormal;

  // Apply options to engine
  FEngine.Options.OverwriteExisting := FOptions.Overwrite;
  FEngine.Options.GenerateUninstaller := not FOptions.NoUninstaller;
  FEngine.Options.GenerateComments := not FOptions.NoComments;
  FEngine.Options.MarkTodoItems := FOptions.NoPrompt;

  if FOptions.OutputDirectory <> '' then
    FEngine.Options.OutputDirectory := FOptions.OutputDirectory;

  // Run conversion
  if FOptions.BatchMode then
  begin
    if ConvertBatch then
      Result := 0
    else
      Result := 1;
  end
  else
  begin
    // Single file conversion
    InputFile := FOptions.InputFiles[0];
    OutputFile := FOptions.OutputFile;

    if OutputFile = '' then
      OutputFile := ChangeFileExt(InputFile, '.nsi');

    if FOptions.OutputDirectory <> '' then
      OutputFile := TPath.Combine(FOptions.OutputDirectory, ExtractFileName(OutputFile));

    if ConvertSingleFile(InputFile, OutputFile) then
      Result := 0
    else
      Result := 1;
  end;

  FExitCode := Result;
end;

function TCLIRunner.ConvertSingleFile(const AInputFile: string;
  const AOutputFile: string): Boolean;
begin
  FReporter.Info('Converting: ' + ExtractFileName(AInputFile));

  Result := FEngine.ConvertFile(AInputFile, AOutputFile);

  FReporter.BlankLine;

  // Report issues
  if FEngine.Issues.Count > 0 then
    FReporter.ReportIssues(FEngine.Issues);

  // Report statistics
  FReporter.ReportStats(FEngine.Stats);

  // Generate report if requested
  if FOptions.ReportFile <> '' then
    FReporter.GenerateReport(FOptions.ReportFile, FEngine, AInputFile);

  if Result then
    FReporter.Success('Conversion complete: ' + AOutputFile)
  else
    FReporter.Error('Conversion failed');
end;

function TCLIRunner.ConvertBatch: Boolean;
var
  i: Integer;
  InputFile, OutputFile, OutputDir: string;
  SuccessCount, FailCount: Integer;
begin
  SuccessCount := 0;
  FailCount := 0;
  OutputDir := FOptions.OutputDirectory;

  if OutputDir = '' then
    OutputDir := '.';

  TDirectory.CreateDirectory(OutputDir);

  FReporter.Info(Format('Batch converting %d files...', [FOptions.InputFiles.Count]));
  FReporter.BlankLine;

  for i := 0 to FOptions.InputFiles.Count - 1 do
  begin
    InputFile := FOptions.InputFiles[i];

    // Handle wildcards
    if (Pos('*', InputFile) > 0) or (Pos('?', InputFile) > 0) then
    begin
      // Expand wildcards
      var Dir := ExtractFilePath(InputFile);
      var Pattern := ExtractFileName(InputFile);
      if Dir = '' then
        Dir := '.';

      var Files := TDirectory.GetFiles(Dir, Pattern);
      for var F in Files do
      begin
        OutputFile := TPath.Combine(OutputDir, ChangeFileExt(ExtractFileName(F), '.nsi'));

        FReporter.Info(Format('[%d/%d] %s', [i + 1, FOptions.InputFiles.Count, ExtractFileName(F)]));

        if FEngine.ConvertFile(F, OutputFile) then
          Inc(SuccessCount)
        else
          Inc(FailCount);
      end;
    end
    else
    begin
      OutputFile := TPath.Combine(OutputDir, ChangeFileExt(ExtractFileName(InputFile), '.nsi'));

      FReporter.Info(Format('[%d/%d] %s', [i + 1, FOptions.InputFiles.Count, ExtractFileName(InputFile)]));

      if FEngine.ConvertFile(InputFile, OutputFile) then
        Inc(SuccessCount)
      else
        Inc(FailCount);
    end;
  end;

  FReporter.BlankLine;
  FReporter.Info(Format('Batch conversion complete: %d succeeded, %d failed',
    [SuccessCount, FailCount]));

  Result := (FailCount = 0);
end;

end.
