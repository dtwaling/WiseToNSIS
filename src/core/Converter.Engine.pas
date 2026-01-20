unit Converter.Engine;

{******************************************************************************
  WiseToNSIS Converter - Main Conversion Engine

  Orchestrates the conversion process: parsing, mapping, and generation.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Conversion.Types, WSE.AST, WSE.Parser, NSIS.Generator, NSIS.Mapper;

type
  { Conversion result statistics }
  TConversionStats = record
    TotalBlocks: Integer;
    ConvertedBlocks: Integer;
    PartialBlocks: Integer;
    SkippedBlocks: Integer;
    FailedBlocks: Integer;
    ErrorCount: Integer;
    WarningCount: Integer;

    procedure Reset;
    function SuccessRate: Double;
  end;

  { Conversion options }
  TConverterOptions = class
  private
    FGenerateUninstaller: Boolean;
    FGenerateComments: Boolean;
    FMarkTodoItems: Boolean;
    FUseLogicLib: Boolean;
    FSimpleSCPlugin: Boolean;
    FOutputDirectory: string;
    FOverwriteExisting: Boolean;
    FVerboseOutput: Boolean;
  public
    constructor Create;

    property GenerateUninstaller: Boolean read FGenerateUninstaller write FGenerateUninstaller;
    property GenerateComments: Boolean read FGenerateComments write FGenerateComments;
    property MarkTodoItems: Boolean read FMarkTodoItems write FMarkTodoItems;
    property UseLogicLib: Boolean read FUseLogicLib write FUseLogicLib;
    property SimpleSCPlugin: Boolean read FSimpleSCPlugin write FSimpleSCPlugin;
    property OutputDirectory: string read FOutputDirectory write FOutputDirectory;
    property OverwriteExisting: Boolean read FOverwriteExisting write FOverwriteExisting;
    property VerboseOutput: Boolean read FVerboseOutput write FVerboseOutput;
  end;

  { Progress event types }
  TConversionPhase = (cpParsing, cpMapping, cpGenerating, cpComplete);

  TProgressEvent = procedure(Sender: TObject; APhase: TConversionPhase;
    AProgress: Integer; const AMessage: string) of object;

  TIssueEvent = procedure(Sender: TObject; AIssue: TConversionIssue) of object;

  { Main conversion engine }
  TConverterEngine = class
  private
    FOptions: TConverterOptions;
    FIssues: TConversionIssueList;
    FStats: TConversionStats;
    FDocument: TWseDocument;
    FScript: TNsisScript;

    FOnProgress: TProgressEvent;
    FOnIssue: TIssueEvent;

    procedure DoProgress(APhase: TConversionPhase; AProgress: Integer; const AMessage: string);
    procedure DoIssue(AIssue: TConversionIssue);
    procedure CalculateStats;
  public
    constructor Create;
    destructor Destroy; override;

    { Main conversion methods }
    function ConvertFile(const AInputFile: string; const AOutputFile: string = ''): Boolean;
    function ConvertString(const AContent: string; const ASourceName: string = ''): Boolean;

    { Output methods }
    function GetNsisScript: string;
    procedure SaveToFile(const AFilename: string);

    { Access to results }
    property Document: TWseDocument read FDocument;
    property Script: TNsisScript read FScript;
    property Issues: TConversionIssueList read FIssues;
    property Stats: TConversionStats read FStats;
    property Options: TConverterOptions read FOptions;

    { Events }
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnIssue: TIssueEvent read FOnIssue write FOnIssue;
  end;

  { Batch converter for multiple files }
  TBatchConverter = class
  private
    FEngine: TConverterEngine;
    FInputFiles: TStringList;
    FSuccessCount: Integer;
    FFailCount: Integer;

    FOnFileProgress: TProgressEvent;
    FOnFileComplete: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddFile(const AFilename: string);
    procedure AddFiles(const APattern: string);
    procedure ClearFiles;

    function ConvertAll(const AOutputDir: string): Boolean;

    property Engine: TConverterEngine read FEngine;
    property InputFiles: TStringList read FInputFiles;
    property SuccessCount: Integer read FSuccessCount;
    property FailCount: Integer read FFailCount;

    property OnFileProgress: TProgressEvent read FOnFileProgress write FOnFileProgress;
    property OnFileComplete: TNotifyEvent read FOnFileComplete write FOnFileComplete;
  end;

implementation

uses
  System.IOUtils, System.StrUtils;

{ TConversionStats }

procedure TConversionStats.Reset;
begin
  TotalBlocks := 0;
  ConvertedBlocks := 0;
  PartialBlocks := 0;
  SkippedBlocks := 0;
  FailedBlocks := 0;
  ErrorCount := 0;
  WarningCount := 0;
end;

function TConversionStats.SuccessRate: Double;
begin
  if TotalBlocks = 0 then
    Result := 0
  else
    Result := (ConvertedBlocks / TotalBlocks) * 100;
end;

{ TConverterOptions }

constructor TConverterOptions.Create;
begin
  inherited Create;
  FGenerateUninstaller := True;
  FGenerateComments := True;
  FMarkTodoItems := True;
  FUseLogicLib := True;
  FSimpleSCPlugin := True;
  FOutputDirectory := '';
  FOverwriteExisting := False;
  FVerboseOutput := False;
end;

{ TConverterEngine }

constructor TConverterEngine.Create;
begin
  inherited Create;
  FOptions := TConverterOptions.Create;
  FIssues := TConversionIssueList.Create(True);
  FStats.Reset;
  FDocument := nil;
  FScript := nil;
end;

destructor TConverterEngine.Destroy;
begin
  FOptions.Free;
  FIssues.Free;
  FDocument.Free;
  FScript.Free;
  inherited Destroy;
end;

procedure TConverterEngine.DoProgress(APhase: TConversionPhase; AProgress: Integer;
  const AMessage: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, APhase, AProgress, AMessage);
end;

procedure TConverterEngine.DoIssue(AIssue: TConversionIssue);
begin
  if Assigned(FOnIssue) then
    FOnIssue(Self, AIssue);
end;

procedure TConverterEngine.CalculateStats;

  procedure CountNode(ANode: TWseNode);
  var
    i: Integer;
  begin
    if ANode is TWseBlock then
    begin
      Inc(FStats.TotalBlocks);
      case TWseBlock(ANode).ConversionStatus of
        csConverted:    Inc(FStats.ConvertedBlocks);
        csPartial:      Inc(FStats.PartialBlocks);
        csSkipped:      Inc(FStats.SkippedBlocks);
        csFailed:       Inc(FStats.FailedBlocks);
      end;
    end;

    for i := 0 to ANode.ChildCount - 1 do
      CountNode(ANode.Children[i]);
  end;

begin
  FStats.Reset;

  if Assigned(FDocument) then
    CountNode(FDocument);

  FStats.ErrorCount := FIssues.ErrorCount;
  FStats.WarningCount := FIssues.WarningCount;
end;

function TConverterEngine.ConvertFile(const AInputFile: string;
  const AOutputFile: string): Boolean;
var
  Parser: TWseParser;
  Mapper: TNsisMapperVisitor;
  MapperOptions: TNsisMapperOptions;
  OutputPath: string;
begin
  Result := False;
  FIssues.Clear;
  FStats.Reset;

  // Free previous results
  FreeAndNil(FDocument);
  FreeAndNil(FScript);

  // Verify input file exists
  if not TFile.Exists(AInputFile) then
  begin
    FIssues.AddIssue(csCritical, icParsing, 'Input file not found: ' + AInputFile);
    Exit;
  end;

  try
    // Phase 1: Parsing
    DoProgress(cpParsing, 0, 'Parsing ' + ExtractFileName(AInputFile) + '...');

    Parser := TWseParser.Create;
    try
      FDocument := Parser.Parse(AInputFile);

      // Copy parser issues
      FIssues.AddRange(Parser.Issues.ToArray);

      DoProgress(cpParsing, 100, 'Parsing complete');
    finally
      Parser.Free;
    end;

    // Phase 2: Mapping
    DoProgress(cpMapping, 0, 'Converting to NSIS...');

    MapperOptions.GenerateUninstaller := FOptions.GenerateUninstaller;
    MapperOptions.GenerateComments := FOptions.GenerateComments;
    MapperOptions.MarkTodoItems := FOptions.MarkTodoItems;
    MapperOptions.UseLogicLib := FOptions.UseLogicLib;
    MapperOptions.SimpleSCPlugin := FOptions.SimpleSCPlugin;

    Mapper := TNsisMapperVisitor.Create(FIssues);
    try
      Mapper.Options := MapperOptions;
      FScript := Mapper.Convert(FDocument);

      DoProgress(cpMapping, 100, 'Conversion complete');
    finally
      Mapper.Free;
    end;

    // Phase 3: Generation (if output file specified)
    if AOutputFile <> '' then
    begin
      DoProgress(cpGenerating, 0, 'Generating NSIS script...');

      OutputPath := AOutputFile;
      if FOptions.OutputDirectory <> '' then
        OutputPath := TPath.Combine(FOptions.OutputDirectory, ExtractFileName(AOutputFile));

      // Check if output exists
      if TFile.Exists(OutputPath) and not FOptions.OverwriteExisting then
      begin
        FIssues.AddIssue(csError, icGeneration, 'Output file already exists: ' + OutputPath);
        Exit;
      end;

      FScript.SaveToFile(OutputPath);

      DoProgress(cpGenerating, 100, 'Script saved to ' + OutputPath);
    end;

    // Calculate statistics
    CalculateStats;

    DoProgress(cpComplete, 100, Format('Conversion complete: %d/%d blocks converted',
      [FStats.ConvertedBlocks, FStats.TotalBlocks]));

    Result := not FIssues.HasCritical;

  except
    on E: Exception do
    begin
      FIssues.AddIssue(csCritical, icParsing, 'Conversion failed: ' + E.Message);
      DoProgress(cpComplete, 0, 'Conversion failed');
    end;
  end;
end;

function TConverterEngine.ConvertString(const AContent: string;
  const ASourceName: string): Boolean;
var
  Parser: TWseParser;
  Mapper: TNsisMapperVisitor;
  MapperOptions: TNsisMapperOptions;
begin
  Result := False;
  FIssues.Clear;
  FStats.Reset;

  // Free previous results
  FreeAndNil(FDocument);
  FreeAndNil(FScript);

  try
    // Phase 1: Parsing
    DoProgress(cpParsing, 0, 'Parsing...');

    Parser := TWseParser.Create;
    try
      FDocument := Parser.ParseString(AContent, ASourceName);

      // Copy parser issues
      FIssues.AddRange(Parser.Issues.ToArray);

      DoProgress(cpParsing, 100, 'Parsing complete');
    finally
      Parser.Free;
    end;

    // Phase 2: Mapping
    DoProgress(cpMapping, 0, 'Converting to NSIS...');

    MapperOptions.GenerateUninstaller := FOptions.GenerateUninstaller;
    MapperOptions.GenerateComments := FOptions.GenerateComments;
    MapperOptions.MarkTodoItems := FOptions.MarkTodoItems;
    MapperOptions.UseLogicLib := FOptions.UseLogicLib;
    MapperOptions.SimpleSCPlugin := FOptions.SimpleSCPlugin;

    Mapper := TNsisMapperVisitor.Create(FIssues);
    try
      Mapper.Options := MapperOptions;
      FScript := Mapper.Convert(FDocument);

      DoProgress(cpMapping, 100, 'Conversion complete');
    finally
      Mapper.Free;
    end;

    // Calculate statistics
    CalculateStats;

    DoProgress(cpComplete, 100, Format('Conversion complete: %d/%d blocks converted',
      [FStats.ConvertedBlocks, FStats.TotalBlocks]));

    Result := not FIssues.HasCritical;

  except
    on E: Exception do
    begin
      FIssues.AddIssue(csCritical, icParsing, 'Conversion failed: ' + E.Message);
      DoProgress(cpComplete, 0, 'Conversion failed');
    end;
  end;
end;

function TConverterEngine.GetNsisScript: string;
begin
  if Assigned(FScript) then
    Result := FScript.Generate
  else
    Result := '';
end;

procedure TConverterEngine.SaveToFile(const AFilename: string);
begin
  if Assigned(FScript) then
    FScript.SaveToFile(AFilename);
end;

{ TBatchConverter }

constructor TBatchConverter.Create;
begin
  inherited Create;
  FEngine := TConverterEngine.Create;
  FInputFiles := TStringList.Create;
  FSuccessCount := 0;
  FFailCount := 0;
end;

destructor TBatchConverter.Destroy;
begin
  FEngine.Free;
  FInputFiles.Free;
  inherited Destroy;
end;

procedure TBatchConverter.AddFile(const AFilename: string);
begin
  if TFile.Exists(AFilename) then
    FInputFiles.Add(AFilename);
end;

procedure TBatchConverter.AddFiles(const APattern: string);
var
  Dir, Pattern: string;
  Files: TArray<string>;
  F: string;
begin
  Dir := ExtractFilePath(APattern);
  Pattern := ExtractFileName(APattern);

  if Dir = '' then
    Dir := '.';

  if TDirectory.Exists(Dir) then
  begin
    Files := TDirectory.GetFiles(Dir, Pattern);
    for F in Files do
      FInputFiles.Add(F);
  end;
end;

procedure TBatchConverter.ClearFiles;
begin
  FInputFiles.Clear;
end;

function TBatchConverter.ConvertAll(const AOutputDir: string): Boolean;
var
  i: Integer;
  InputFile, OutputFile: string;
begin
  FSuccessCount := 0;
  FFailCount := 0;

  // Ensure output directory exists
  if AOutputDir <> '' then
    TDirectory.CreateDirectory(AOutputDir);

  FEngine.Options.OutputDirectory := AOutputDir;
  FEngine.Options.OverwriteExisting := True;

  for i := 0 to FInputFiles.Count - 1 do
  begin
    InputFile := FInputFiles[i];
    OutputFile := ChangeFileExt(ExtractFileName(InputFile), '.nsi');

    if AOutputDir <> '' then
      OutputFile := TPath.Combine(AOutputDir, OutputFile);

    if Assigned(FOnFileProgress) then
      FOnFileProgress(Self, cpParsing, (i * 100) div FInputFiles.Count,
        Format('Converting %d/%d: %s', [i + 1, FInputFiles.Count, ExtractFileName(InputFile)]));

    if FEngine.ConvertFile(InputFile, OutputFile) then
      Inc(FSuccessCount)
    else
      Inc(FFailCount);

    if Assigned(FOnFileComplete) then
      FOnFileComplete(Self);
  end;

  Result := (FFailCount = 0);
end;

end.
