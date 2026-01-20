unit CLI.Reporter;

{******************************************************************************
  WiseToNSIS Converter - CLI Reporter

  Console output formatting and report generation for CLI.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes,
  Conversion.Types, Converter.Engine;

type
  { Console output level }
  TOutputLevel = (olQuiet, olNormal, olVerbose);

  { CLI Reporter for console output }
  TCLIReporter = class
  private
    FOutputLevel: TOutputLevel;
    FUseColors: Boolean;
    FLastProgressLine: string;

    procedure WriteColored(const AText: string; AColor: Integer);
    procedure ClearProgressLine;
  public
    constructor Create;

    { Output methods }
    procedure Info(const AMessage: string);
    procedure Verbose(const AMessage: string);
    procedure Warning(const AMessage: string);
    procedure Error(const AMessage: string);
    procedure Success(const AMessage: string);
    procedure Progress(APercent: Integer; const AMessage: string);
    procedure BlankLine;

    { Issue reporting }
    procedure ReportIssue(AIssue: TConversionIssue);
    procedure ReportIssues(AIssues: TConversionIssueList);

    { Statistics reporting }
    procedure ReportStats(const AStats: TConversionStats);

    { Report generation }
    procedure GenerateReport(const AFilename: string; AEngine: TConverterEngine;
      const AInputFile: string);

    property OutputLevel: TOutputLevel read FOutputLevel write FOutputLevel;
    property UseColors: Boolean read FUseColors write FUseColors;
  end;

const
  { Console colors }
  COLOR_DEFAULT = 7;
  COLOR_RED = 12;
  COLOR_GREEN = 10;
  COLOR_YELLOW = 14;
  COLOR_CYAN = 11;
  COLOR_GRAY = 8;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.IOUtils;

{ TCLIReporter }

constructor TCLIReporter.Create;
begin
  inherited Create;
  FOutputLevel := olNormal;
  FUseColors := True;
  FLastProgressLine := '';
end;

procedure TCLIReporter.WriteColored(const AText: string; AColor: Integer);
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
  Info: TConsoleScreenBufferInfo;
  OldColor: Word;
begin
  if not FUseColors then
  begin
    Write(AText);
    Exit;
  end;

  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(Handle, Info);
  OldColor := Info.wAttributes;

  SetConsoleTextAttribute(Handle, AColor);
  Write(AText);
  SetConsoleTextAttribute(Handle, OldColor);
end;
{$ELSE}
begin
  Write(AText);
end;
{$ENDIF}

procedure TCLIReporter.ClearProgressLine;
begin
  if FLastProgressLine <> '' then
  begin
    Write(#13 + StringOfChar(' ', Length(FLastProgressLine)) + #13);
    FLastProgressLine := '';
  end;
end;

procedure TCLIReporter.Info(const AMessage: string);
begin
  if FOutputLevel = olQuiet then
    Exit;

  ClearProgressLine;
  WriteLn(AMessage);
end;

procedure TCLIReporter.Verbose(const AMessage: string);
begin
  if FOutputLevel <> olVerbose then
    Exit;

  ClearProgressLine;
  WriteColored('  ', COLOR_GRAY);
  WriteColored(AMessage, COLOR_GRAY);
  WriteLn;
end;

procedure TCLIReporter.Warning(const AMessage: string);
begin
  ClearProgressLine;
  WriteColored('[WARNING] ', COLOR_YELLOW);
  WriteLn(AMessage);
end;

procedure TCLIReporter.Error(const AMessage: string);
begin
  ClearProgressLine;
  WriteColored('[ERROR] ', COLOR_RED);
  WriteLn(AMessage);
end;

procedure TCLIReporter.Success(const AMessage: string);
begin
  ClearProgressLine;
  WriteColored('[OK] ', COLOR_GREEN);
  WriteLn(AMessage);
end;

procedure TCLIReporter.Progress(APercent: Integer; const AMessage: string);
var
  ProgressBar: string;
  BarWidth, FilledWidth: Integer;
begin
  if FOutputLevel = olQuiet then
    Exit;

  BarWidth := 20;
  FilledWidth := (APercent * BarWidth) div 100;
  ProgressBar := '[' + StringOfChar('#', FilledWidth) +
    StringOfChar('-', BarWidth - FilledWidth) + ']';

  FLastProgressLine := Format('%s %3d%% %s', [ProgressBar, APercent, AMessage]);
  Write(#13 + FLastProgressLine);
end;

procedure TCLIReporter.BlankLine;
begin
  if FOutputLevel = olQuiet then
    Exit;

  ClearProgressLine;
  WriteLn;
end;

procedure TCLIReporter.ReportIssue(AIssue: TConversionIssue);
var
  SeverityStr: string;
  Color: Integer;
begin
  case AIssue.Severity of
    csInfo:
      begin
        if FOutputLevel <> olVerbose then
          Exit;
        SeverityStr := 'INFO';
        Color := COLOR_CYAN;
      end;
    csWarning:
      begin
        SeverityStr := 'WARNING';
        Color := COLOR_YELLOW;
      end;
    csError:
      begin
        SeverityStr := 'ERROR';
        Color := COLOR_RED;
      end;
    csCritical:
      begin
        SeverityStr := 'CRITICAL';
        Color := COLOR_RED;
      end;
  else
    SeverityStr := 'UNKNOWN';
    Color := COLOR_DEFAULT;
  end;

  ClearProgressLine;
  WriteColored(Format('[%s] ', [SeverityStr]), Color);

  if AIssue.SourceLine > 0 then
    Write(Format('(%d:%d) ', [AIssue.SourceLine, AIssue.SourceColumn]));

  WriteLn(AIssue.Message);
end;

procedure TCLIReporter.ReportIssues(AIssues: TConversionIssueList);
var
  Issue: TConversionIssue;
begin
  for Issue in AIssues do
    ReportIssue(Issue);
end;

procedure TCLIReporter.ReportStats(const AStats: TConversionStats);
begin
  if FOutputLevel = olQuiet then
    Exit;

  ClearProgressLine;
  BlankLine;
  Info('Conversion Statistics:');
  Info(Format('  Total blocks:     %d', [AStats.TotalBlocks]));
  Info(Format('  Converted:        %d', [AStats.ConvertedBlocks]));
  Info(Format('  Partial:          %d', [AStats.PartialBlocks]));
  Info(Format('  Skipped:          %d', [AStats.SkippedBlocks]));
  Info(Format('  Failed:           %d', [AStats.FailedBlocks]));
  Info(Format('  Errors:           %d', [AStats.ErrorCount]));
  Info(Format('  Warnings:         %d', [AStats.WarningCount]));
  Info(Format('  Success rate:     %.1f%%', [AStats.SuccessRate]));
end;

procedure TCLIReporter.GenerateReport(const AFilename: string; AEngine: TConverterEngine;
  const AInputFile: string);
var
  Report: TStringList;
  Issue: TConversionIssue;
begin
  Report := TStringList.Create;
  try
    Report.Add('WiseToNSIS Conversion Report');
    Report.Add('============================');
    Report.Add('');
    Report.Add('Generated: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    Report.Add('Input file: ' + AInputFile);
    Report.Add('');

    Report.Add('Statistics:');
    Report.Add('-----------');
    Report.Add(Format('Total blocks:     %d', [AEngine.Stats.TotalBlocks]));
    Report.Add(Format('Converted:        %d', [AEngine.Stats.ConvertedBlocks]));
    Report.Add(Format('Partial:          %d', [AEngine.Stats.PartialBlocks]));
    Report.Add(Format('Skipped:          %d', [AEngine.Stats.SkippedBlocks]));
    Report.Add(Format('Failed:           %d', [AEngine.Stats.FailedBlocks]));
    Report.Add(Format('Errors:           %d', [AEngine.Stats.ErrorCount]));
    Report.Add(Format('Warnings:         %d', [AEngine.Stats.WarningCount]));
    Report.Add(Format('Success rate:     %.1f%%', [AEngine.Stats.SuccessRate]));
    Report.Add('');

    if AEngine.Issues.Count > 0 then
    begin
      Report.Add('Issues:');
      Report.Add('-------');

      for Issue in AEngine.Issues do
      begin
        Report.Add(Format('[%s] (%d:%d) %s',
          [SeverityToStr(Issue.Severity), Issue.SourceLine, Issue.SourceColumn, Issue.Message]));

        if Issue.SuggestedFix <> '' then
          Report.Add('  Suggested fix: ' + Issue.SuggestedFix);
      end;
    end;

    Report.SaveToFile(AFilename);

    Verbose('Report saved to: ' + AFilename);
  finally
    Report.Free;
  end;
end;

end.
