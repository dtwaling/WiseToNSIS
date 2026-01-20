unit Mapper.Tests;

{******************************************************************************
  WiseToNSIS Converter - Mapper Tests

  DUnit tests for the WSE to NSIS mapper.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes,
  TestFramework,
  Conversion.Types, WSE.AST, WSE.Parser, NSIS.Generator, NSIS.Mapper;

type
  { Mapper tests }
  TMapperTests = class(TTestCase)
  private
    FParser: TWseParser;
    FIssues: TConversionIssueList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function ConvertScript(const AScript: string): string;
  published
    procedure TestSetVariableMapping;
    procedure TestIfStatementMapping;
    procedure TestInstallFileMapping;
    procedure TestEditRegistryMapping;
    procedure TestDisplayMessageMapping;
    procedure TestServiceMapping;
    procedure TestVariableConversion;
    procedure TestCreateDirectoryMapping;
    procedure TestDeleteFileMapping;
    procedure TestExecuteProgramMapping;
  end;

implementation

uses
  System.StrUtils;

{ TMapperTests }

procedure TMapperTests.SetUp;
begin
  FParser := TWseParser.Create;
  FIssues := TConversionIssueList.Create(True);
end;

procedure TMapperTests.TearDown;
begin
  FIssues.Free;
  FParser.Free;
end;

function TMapperTests.ConvertScript(const AScript: string): string;
var
  Doc: TWseDocument;
  Mapper: TNsisMapperVisitor;
  Script: TNsisScript;
begin
  Result := '';
  Doc := FParser.ParseString(AScript, 'Test');
  try
    Mapper := TNsisMapperVisitor.Create(FIssues);
    try
      Script := Mapper.Convert(Doc);
      try
        Result := Script.Generate;
      finally
        Script.Free;
      end;
    finally
      Mapper.Free;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TMapperTests.TestSetVariableMapping;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Set Variable' + sLineBreak +
    '  Variable=MYVAR' + sLineBreak +
    '  Value=test value' + sLineBreak +
    'end';
var
  Output: string;
begin
  Output := ConvertScript(TestScript);
  Check(Pos('StrCpy $MYVAR "test value"', Output) > 0,
    'Output should contain StrCpy for variable');
  Check(Pos('Var MYVAR', Output) > 0,
    'Output should declare the variable');
end;

procedure TMapperTests.TestIfStatementMapping;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: If/While Statement' + sLineBreak +
    '  Variable=TESTVAR' + sLineBreak +
    '  Value=expected' + sLineBreak +
    '  Flags=00000000' + sLineBreak +
    'end' + sLineBreak +
    'item: Set Variable' + sLineBreak +
    '  Variable=INNER' + sLineBreak +
    '  Value=inside if' + sLineBreak +
    'end' + sLineBreak +
    'item: End Block' + sLineBreak +
    'end';
var
  Output: string;
begin
  Output := ConvertScript(TestScript);
  Check(Pos('${If}', Output) > 0, 'Output should contain ${If}');
  Check(Pos('${EndIf}', Output) > 0, 'Output should contain ${EndIf}');
  Check(Pos('$TESTVAR == "expected"', Output) > 0, 'Output should contain comparison');
end;

procedure TMapperTests.TestInstallFileMapping;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Install File' + sLineBreak +
    '  Source=C:\Build\app.exe' + sLineBreak +
    '  Destination=%MAINDIR%\bin\app.exe' + sLineBreak +
    '  Flags=0000000000000010' + sLineBreak +
    'end';
var
  Output: string;
begin
  Output := ConvertScript(TestScript);
  Check(Pos('SetOutPath "$INSTDIR\bin"', Output) > 0,
    'Output should contain SetOutPath');
  Check(Pos('File "C:\Build\app.exe"', Output) > 0,
    'Output should contain File command');
end;

procedure TMapperTests.TestEditRegistryMapping;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Edit Registry' + sLineBreak +
    '  Total Keys=1' + sLineBreak +
    '  Key=Software\BuilderMT\App' + sLineBreak +
    '  New Value=%MAINDIR%' + sLineBreak +
    '  Value Name=InstallPath' + sLineBreak +
    '  Root=2' + sLineBreak +
    'end';
var
  Output: string;
begin
  Output := ConvertScript(TestScript);
  Check(Pos('WriteRegStr HKLM "Software\BuilderMT\App" "InstallPath"', Output) > 0,
    'Output should contain WriteRegStr');
end;

procedure TMapperTests.TestDisplayMessageMapping;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Display Message' + sLineBreak +
    '  Title=Test' + sLineBreak +
    '  Text=Hello World' + sLineBreak +
    '  Flags=00000000' + sLineBreak +
    'end';
var
  Output: string;
begin
  Output := ConvertScript(TestScript);
  Check(Pos('MessageBox MB_OK "Hello World"', Output) > 0,
    'Output should contain MessageBox');
end;

procedure TMapperTests.TestServiceMapping;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Start/Stop Service' + sLineBreak +
    '  Service Name=MyService' + sLineBreak +
    '  Flags=00000000' + sLineBreak +
    'end';
var
  Output: string;
begin
  Output := ConvertScript(TestScript);
  Check(Pos('SimpleSC::StartService "MyService"', Output) > 0,
    'Output should contain SimpleSC::StartService');
end;

procedure TMapperTests.TestVariableConversion;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Set Variable' + sLineBreak +
    '  Variable=PATH' + sLineBreak +
    '  Value=%MAINDIR%\bin' + sLineBreak +
    'end';
var
  Output: string;
begin
  Output := ConvertScript(TestScript);
  // %MAINDIR% should be converted to $INSTDIR
  Check(Pos('$INSTDIR\bin', Output) > 0,
    'Output should convert %MAINDIR% to $INSTDIR');
end;

procedure TMapperTests.TestCreateDirectoryMapping;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Create Directory' + sLineBreak +
    '  Pathname=%MAINDIR%\logs' + sLineBreak +
    'end';
var
  Output: string;
begin
  Output := ConvertScript(TestScript);
  Check(Pos('CreateDirectory "$INSTDIR\logs"', Output) > 0,
    'Output should contain CreateDirectory');
end;

procedure TMapperTests.TestDeleteFileMapping;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Delete File' + sLineBreak +
    '  Pathname=%MAINDIR%\old.exe' + sLineBreak +
    'end';
var
  Output: string;
begin
  Output := ConvertScript(TestScript);
  Check(Pos('Delete "$INSTDIR\old.exe"', Output) > 0,
    'Output should contain Delete');
end;

procedure TMapperTests.TestExecuteProgramMapping;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Execute Program' + sLineBreak +
    '  Pathname=%MAINDIR%\setup.exe' + sLineBreak +
    '  Command Line=/silent' + sLineBreak +
    '  Flags=00000000' + sLineBreak +
    'end';
var
  Output: string;
begin
  Output := ConvertScript(TestScript);
  Check(Pos('ExecWait', Output) > 0, 'Output should contain ExecWait');
  Check(Pos('$INSTDIR\setup.exe', Output) > 0, 'Output should contain converted path');
end;

initialization
  RegisterTest(TMapperTests.Suite);

end.
