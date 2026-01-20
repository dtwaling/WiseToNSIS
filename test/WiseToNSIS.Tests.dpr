program WiseToNSIS.Tests;

{******************************************************************************
  WiseToNSIS Converter - Test Suite

  DUnit tests for the converter.
******************************************************************************}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  TestFramework,
  TextTestRunner,
  GUITestRunner,
  Conversion.Types in '..\src\common\Conversion.Types.pas',
  Conversion.Registry in '..\src\common\Conversion.Registry.pas',
  WSE.AST in '..\src\core\WSE.AST.pas',
  WSE.Parser in '..\src\core\WSE.Parser.pas',
  WSE.Flags in '..\src\core\WSE.Flags.pas',
  NSIS.Generator in '..\src\core\NSIS.Generator.pas',
  NSIS.Mapper in '..\src\core\NSIS.Mapper.pas',
  NSIS.Pages in '..\src\core\NSIS.Pages.pas',
  Converter.Engine in '..\src\core\Converter.Engine.pas',
  Parser.Tests in 'Parser.Tests.pas',
  Mapper.Tests in 'Mapper.Tests.pas';

{$R *.res}

begin
  {$IFDEF CONSOLE_TESTRUNNER}
  TextTestRunner.RunRegisteredTests.Free;
  {$ELSE}
  GUITestRunner.RunRegisteredTests;
  {$ENDIF}
end.
