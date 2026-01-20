program WiseToNSISCLI;

{******************************************************************************
  WiseToNSIS Converter - Command Line Interface

  Converts Wise Install scripts (.wse) to NSIS scripts (.nsi/.nsh).

  Usage: WiseToNSISCLI [options] <input.wse> [output.nsi]
******************************************************************************}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Conversion.Types in 'src\common\Conversion.Types.pas',
  Conversion.Registry in 'src\common\Conversion.Registry.pas',
  WSE.AST in 'src\core\WSE.AST.pas',
  WSE.Parser in 'src\core\WSE.Parser.pas',
  WSE.Flags in 'src\core\WSE.Flags.pas',
  NSIS.Generator in 'src\core\NSIS.Generator.pas',
  NSIS.Mapper in 'src\core\NSIS.Mapper.pas',
  NSIS.Pages in 'src\core\NSIS.Pages.pas',
  Converter.Engine in 'src\core\Converter.Engine.pas',
  CLI.Options in 'src\cli\CLI.Options.pas',
  CLI.Reporter in 'src\cli\CLI.Reporter.pas',
  CLI.Runner in 'src\cli\CLI.Runner.pas';

var
  ExitCode: Integer;

begin
  try
    ExitCode := RunCLI;
    Halt(ExitCode);
  except
    on E: Exception do
    begin
      WriteLn('Fatal error: ', E.Message);
      Halt(2);
    end;
  end;
end.
