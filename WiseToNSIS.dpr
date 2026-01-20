program WiseToNSIS;

{******************************************************************************
  WiseToNSIS Converter - GUI Application

  Converts Wise Install scripts (.wse) to NSIS scripts (.nsi/.nsh).
******************************************************************************}

uses
  Vcl.Forms,
  Conversion.Types in 'src\common\Conversion.Types.pas',
  Conversion.Registry in 'src\common\Conversion.Registry.pas',
  WSE.AST in 'src\core\WSE.AST.pas',
  WSE.Parser in 'src\core\WSE.Parser.pas',
  WSE.Flags in 'src\core\WSE.Flags.pas',
  NSIS.Generator in 'src\core\NSIS.Generator.pas',
  NSIS.Mapper in 'src\core\NSIS.Mapper.pas',
  NSIS.Pages in 'src\core\NSIS.Pages.pas',
  Converter.Engine in 'src\core\Converter.Engine.pas',
  Main.Form in 'src\gui\Main.Form.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'WiseToNSIS Converter';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
