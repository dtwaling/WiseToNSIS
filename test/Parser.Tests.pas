unit Parser.Tests;

{******************************************************************************
  WiseToNSIS Converter - Parser Tests

  DUnit tests for the WSE parser.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes,
  TestFramework,
  Conversion.Types, WSE.AST, WSE.Parser;

type
  { Parser tests }
  TParserTests = class(TTestCase)
  private
    FParser: TWseParser;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestParseDocumentType;
    procedure TestParseGlobalBlock;
    procedure TestParseSetVariable;
    procedure TestParseIfStatement;
    procedure TestParseNestedBlocks;
    procedure TestParseRemarkedItem;
    procedure TestParseInstallFile;
    procedure TestParseEditRegistry;
    procedure TestParseDisplayMessage;
    procedure TestParseWizardBlock;
    procedure TestParseMultiLineText;
    procedure TestVariableExtraction;
  end;

implementation

{ TParserTests }

procedure TParserTests.SetUp;
begin
  FParser := TWseParser.Create;
end;

procedure TParserTests.TearDown;
begin
  FParser.Free;
end;

procedure TParserTests.TestParseDocumentType;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Global' + sLineBreak +
    '  Version=9.02' + sLineBreak +
    'end';
var
  Doc: TWseDocument;
begin
  Doc := FParser.ParseString(TestScript, 'Test');
  try
    CheckEquals('WSE', Doc.DocumentType, 'Document type should be WSE');
  finally
    Doc.Free;
  end;
end;

procedure TParserTests.TestParseGlobalBlock;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Global' + sLineBreak +
    '  Version=9.02' + sLineBreak +
    '  Title=Test Application' + sLineBreak +
    '  Flags=00010101' + sLineBreak +
    'end';
var
  Doc: TWseDocument;
  GlobalBlock: TWseBlock;
begin
  Doc := FParser.ParseString(TestScript, 'Test');
  try
    GlobalBlock := Doc.GetGlobalBlock;
    CheckNotNull(GlobalBlock, 'Global block should exist');
    Check(GlobalBlock is TWseGlobalBlock, 'Should be TWseGlobalBlock');
    CheckEquals('9.02', TWseGlobalBlock(GlobalBlock).Version, 'Version should match');
    CheckEquals('Test Application', TWseGlobalBlock(GlobalBlock).Title, 'Title should match');
  finally
    Doc.Free;
  end;
end;

procedure TParserTests.TestParseSetVariable;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Set Variable' + sLineBreak +
    '  Variable=MYVAR' + sLineBreak +
    '  Value=test value' + sLineBreak +
    '  Flags=10000000' + sLineBreak +
    'end';
var
  Doc: TWseDocument;
  Block: TWseBlock;
begin
  Doc := FParser.ParseString(TestScript, 'Test');
  try
    CheckEquals(1, Doc.ChildCount, 'Should have one child');
    Block := TWseBlock(Doc.Children[0]);
    Check(Block is TWseSetVariableBlock, 'Should be TWseSetVariableBlock');
    CheckEquals('MYVAR', TWseSetVariableBlock(Block).Variable, 'Variable name should match');
    CheckEquals('test value', TWseSetVariableBlock(Block).Value, 'Value should match');
  finally
    Doc.Free;
  end;
end;

procedure TParserTests.TestParseIfStatement;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: If/While Statement' + sLineBreak +
    '  Variable=TESTVAR' + sLineBreak +
    '  Value=expected' + sLineBreak +
    '  Flags=00000000' + sLineBreak +
    'end' + sLineBreak +
    'item: End Block' + sLineBreak +
    'end';
var
  Doc: TWseDocument;
  Block: TWseBlock;
begin
  Doc := FParser.ParseString(TestScript, 'Test');
  try
    CheckEquals(2, Doc.ChildCount, 'Should have two children');
    Block := TWseBlock(Doc.Children[0]);
    Check(Block is TWseIfWhileBlock, 'First should be TWseIfWhileBlock');
    CheckEquals('TESTVAR', TWseIfWhileBlock(Block).Variable, 'Variable should match');
    CheckEquals('expected', TWseIfWhileBlock(Block).Value, 'Value should match');
    CheckFalse(TWseIfWhileBlock(Block).IsWhileLoop, 'Should not be while loop');

    Block := TWseBlock(Doc.Children[1]);
    Check(Block is TWseEndBlock, 'Second should be TWseEndBlock');
  finally
    Doc.Free;
  end;
end;

procedure TParserTests.TestParseNestedBlocks;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Wizard Block' + sLineBreak +
    '  Direction Variable=DIRECTION' + sLineBreak +
    '  item: Custom Dialog Set' + sLineBreak +
    '    Name=Welcome' + sLineBreak +
    '    item: Dialog' + sLineBreak +
    '      Title=Welcome Dialog' + sLineBreak +
    '    end' + sLineBreak +
    '  end' + sLineBreak +
    'end';
var
  Doc: TWseDocument;
  WizardBlock, DialogSetBlock, DialogBlock: TWseBlock;
begin
  Doc := FParser.ParseString(TestScript, 'Test');
  try
    CheckEquals(1, Doc.ChildCount, 'Document should have one child');

    WizardBlock := TWseBlock(Doc.Children[0]);
    Check(WizardBlock is TWseWizardBlock, 'Should be TWseWizardBlock');
    CheckEquals(1, WizardBlock.ChildCount, 'Wizard block should have one child');

    DialogSetBlock := TWseBlock(WizardBlock.Children[0]);
    Check(DialogSetBlock is TWseCustomDialogBlock, 'Should be TWseCustomDialogBlock');
    CheckEquals('Welcome', TWseCustomDialogBlock(DialogSetBlock).Name, 'Dialog name should match');
    CheckEquals(1, DialogSetBlock.ChildCount, 'Dialog set should have one child');

    DialogBlock := TWseBlock(DialogSetBlock.Children[0]);
    Check(DialogBlock is TWseDialogBlock, 'Should be TWseDialogBlock');
    CheckEquals('Welcome Dialog', TWseDialogBlock(DialogBlock).Title, 'Dialog title should match');
  finally
    Doc.Free;
  end;
end;

procedure TParserTests.TestParseRemarkedItem;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'remarked item: Set Variable' + sLineBreak +
    '  Variable=COMMENTED' + sLineBreak +
    '  Value=hidden' + sLineBreak +
    'end';
var
  Doc: TWseDocument;
  Block: TWseBlock;
begin
  Doc := FParser.ParseString(TestScript, 'Test');
  try
    CheckEquals(1, Doc.ChildCount, 'Should have one child');
    Block := TWseBlock(Doc.Children[0]);
    CheckTrue(Block.IsRemarked, 'Block should be remarked');
  finally
    Doc.Free;
  end;
end;

procedure TParserTests.TestParseInstallFile;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Install File' + sLineBreak +
    '  Source=C:\Build\app.exe' + sLineBreak +
    '  Destination=%MAINDIR%\app.exe' + sLineBreak +
    '  Flags=0000000100000010' + sLineBreak +
    'end';
var
  Doc: TWseDocument;
  Block: TWseInstallFileBlock;
begin
  Doc := FParser.ParseString(TestScript, 'Test');
  try
    CheckEquals(1, Doc.ChildCount, 'Should have one child');
    Check(Doc.Children[0] is TWseInstallFileBlock, 'Should be TWseInstallFileBlock');
    Block := TWseInstallFileBlock(Doc.Children[0]);
    CheckEquals('C:\Build\app.exe', Block.Source, 'Source should match');
    CheckEquals('%MAINDIR%\app.exe', Block.Destination, 'Destination should match');
  finally
    Doc.Free;
  end;
end;

procedure TParserTests.TestParseEditRegistry;
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
  Doc: TWseDocument;
  Block: TWseEditRegistryBlock;
begin
  Doc := FParser.ParseString(TestScript, 'Test');
  try
    CheckEquals(1, Doc.ChildCount, 'Should have one child');
    Check(Doc.Children[0] is TWseEditRegistryBlock, 'Should be TWseEditRegistryBlock');
    Block := TWseEditRegistryBlock(Doc.Children[0]);
    CheckEquals('Software\BuilderMT\App', Block.Key, 'Key should match');
    CheckEquals('InstallPath', Block.ValueName, 'Value name should match');
    CheckEquals('%MAINDIR%', Block.NewValue, 'New value should match');
    CheckEquals(2, Block.Root, 'Root should be HKLM (2)');
    CheckEquals('HKLM', Block.GetNsisRootKey, 'NSIS root key should be HKLM');
  finally
    Doc.Free;
  end;
end;

procedure TParserTests.TestParseDisplayMessage;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Display Message' + sLineBreak +
    '  Title=Error' + sLineBreak +
    '  Text=First line' + sLineBreak +
    '  Text=Second line' + sLineBreak +
    '  Flags=00100000' + sLineBreak +
    'end';
var
  Doc: TWseDocument;
  Block: TWseDisplayMessageBlock;
begin
  Doc := FParser.ParseString(TestScript, 'Test');
  try
    CheckEquals(1, Doc.ChildCount, 'Should have one child');
    Check(Doc.Children[0] is TWseDisplayMessageBlock, 'Should be TWseDisplayMessageBlock');
    Block := TWseDisplayMessageBlock(Doc.Children[0]);
    CheckEquals('Error', Block.Title, 'Title should match');
    Check(Pos('First line', Block.Text) > 0, 'Text should contain first line');
    Check(Pos('Second line', Block.Text) > 0, 'Text should contain second line');
  finally
    Doc.Free;
  end;
end;

procedure TParserTests.TestParseWizardBlock;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Wizard Block' + sLineBreak +
    '  Direction Variable=DIRECTION' + sLineBreak +
    '  Display Variable=DISPLAY' + sLineBreak +
    '  Bitmap Pathname=C:\Images\header.bmp' + sLineBreak +
    'end';
var
  Doc: TWseDocument;
  Block: TWseWizardBlock;
begin
  Doc := FParser.ParseString(TestScript, 'Test');
  try
    CheckEquals(1, Doc.ChildCount, 'Should have one child');
    Check(Doc.Children[0] is TWseWizardBlock, 'Should be TWseWizardBlock');
    Block := TWseWizardBlock(Doc.Children[0]);
    CheckEquals('DIRECTION', Block.DirectionVariable, 'Direction variable should match');
    CheckEquals('DISPLAY', Block.DisplayVariable, 'Display variable should match');
    CheckEquals('C:\Images\header.bmp', Block.BitmapPathname, 'Bitmap pathname should match');
  finally
    Doc.Free;
  end;
end;

procedure TParserTests.TestParseMultiLineText;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Static' + sLineBreak +
    '  Rectangle=10 10 100 50' + sLineBreak +
    '  Text=Line one' + sLineBreak +
    '  Text=Line two' + sLineBreak +
    '  Text=Line three' + sLineBreak +
    'end';
var
  Doc: TWseDocument;
  Block: TWseStaticBlock;
begin
  Doc := FParser.ParseString(TestScript, 'Test');
  try
    CheckEquals(1, Doc.ChildCount, 'Should have one child');
    Check(Doc.Children[0] is TWseStaticBlock, 'Should be TWseStaticBlock');
    Block := TWseStaticBlock(Doc.Children[0]);
    // GetText returns all Text values joined
    Check(Pos('Line one', Block.Text) > 0, 'Text should contain Line one');
  finally
    Doc.Free;
  end;
end;

procedure TParserTests.TestVariableExtraction;
const
  TestScript =
    'Document Type: WSE' + sLineBreak +
    'item: Global' + sLineBreak +
    '  Version=9.02' + sLineBreak +
    '  Variable Name1=MAINDIR' + sLineBreak +
    '  Variable Default1=C:\Program Files\App' + sLineBreak +
    '  Variable Flags1=00001000' + sLineBreak +
    '  Variable Name2=CUSTOM' + sLineBreak +
    '  Variable Default2=default value' + sLineBreak +
    '  Variable Flags2=00000000' + sLineBreak +
    'end';
var
  Doc: TWseDocument;
  VarInfo: TVariableInfo;
begin
  Doc := FParser.ParseString(TestScript, 'Test');
  try
    CheckTrue(Doc.Variables.VariableExists('MAINDIR'), 'MAINDIR should exist');
    CheckTrue(Doc.Variables.VariableExists('CUSTOM'), 'CUSTOM should exist');

    VarInfo := Doc.Variables.GetVariable('MAINDIR');
    CheckNotNull(VarInfo, 'MAINDIR info should exist');
    CheckEquals('C:\Program Files\App', VarInfo.DefaultValue, 'MAINDIR default should match');

    VarInfo := Doc.Variables.GetVariable('CUSTOM');
    CheckNotNull(VarInfo, 'CUSTOM info should exist');
    CheckEquals('default value', VarInfo.DefaultValue, 'CUSTOM default should match');
  finally
    Doc.Free;
  end;
end;

initialization
  RegisterTest(TParserTests.Suite);

end.
