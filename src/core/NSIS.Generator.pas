unit NSIS.Generator;

{******************************************************************************
  WiseToNSIS Converter - NSIS Script Generator

  Generates NSIS script output from the converted AST.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Conversion.Types, WSE.AST;

type
  { Forward declarations }
  TNsisSection = class;
  TNsisFunction = class;

  { NSIS script builder }
  TNsisScript = class
  private
    FName: string;
    FOutFile: string;
    FInstallDir: string;
    FExecutionLevel: string;
    FUnicode: Boolean;

    FIncludes: TStringList;
    FDefines: TStringList;
    FVariables: TStringList;
    FPages: TStringList;
    FSections: TObjectList<TNsisSection>;
    FFunctions: TObjectList<TNsisFunction>;
    FLanguages: TStringList;
    FInitCode: TStringList;
    FCustomPageCode: TStringList;
    FUninstallSection: TNsisSection;

    function GetSection(Index: Integer): TNsisSection;
    function GetSectionCount: Integer;
    function GetFunction(Index: Integer): TNsisFunction;
    function GetFunctionCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    { Building methods }
    procedure AddInclude(const AInclude: string);
    procedure AddDefine(const AName: string; const AValue: string = '');
    procedure AddVariable(const AVarName: string);
    procedure AddPage(const APage: string);
    procedure AddLanguage(const ALanguage: string);
    procedure AddInitCode(const ACode: string);
    procedure AddCustomPageCode(const ACode: string);

    function CreateSection(const AName: string; const AId: string = ''): TNsisSection;
    function CreateFunction(const AName: string): TNsisFunction;

    { Output generation }
    function Generate: string;
    procedure SaveToFile(const AFilename: string);

    { Properties }
    property Name: string read FName write FName;
    property OutFile: string read FOutFile write FOutFile;
    property InstallDir: string read FInstallDir write FInstallDir;
    property ExecutionLevel: string read FExecutionLevel write FExecutionLevel;
    property Unicode: Boolean read FUnicode write FUnicode;

    property Sections[Index: Integer]: TNsisSection read GetSection;
    property SectionCount: Integer read GetSectionCount;
    property Functions[Index: Integer]: TNsisFunction read GetFunction;
    property FunctionCount: Integer read GetFunctionCount;

    property Includes: TStringList read FIncludes;
    property Defines: TStringList read FDefines;
    property Variables: TStringList read FVariables;
    property Pages: TStringList read FPages;
    property Languages: TStringList read FLanguages;
    property InitCode: TStringList read FInitCode;
    property CustomPageCode: TStringList read FCustomPageCode;
    property UninstallSection: TNsisSection read FUninstallSection;
  end;

  { NSIS section builder }
  TNsisSection = class
  private
    FName: string;
    FId: string;
    FCode: TStringList;
    FIndentLevel: Integer;
    FReadOnly: Boolean;
    FOptional: Boolean;
  public
    constructor Create(const AName: string; const AId: string = '');
    destructor Destroy; override;

    { Code building }
    procedure AddLine(const ALine: string);
    procedure AddLines(const ALines: array of string);
    procedure AddComment(const AComment: string);
    procedure AddBlankLine;
    procedure Indent;
    procedure Outdent;

    { Common NSIS commands }
    procedure SetOutPath(const APath: string);
    procedure AddFile(const ASource: string; ARecursive: Boolean = False);
    procedure WriteRegStr(const ARootKey, ASubKey, AValueName, AValue: string);
    procedure WriteRegDWORD(const ARootKey, ASubKey, AValueName: string; AValue: Integer);
    procedure ReadRegStr(const AVarName, ARootKey, ASubKey, AValueName: string);
    procedure CreateShortCut(const ALink, ATarget: string; const AParams: string = '';
      const AIcon: string = ''; AIconIndex: Integer = 0; const AWorkDir: string = '');
    procedure ExecWait(const ACommand: string);
    procedure Exec(const ACommand: string);
    procedure Delete(const APath: string; ARebootOK: Boolean = False);
    procedure RMDir(const APath: string; ARecursive: Boolean = False; ARebootOK: Boolean = False);
    procedure CreateDirectory(const APath: string);
    procedure MessageBox(const AFlags, AMessage: string);
    procedure DetailPrint(const AMessage: string);
    procedure WriteUninstaller(const APath: string);
    procedure CopyFiles(const ASource, ADest: string; ASilent: Boolean = True);

    { Output }
    function Generate: string;

    { Properties }
    property Name: string read FName write FName;
    property Id: string read FId write FId;
    property Code: TStringList read FCode;
    property IndentLevel: Integer read FIndentLevel write FIndentLevel;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Optional: Boolean read FOptional write FOptional;
  end;

  { NSIS function builder }
  TNsisFunction = class
  private
    FName: string;
    FCode: TStringList;
    FIndentLevel: Integer;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    { Code building }
    procedure AddLine(const ALine: string);
    procedure AddLines(const ALines: array of string);
    procedure AddComment(const AComment: string);
    procedure AddBlankLine;
    procedure Indent;
    procedure Outdent;

    { Output }
    function Generate: string;

    { Properties }
    property Name: string read FName write FName;
    property Code: TStringList read FCode;
    property IndentLevel: Integer read FIndentLevel write FIndentLevel;
  end;

  { Code builder helper for generating NSIS code blocks }
  TNsisCodeBuilder = class
  private
    FLines: TStringList;
    FIndentLevel: Integer;
    FIndentString: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetIndentString(const AIndent: string);
    procedure AddLine(const ALine: string);
    procedure AddLines(const ALines: array of string);
    procedure AddComment(const AComment: string);
    procedure AddBlankLine;
    procedure Indent;
    procedure Outdent;
    procedure Clear;

    function GetCode: string;

    property Lines: TStringList read FLines;
    property IndentLevel: Integer read FIndentLevel write FIndentLevel;
  end;

{ Helper functions }
function EscapeNsisString(const AValue: string): string;
function ConvertWisePathToNsis(const AWisePath: string): string;
function ConvertWiseVarReferences(const AText: string): string;

implementation

uses
  System.StrUtils, System.RegularExpressions;

{ Helper functions }

function EscapeNsisString(const AValue: string): string;
begin
  Result := AValue;
  // Escape special characters for NSIS strings
  Result := StringReplace(Result, '$', '$$', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '$\"', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '$\r$\n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '$\n', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '$\t', [rfReplaceAll]);
end;

function ConvertWisePathToNsis(const AWisePath: string): string;
begin
  Result := AWisePath;
  // Convert backslashes (Windows uses either)
  Result := StringReplace(Result, '/', '\', [rfReplaceAll]);
  // Convert variable references
  Result := ConvertWiseVarReferences(Result);
end;

function ConvertWiseVarReferences(const AText: string): string;
var
  Regex: TRegEx;
  Match: TMatch;
  VarName, NsisVar: string;
begin
  Result := AText;

  // Match %VARNAME% pattern
  Regex := TRegEx.Create('%([A-Za-z_][A-Za-z0-9_]*)%');
  Match := Regex.Match(Result);

  while Match.Success do
  begin
    VarName := Match.Groups[1].Value;
    NsisVar := ConvertWiseVariable('%' + VarName + '%');
    Result := StringReplace(Result, Match.Value, NsisVar, []);
    Match := Regex.Match(Result);
  end;
end;

{ TNsisScript }

constructor TNsisScript.Create;
begin
  inherited Create;
  FName := '';
  FOutFile := '';
  FInstallDir := '';
  FExecutionLevel := 'admin';
  FUnicode := True;

  FIncludes := TStringList.Create;
  FDefines := TStringList.Create;
  FVariables := TStringList.Create;
  FPages := TStringList.Create;
  FSections := TObjectList<TNsisSection>.Create(True);
  FFunctions := TObjectList<TNsisFunction>.Create(True);
  FLanguages := TStringList.Create;
  FInitCode := TStringList.Create;
  FCustomPageCode := TStringList.Create;
  FUninstallSection := TNsisSection.Create('Uninstall', '');
end;

destructor TNsisScript.Destroy;
begin
  FIncludes.Free;
  FDefines.Free;
  FVariables.Free;
  FPages.Free;
  FSections.Free;
  FFunctions.Free;
  FLanguages.Free;
  FInitCode.Free;
  FCustomPageCode.Free;
  FUninstallSection.Free;
  inherited Destroy;
end;

procedure TNsisScript.AddInclude(const AInclude: string);
begin
  if FIncludes.IndexOf(AInclude) < 0 then
    FIncludes.Add(AInclude);
end;

procedure TNsisScript.AddDefine(const AName: string; const AValue: string);
begin
  if AValue = '' then
    FDefines.Add(AName + '=')
  else
    FDefines.Add(AName + '=' + AValue);
end;

procedure TNsisScript.AddVariable(const AVarName: string);
begin
  if FVariables.IndexOf(AVarName) < 0 then
    FVariables.Add(AVarName);
end;

procedure TNsisScript.AddPage(const APage: string);
begin
  FPages.Add(APage);
end;

procedure TNsisScript.AddLanguage(const ALanguage: string);
begin
  if FLanguages.IndexOf(ALanguage) < 0 then
    FLanguages.Add(ALanguage);
end;

procedure TNsisScript.AddInitCode(const ACode: string);
begin
  FInitCode.Add(ACode);
end;

procedure TNsisScript.AddCustomPageCode(const ACode: string);
begin
  FCustomPageCode.Add(ACode);
end;

function TNsisScript.CreateSection(const AName: string; const AId: string): TNsisSection;
begin
  Result := TNsisSection.Create(AName, AId);
  FSections.Add(Result);
end;

function TNsisScript.CreateFunction(const AName: string): TNsisFunction;
begin
  Result := TNsisFunction.Create(AName);
  FFunctions.Add(Result);
end;

function TNsisScript.GetSection(Index: Integer): TNsisSection;
begin
  Result := FSections[Index];
end;

function TNsisScript.GetSectionCount: Integer;
begin
  Result := FSections.Count;
end;

function TNsisScript.GetFunction(Index: Integer): TNsisFunction;
begin
  Result := FFunctions[Index];
end;

function TNsisScript.GetFunctionCount: Integer;
begin
  Result := FFunctions.Count;
end;

function TNsisScript.Generate: string;
var
  Output: TStringList;
  i: Integer;
  DefName, DefValue: string;
  OnInitFunc: TNsisFunction;
begin
  Output := TStringList.Create;
  try
    // Header comment
    Output.Add(';--------------------------------------------------');
    Output.Add('; ' + FName);
    Output.Add('; Generated by WiseToNSIS Converter');
    Output.Add('; ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
    Output.Add(';--------------------------------------------------');
    Output.Add('');

    // Unicode setting
    if FUnicode then
      Output.Add('Unicode True');
    Output.Add('');

    // Includes
    if FIncludes.Count > 0 then
    begin
      Output.Add('; Includes');
      for i := 0 to FIncludes.Count - 1 do
        Output.Add('!include "' + FIncludes[i] + '"');
      Output.Add('');
    end;

    // Defines
    if FDefines.Count > 0 then
    begin
      Output.Add('; Defines');
      for i := 0 to FDefines.Count - 1 do
      begin
        DefName := FDefines.Names[i];
        DefValue := FDefines.ValueFromIndex[i];
        if DefValue = '' then
          Output.Add('!define ' + DefName)
        else
          Output.Add('!define ' + DefName + ' "' + DefValue + '"');
      end;
      Output.Add('');
    end;

    // Metadata
    Output.Add('; Metadata');
    if FName <> '' then
      Output.Add('Name "' + FName + '"');
    if FOutFile <> '' then
      Output.Add('OutFile "' + FOutFile + '"');
    if FInstallDir <> '' then
      Output.Add('InstallDir "' + FInstallDir + '"');
    if FExecutionLevel <> '' then
      Output.Add('RequestExecutionLevel ' + FExecutionLevel);
    Output.Add('');

    // Variables
    if FVariables.Count > 0 then
    begin
      Output.Add('; Variables');
      for i := 0 to FVariables.Count - 1 do
        Output.Add('Var ' + FVariables[i]);
      Output.Add('');
    end;

    // MUI configuration
    Output.Add('; Modern UI Configuration');
    Output.Add('!define MUI_ABORTWARNING');
    Output.Add('');

    // Pages
    if FPages.Count > 0 then
    begin
      Output.Add('; Pages');
      for i := 0 to FPages.Count - 1 do
        Output.Add(FPages[i]);
      Output.Add('');
    end
    else
    begin
      // Default pages
      Output.Add('; Pages');
      Output.Add('!insertmacro MUI_PAGE_WELCOME');
      Output.Add('!insertmacro MUI_PAGE_DIRECTORY');
      Output.Add('!insertmacro MUI_PAGE_INSTFILES');
      Output.Add('!insertmacro MUI_PAGE_FINISH');
      Output.Add('');
      Output.Add('!insertmacro MUI_UNPAGE_CONFIRM');
      Output.Add('!insertmacro MUI_UNPAGE_INSTFILES');
      Output.Add('');
    end;

    // Languages
    if FLanguages.Count > 0 then
    begin
      Output.Add('; Languages');
      for i := 0 to FLanguages.Count - 1 do
        Output.Add('!insertmacro MUI_LANGUAGE "' + FLanguages[i] + '"');
      Output.Add('');
    end
    else
    begin
      Output.Add('; Languages');
      Output.Add('!insertmacro MUI_LANGUAGE "English"');
      Output.Add('');
    end;

    // .onInit function
    if FInitCode.Count > 0 then
    begin
      Output.Add(';--------------------------------------------------');
      Output.Add('; Initialization');
      Output.Add(';--------------------------------------------------');
      Output.Add('Function .onInit');
      for i := 0 to FInitCode.Count - 1 do
        Output.Add('  ' + FInitCode[i]);
      Output.Add('FunctionEnd');
      Output.Add('');
    end;

    // Functions
    for i := 0 to FFunctions.Count - 1 do
    begin
      Output.Add(FFunctions[i].Generate);
      Output.Add('');
    end;

    // Custom page code (nsDialogs pages from Custom Dialogs)
    if FCustomPageCode.Count > 0 then
    begin
      Output.Add(';--------------------------------------------------');
      Output.Add('; Custom Page Functions');
      Output.Add(';--------------------------------------------------');
      for i := 0 to FCustomPageCode.Count - 1 do
        Output.Add(FCustomPageCode[i]);
      Output.Add('');
    end;

    // Sections
    Output.Add(';--------------------------------------------------');
    Output.Add('; Installation Sections');
    Output.Add(';--------------------------------------------------');
    for i := 0 to FSections.Count - 1 do
    begin
      Output.Add(FSections[i].Generate);
      Output.Add('');
    end;

    // Uninstall section
    if FUninstallSection.Code.Count > 0 then
    begin
      Output.Add(';--------------------------------------------------');
      Output.Add('; Uninstaller Section');
      Output.Add(';--------------------------------------------------');
      Output.Add(FUninstallSection.Generate);
    end;

    Result := Output.Text;
  finally
    Output.Free;
  end;
end;

procedure TNsisScript.SaveToFile(const AFilename: string);
var
  Output: TStringList;
begin
  Output := TStringList.Create;
  try
    Output.Text := Generate;
    Output.SaveToFile(AFilename);
  finally
    Output.Free;
  end;
end;

{ TNsisSection }

constructor TNsisSection.Create(const AName: string; const AId: string);
begin
  inherited Create;
  FName := AName;
  FId := AId;
  FCode := TStringList.Create;
  FIndentLevel := 0;
  FReadOnly := False;
  FOptional := False;
end;

destructor TNsisSection.Destroy;
begin
  FCode.Free;
  inherited Destroy;
end;

procedure TNsisSection.AddLine(const ALine: string);
begin
  FCode.Add(StringOfChar(' ', FIndentLevel * 2) + ALine);
end;

procedure TNsisSection.AddLines(const ALines: array of string);
var
  i: Integer;
begin
  for i := Low(ALines) to High(ALines) do
    AddLine(ALines[i]);
end;

procedure TNsisSection.AddComment(const AComment: string);
begin
  AddLine('; ' + AComment);
end;

procedure TNsisSection.AddBlankLine;
begin
  FCode.Add('');
end;

procedure TNsisSection.Indent;
begin
  Inc(FIndentLevel);
end;

procedure TNsisSection.Outdent;
begin
  if FIndentLevel > 0 then
    Dec(FIndentLevel);
end;

procedure TNsisSection.SetOutPath(const APath: string);
begin
  AddLine('SetOutPath "' + ConvertWisePathToNsis(APath) + '"');
end;

procedure TNsisSection.AddFile(const ASource: string; ARecursive: Boolean);
begin
  if ARecursive then
    AddLine('File /r "' + ConvertWisePathToNsis(ASource) + '"')
  else
    AddLine('File "' + ConvertWisePathToNsis(ASource) + '"');
end;

procedure TNsisSection.WriteRegStr(const ARootKey, ASubKey, AValueName, AValue: string);
begin
  AddLine(Format('WriteRegStr %s "%s" "%s" "%s"',
    [ARootKey, ASubKey, AValueName, ConvertWiseVarReferences(AValue)]));
end;

procedure TNsisSection.WriteRegDWORD(const ARootKey, ASubKey, AValueName: string; AValue: Integer);
begin
  AddLine(Format('WriteRegDWORD %s "%s" "%s" %d',
    [ARootKey, ASubKey, AValueName, AValue]));
end;

procedure TNsisSection.ReadRegStr(const AVarName, ARootKey, ASubKey, AValueName: string);
begin
  AddLine(Format('ReadRegStr %s %s "%s" "%s"',
    [AVarName, ARootKey, ASubKey, AValueName]));
end;

procedure TNsisSection.CreateShortCut(const ALink, ATarget: string; const AParams: string;
  const AIcon: string; AIconIndex: Integer; const AWorkDir: string);
var
  Cmd: string;
begin
  Cmd := Format('CreateShortCut "%s" "%s"',
    [ConvertWisePathToNsis(ALink), ConvertWisePathToNsis(ATarget)]);

  if AParams <> '' then
    Cmd := Cmd + ' "' + AParams + '"';

  if AIcon <> '' then
  begin
    if AParams = '' then
      Cmd := Cmd + ' ""';
    Cmd := Cmd + ' "' + ConvertWisePathToNsis(AIcon) + '" ' + IntToStr(AIconIndex);
  end;

  AddLine(Cmd);
end;

procedure TNsisSection.ExecWait(const ACommand: string);
begin
  AddLine('ExecWait "' + ConvertWiseVarReferences(ACommand) + '"');
end;

procedure TNsisSection.Exec(const ACommand: string);
begin
  AddLine('Exec "' + ConvertWiseVarReferences(ACommand) + '"');
end;

procedure TNsisSection.Delete(const APath: string; ARebootOK: Boolean);
begin
  if ARebootOK then
    AddLine('Delete /REBOOTOK "' + ConvertWisePathToNsis(APath) + '"')
  else
    AddLine('Delete "' + ConvertWisePathToNsis(APath) + '"');
end;

procedure TNsisSection.RMDir(const APath: string; ARecursive: Boolean; ARebootOK: Boolean);
var
  Flags: string;
begin
  Flags := '';
  if ARecursive then
    Flags := Flags + '/r ';
  if ARebootOK then
    Flags := Flags + '/REBOOTOK ';

  AddLine('RMDir ' + Flags + '"' + ConvertWisePathToNsis(APath) + '"');
end;

procedure TNsisSection.CreateDirectory(const APath: string);
begin
  AddLine('CreateDirectory "' + ConvertWisePathToNsis(APath) + '"');
end;

procedure TNsisSection.MessageBox(const AFlags, AMessage: string);
begin
  AddLine('MessageBox ' + AFlags + ' "' + EscapeNsisString(ConvertWiseVarReferences(AMessage)) + '"');
end;

procedure TNsisSection.DetailPrint(const AMessage: string);
begin
  AddLine('DetailPrint "' + EscapeNsisString(ConvertWiseVarReferences(AMessage)) + '"');
end;

procedure TNsisSection.WriteUninstaller(const APath: string);
begin
  AddLine('WriteUninstaller "' + ConvertWisePathToNsis(APath) + '"');
end;

procedure TNsisSection.CopyFiles(const ASource, ADest: string; ASilent: Boolean);
begin
  if ASilent then
    AddLine('CopyFiles /SILENT "' + ConvertWisePathToNsis(ASource) + '" "' + ConvertWisePathToNsis(ADest) + '"')
  else
    AddLine('CopyFiles "' + ConvertWisePathToNsis(ASource) + '" "' + ConvertWisePathToNsis(ADest) + '"');
end;

function TNsisSection.Generate: string;
var
  Output: TStringList;
  i: Integer;
  SectionDecl: string;
begin
  Output := TStringList.Create;
  try
    // Section declaration
    if FOptional then
      SectionDecl := 'Section /o "' + FName + '"'
    else
      SectionDecl := 'Section "' + FName + '"';

    if FId <> '' then
      SectionDecl := SectionDecl + ' ' + FId;

    Output.Add(SectionDecl);

    // Read-only flag
    if FReadOnly then
      Output.Add('  SectionIn RO');

    // Section code
    for i := 0 to FCode.Count - 1 do
      Output.Add('  ' + FCode[i]);

    Output.Add('SectionEnd');

    Result := Output.Text;
  finally
    Output.Free;
  end;
end;

{ TNsisFunction }

constructor TNsisFunction.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FCode := TStringList.Create;
  FIndentLevel := 0;
end;

destructor TNsisFunction.Destroy;
begin
  FCode.Free;
  inherited Destroy;
end;

procedure TNsisFunction.AddLine(const ALine: string);
begin
  FCode.Add(StringOfChar(' ', FIndentLevel * 2) + ALine);
end;

procedure TNsisFunction.AddLines(const ALines: array of string);
var
  i: Integer;
begin
  for i := Low(ALines) to High(ALines) do
    AddLine(ALines[i]);
end;

procedure TNsisFunction.AddComment(const AComment: string);
begin
  AddLine('; ' + AComment);
end;

procedure TNsisFunction.AddBlankLine;
begin
  FCode.Add('');
end;

procedure TNsisFunction.Indent;
begin
  Inc(FIndentLevel);
end;

procedure TNsisFunction.Outdent;
begin
  if FIndentLevel > 0 then
    Dec(FIndentLevel);
end;

function TNsisFunction.Generate: string;
var
  Output: TStringList;
  i: Integer;
begin
  Output := TStringList.Create;
  try
    Output.Add('Function ' + FName);

    for i := 0 to FCode.Count - 1 do
      Output.Add('  ' + FCode[i]);

    Output.Add('FunctionEnd');

    Result := Output.Text;
  finally
    Output.Free;
  end;
end;

{ TNsisCodeBuilder }

constructor TNsisCodeBuilder.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
  FIndentLevel := 0;
  FIndentString := '  ';
end;

destructor TNsisCodeBuilder.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TNsisCodeBuilder.SetIndentString(const AIndent: string);
begin
  FIndentString := AIndent;
end;

procedure TNsisCodeBuilder.AddLine(const ALine: string);
var
  Prefix: string;
  i: Integer;
begin
  Prefix := '';
  for i := 1 to FIndentLevel do
    Prefix := Prefix + FIndentString;
  FLines.Add(Prefix + ALine);
end;

procedure TNsisCodeBuilder.AddLines(const ALines: array of string);
var
  i: Integer;
begin
  for i := Low(ALines) to High(ALines) do
    AddLine(ALines[i]);
end;

procedure TNsisCodeBuilder.AddComment(const AComment: string);
begin
  AddLine('; ' + AComment);
end;

procedure TNsisCodeBuilder.AddBlankLine;
begin
  FLines.Add('');
end;

procedure TNsisCodeBuilder.Indent;
begin
  Inc(FIndentLevel);
end;

procedure TNsisCodeBuilder.Outdent;
begin
  if FIndentLevel > 0 then
    Dec(FIndentLevel);
end;

procedure TNsisCodeBuilder.Clear;
begin
  FLines.Clear;
  FIndentLevel := 0;
end;

function TNsisCodeBuilder.GetCode: string;
begin
  Result := FLines.Text;
end;

end.
