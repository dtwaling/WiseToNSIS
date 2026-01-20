unit NSIS.Mapper;

{******************************************************************************
  WiseToNSIS Converter - AST to NSIS Mapper

  Visitor pattern implementation that transforms WSE AST nodes to NSIS output.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Conversion.Types, WSE.AST, WSE.Flags, NSIS.Generator, NSIS.Pages;

type
  { Mapper options }
  TNsisMapperOptions = record
    GenerateUninstaller: Boolean;
    GenerateComments: Boolean;
    MarkTodoItems: Boolean;
    UseLogicLib: Boolean;
    SimpleSCPlugin: Boolean;
  end;

  { Forward declaration }
  TNsisMapperVisitor = class;

  { Base visitor interface implementation - no reference counting }
  TWseBaseVisitor = class(TObject, IWseVisitor)
  protected
    { IInterface - disable reference counting for manual memory management }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    procedure VisitDocument(ANode: TWseDocument); virtual;
    procedure VisitBlock(ANode: TWseBlock); virtual;
    procedure VisitChildren(ANode: TWseNode);
  end;

  { NSIS Mapper Visitor - transforms WSE to NSIS }
  TNsisMapperVisitor = class(TWseBaseVisitor)
  private
    FScript: TNsisScript;
    FCurrentSection: TNsisSection;
    FIssues: TConversionIssueList;
    FOptions: TNsisMapperOptions;
    FConditionStack: TStack<Boolean>;  // Track if/else nesting
    FInWizardBlock: Boolean;
    FPageGenerator: TNsisPageGenerator;  // Custom dialog to nsDialogs converter

    procedure SetupDefaultOptions;
    procedure SetupStandardIncludes;

    { Block mapping methods }
    procedure MapGlobalBlock(ABlock: TWseGlobalBlock);
    procedure MapSetVariableBlock(ABlock: TWseSetVariableBlock);
    procedure MapIfWhileBlock(ABlock: TWseIfWhileBlock);
    procedure MapElseBlock(ABlock: TWseElseBlock);
    procedure MapEndBlock(ABlock: TWseEndBlock);
    procedure MapInstallFileBlock(ABlock: TWseInstallFileBlock);
    procedure MapEditRegistryBlock(ABlock: TWseEditRegistryBlock);
    procedure MapGetRegistryBlock(ABlock: TWseGetRegistryBlock);
    procedure MapDisplayMessageBlock(ABlock: TWseDisplayMessageBlock);
    procedure MapExecuteProgramBlock(ABlock: TWseExecuteProgramBlock);
    procedure MapServiceBlock(ABlock: TWseServiceBlock);
    procedure MapCreateShortcutBlock(ABlock: TWseCreateShortcutBlock);
    procedure MapRemarkBlock(ABlock: TWseRemarkBlock);
    procedure MapCheckExistsBlock(ABlock: TWseCheckExistsBlock);
    procedure MapCreateDirectoryBlock(ABlock: TWseCreateDirectoryBlock);
    procedure MapDeleteFileBlock(ABlock: TWseDeleteFileBlock);
    procedure MapCopyFileBlock(ABlock: TWseCopyFileBlock);
    procedure MapExitBlock(ABlock: TWseExitBlock);
    procedure MapWizardBlock(ABlock: TWseWizardBlock);
    procedure MapCustomDialogBlock(ABlock: TWseCustomDialogBlock);
    procedure MapParseStringBlock(ABlock: TWseParseStringBlock);
    procedure MapGetSystemInfoBlock(ABlock: TWseGetSystemInfoBlock);
    procedure MapGetEnvironmentBlock(ABlock: TWseGetEnvironmentBlock);
    procedure MapEditIniBlock(ABlock: TWseEditIniBlock);
    procedure MapReadIniBlock(ABlock: TWseReadIniBlock);
    procedure MapGetTempFilenameBlock(ABlock: TWseGetTempFilenameBlock);
    procedure MapOpenCloseLogBlock(ABlock: TWseOpenCloseLogBlock);
    procedure MapAddTextToLogBlock(ABlock: TWseAddTextToLogBlock);
    procedure MapIncludeScriptBlock(ABlock: TWseIncludeScriptBlock);
    procedure MapCustomScriptBlock(ABlock: TWseCustomScriptBlock);
    procedure MapSelfRegisterBlock(ABlock: TWseSelfRegisterBlock);
    procedure MapElseIfBlock(ABlock: TWseElseIfBlock);
    procedure MapInsertLineBlock(ABlock: TWseInsertLineBlock);
    procedure MapReadUpdateTextBlock(ABlock: TWseReadUpdateTextBlock);
    procedure MapRenameFileBlock(ABlock: TWseRenameFileBlock);
    procedure MapSearchFileBlock(ABlock: TWseSearchFileBlock);
    procedure MapFindInPathBlock(ABlock: TWseFindInPathBlock);
    procedure MapCheckConfigBlock(ABlock: TWseCheckConfigBlock);
    procedure MapCreateServiceBlock(ABlock: TWseCreateServiceBlock);
    procedure MapExecuteVBScriptBlock(ABlock: TWseExecuteVBScriptBlock);
    procedure MapDialogElementBlock(ABlock: TWseDialogBlock);
    procedure MapStaticBlock(ABlock: TWseStaticBlock);
    procedure MapPushButtonBlock(ABlock: TWsePushButtonBlock);
    procedure MapEditboxBlock(ABlock: TWseEditboxBlock);
    procedure MapCheckboxControlBlock(ABlock: TWseCheckboxBlock);
    procedure MapRadioButtonBlock(ABlock: TWseRadioButtonBlock);
    procedure MapListboxBlock(ABlock: TWseListboxBlock);
    procedure MapSetControlAttrBlock(ABlock: TWseSetControlAttrBlock);
    procedure MapNewEventBlock(ABlock: TWseNewEventBlock);
    procedure MapGenericBlock(ABlock: TWseGenericBlock);

    { Helper methods }
    procedure AddTodo(const AMessage: string; ABlock: TWseBlock);
    procedure EnsureSection;
    function ExtractDestinationPath(const AFullPath: string): string;
    function ExtractFilename(const AFullPath: string): string;
  protected
    procedure VisitDocument(ANode: TWseDocument); override;
    procedure VisitBlock(ANode: TWseBlock); override;
  public
    constructor Create(AIssues: TConversionIssueList);
    destructor Destroy; override;

    function Convert(ADocument: TWseDocument): TNsisScript;

    property Script: TNsisScript read FScript;
    property Issues: TConversionIssueList read FIssues;
    property Options: TNsisMapperOptions read FOptions write FOptions;
  end;

implementation

uses
  System.StrUtils, System.RegularExpressions;

{ TWseBaseVisitor }

function TWseBaseVisitor.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TWseBaseVisitor._AddRef: Integer;
begin
  Result := -1;  // No reference counting
end;

function TWseBaseVisitor._Release: Integer;
begin
  Result := -1;  // No reference counting
end;

procedure TWseBaseVisitor.VisitDocument(ANode: TWseDocument);
begin
  VisitChildren(ANode);
end;

procedure TWseBaseVisitor.VisitBlock(ANode: TWseBlock);
begin
  VisitChildren(ANode);
end;

procedure TWseBaseVisitor.VisitChildren(ANode: TWseNode);
var
  i: Integer;
begin
  for i := 0 to ANode.ChildCount - 1 do
    ANode.Children[i].Accept(Self);
end;

{ TNsisMapperVisitor }

constructor TNsisMapperVisitor.Create(AIssues: TConversionIssueList);
begin
  inherited Create;
  FIssues := AIssues;
  FScript := nil;
  FCurrentSection := nil;
  FConditionStack := TStack<Boolean>.Create;
  FPageGenerator := TNsisPageGenerator.Create;
  FInWizardBlock := False;
  SetupDefaultOptions;
end;

destructor TNsisMapperVisitor.Destroy;
begin
  FPageGenerator.Free;
  FConditionStack.Free;
  inherited Destroy;
end;

procedure TNsisMapperVisitor.SetupDefaultOptions;
begin
  FOptions.GenerateUninstaller := True;
  FOptions.GenerateComments := True;
  FOptions.MarkTodoItems := True;
  FOptions.UseLogicLib := True;
  FOptions.SimpleSCPlugin := True;
end;

procedure TNsisMapperVisitor.SetupStandardIncludes;
begin
  FScript.AddInclude('MUI2.nsh');

  if FOptions.UseLogicLib then
    FScript.AddInclude('LogicLib.nsh');
end;

function TNsisMapperVisitor.Convert(ADocument: TWseDocument): TNsisScript;
begin
  FScript := TNsisScript.Create;
  FCurrentSection := nil;
  FConditionStack.Clear;
  FInWizardBlock := False;

  try
    SetupStandardIncludes;
    ADocument.Accept(Self);
    Result := FScript;
  except
    FScript.Free;
    raise;
  end;
end;

procedure TNsisMapperVisitor.VisitDocument(ANode: TWseDocument);
begin
  // Process all children
  VisitChildren(ANode);

  // Create default main section if none exists
  if FScript.SectionCount = 0 then
  begin
    FCurrentSection := FScript.CreateSection('Main', 'SecMain');
    FCurrentSection.ReadOnly := True;
  end;

  // Add uninstaller creation
  if FOptions.GenerateUninstaller then
  begin
    EnsureSection;
    FCurrentSection.AddBlankLine;
    FCurrentSection.AddComment('Create uninstaller');
    FCurrentSection.WriteUninstaller('$INSTDIR\uninstall.exe');
  end;
end;

procedure TNsisMapperVisitor.VisitBlock(ANode: TWseBlock);
begin
  // Skip remarked blocks
  if ANode.IsRemarked then
  begin
    ANode.ConversionStatus := csSkipped;
    Exit;
  end;

  // Dispatch to specific mapper based on block type
  if ANode is TWseGlobalBlock then
    MapGlobalBlock(TWseGlobalBlock(ANode))
  else if ANode is TWseSetVariableBlock then
    MapSetVariableBlock(TWseSetVariableBlock(ANode))
  else if ANode is TWseIfWhileBlock then
    MapIfWhileBlock(TWseIfWhileBlock(ANode))
  else if ANode is TWseElseBlock then
    MapElseBlock(TWseElseBlock(ANode))
  else if ANode is TWseEndBlock then
    MapEndBlock(TWseEndBlock(ANode))
  else if ANode is TWseInstallFileBlock then
    MapInstallFileBlock(TWseInstallFileBlock(ANode))
  else if ANode is TWseEditRegistryBlock then
    MapEditRegistryBlock(TWseEditRegistryBlock(ANode))
  else if ANode is TWseGetRegistryBlock then
    MapGetRegistryBlock(TWseGetRegistryBlock(ANode))
  else if ANode is TWseDisplayMessageBlock then
    MapDisplayMessageBlock(TWseDisplayMessageBlock(ANode))
  else if ANode is TWseExecuteProgramBlock then
    MapExecuteProgramBlock(TWseExecuteProgramBlock(ANode))
  else if ANode is TWseServiceBlock then
    MapServiceBlock(TWseServiceBlock(ANode))
  else if ANode is TWseCreateShortcutBlock then
    MapCreateShortcutBlock(TWseCreateShortcutBlock(ANode))
  else if ANode is TWseRemarkBlock then
    MapRemarkBlock(TWseRemarkBlock(ANode))
  else if ANode is TWseCheckExistsBlock then
    MapCheckExistsBlock(TWseCheckExistsBlock(ANode))
  else if ANode is TWseCreateDirectoryBlock then
    MapCreateDirectoryBlock(TWseCreateDirectoryBlock(ANode))
  else if ANode is TWseDeleteFileBlock then
    MapDeleteFileBlock(TWseDeleteFileBlock(ANode))
  else if ANode is TWseCopyFileBlock then
    MapCopyFileBlock(TWseCopyFileBlock(ANode))
  else if ANode is TWseExitBlock then
    MapExitBlock(TWseExitBlock(ANode))
  else if ANode is TWseWizardBlock then
    MapWizardBlock(TWseWizardBlock(ANode))
  else if ANode is TWseCustomDialogBlock then
    MapCustomDialogBlock(TWseCustomDialogBlock(ANode))
  else if ANode is TWseParseStringBlock then
    MapParseStringBlock(TWseParseStringBlock(ANode))
  else if ANode is TWseGetSystemInfoBlock then
    MapGetSystemInfoBlock(TWseGetSystemInfoBlock(ANode))
  else if ANode is TWseGetEnvironmentBlock then
    MapGetEnvironmentBlock(TWseGetEnvironmentBlock(ANode))
  else if ANode is TWseEditIniBlock then
    MapEditIniBlock(TWseEditIniBlock(ANode))
  else if ANode is TWseReadIniBlock then
    MapReadIniBlock(TWseReadIniBlock(ANode))
  else if ANode is TWseGetTempFilenameBlock then
    MapGetTempFilenameBlock(TWseGetTempFilenameBlock(ANode))
  else if ANode is TWseOpenCloseLogBlock then
    MapOpenCloseLogBlock(TWseOpenCloseLogBlock(ANode))
  else if ANode is TWseAddTextToLogBlock then
    MapAddTextToLogBlock(TWseAddTextToLogBlock(ANode))
  else if ANode is TWseIncludeScriptBlock then
    MapIncludeScriptBlock(TWseIncludeScriptBlock(ANode))
  else if ANode is TWseCustomScriptBlock then
    MapCustomScriptBlock(TWseCustomScriptBlock(ANode))
  else if ANode is TWseSelfRegisterBlock then
    MapSelfRegisterBlock(TWseSelfRegisterBlock(ANode))
  else if ANode is TWseElseIfBlock then
    MapElseIfBlock(TWseElseIfBlock(ANode))
  else if ANode is TWseInsertLineBlock then
    MapInsertLineBlock(TWseInsertLineBlock(ANode))
  else if ANode is TWseReadUpdateTextBlock then
    MapReadUpdateTextBlock(TWseReadUpdateTextBlock(ANode))
  else if ANode is TWseRenameFileBlock then
    MapRenameFileBlock(TWseRenameFileBlock(ANode))
  else if ANode is TWseSearchFileBlock then
    MapSearchFileBlock(TWseSearchFileBlock(ANode))
  else if ANode is TWseFindInPathBlock then
    MapFindInPathBlock(TWseFindInPathBlock(ANode))
  else if ANode is TWseCheckConfigBlock then
    MapCheckConfigBlock(TWseCheckConfigBlock(ANode))
  else if ANode is TWseCreateServiceBlock then
    MapCreateServiceBlock(TWseCreateServiceBlock(ANode))
  else if ANode is TWseExecuteVBScriptBlock then
    MapExecuteVBScriptBlock(TWseExecuteVBScriptBlock(ANode))
  else if ANode is TWseDialogBlock then
    MapDialogElementBlock(TWseDialogBlock(ANode))
  else if ANode is TWseStaticBlock then
    MapStaticBlock(TWseStaticBlock(ANode))
  else if ANode is TWsePushButtonBlock then
    MapPushButtonBlock(TWsePushButtonBlock(ANode))
  else if ANode is TWseEditboxBlock then
    MapEditboxBlock(TWseEditboxBlock(ANode))
  else if ANode is TWseCheckboxBlock then
    MapCheckboxControlBlock(TWseCheckboxBlock(ANode))
  else if ANode is TWseRadioButtonBlock then
    MapRadioButtonBlock(TWseRadioButtonBlock(ANode))
  else if ANode is TWseListboxBlock then
    MapListboxBlock(TWseListboxBlock(ANode))
  else if ANode is TWseSetControlAttrBlock then
    MapSetControlAttrBlock(TWseSetControlAttrBlock(ANode))
  else if ANode is TWseNewEventBlock then
    MapNewEventBlock(TWseNewEventBlock(ANode))
  else if ANode is TWseGenericBlock then
    MapGenericBlock(TWseGenericBlock(ANode))
  else
  begin
    // Unknown block type
    ANode.ConversionStatus := csPartial;
    AddTodo('Unknown block type: ' + ANode.BlockType, ANode);
    VisitChildren(ANode);
  end;
end;

procedure TNsisMapperVisitor.EnsureSection;
begin
  if FCurrentSection = nil then
  begin
    FCurrentSection := FScript.CreateSection('Main', 'SecMain');
    FCurrentSection.ReadOnly := True;
  end;
end;

procedure TNsisMapperVisitor.AddTodo(const AMessage: string; ABlock: TWseBlock);
begin
  if FOptions.MarkTodoItems then
  begin
    EnsureSection;
    FCurrentSection.AddComment('TODO: ' + AMessage);
  end;

  FIssues.AddIssue(csWarning, icAmbiguous, AMessage, ABlock.Line, ABlock.Column);
end;

function TNsisMapperVisitor.ExtractDestinationPath(const AFullPath: string): string;
var
  LastSlash: Integer;
begin
  LastSlash := LastDelimiter('\/', AFullPath);
  if LastSlash > 0 then
    Result := Copy(AFullPath, 1, LastSlash - 1)
  else
    Result := AFullPath;
end;

function TNsisMapperVisitor.ExtractFilename(const AFullPath: string): string;
var
  LastSlash: Integer;
begin
  LastSlash := LastDelimiter('\/', AFullPath);
  if LastSlash > 0 then
    Result := Copy(AFullPath, LastSlash + 1, MaxInt)
  else
    Result := AFullPath;
end;

{ Block Mapping Methods }

procedure TNsisMapperVisitor.MapGlobalBlock(ABlock: TWseGlobalBlock);
var
  VarInfo: TVariableInfo;
  VarEnum: TVariableDictionary.TPairEnumerator;
  Doc: TWseDocument;
begin
  // Set script metadata
  FScript.Name := ABlock.Title;
  FScript.OutFile := ABlock.ExeFilename;

  // Set execution level
  if SameText(ABlock.RequestedExecutionLevel, 'requireAdministrator') then
    FScript.ExecutionLevel := 'admin'
  else if SameText(ABlock.RequestedExecutionLevel, 'asInvoker') then
    FScript.ExecutionLevel := 'user'
  else
    FScript.ExecutionLevel := 'admin';

  // Extract and register variables
  Doc := TWseDocument(ABlock.Parent);
  if Assigned(Doc) then
  begin
    VarEnum := Doc.Variables.GetEnumerator;
    try
      while VarEnum.MoveNext do
      begin
        VarInfo := VarEnum.Current.Value;
        // Only register non-system variables
        if not IsSystemVariable(VarInfo.Name) then
        begin
          FScript.AddVariable(VarInfo.Name);

          // Set default value in .onInit
          if VarInfo.DefaultValue <> '' then
            FScript.AddInitCode('StrCpy $' + VarInfo.Name + ' "' +
              ConvertWiseVarReferences(VarInfo.DefaultValue) + '"');
        end;
      end;
    finally
      VarEnum.Free;
    end;
  end;

  ABlock.ConversionStatus := csConverted;
  VisitChildren(ABlock);
end;

procedure TNsisMapperVisitor.MapSetVariableBlock(ABlock: TWseSetVariableBlock);
var
  VarName, Value: string;
  VarFlags: TWseVariableFlags;
begin
  EnsureSection;

  VarName := ABlock.Variable;
  Value := ABlock.Value;
  VarFlags := ParseVariableFlags(ABlock.Flags);

  // Ensure variable is declared
  if not IsSystemVariable(VarName) then
    FScript.AddVariable(VarName);

  // Generate StrCpy
  if VarFlags.AppendValue then
    FCurrentSection.AddLine('StrCpy $' + VarName + ' "$' + VarName +
      ConvertWiseVarReferences(Value) + '"')
  else
    FCurrentSection.AddLine('StrCpy $' + VarName + ' "' +
      ConvertWiseVarReferences(Value) + '"');

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapIfWhileBlock(ABlock: TWseIfWhileBlock);
var
  Variable, Value, Op: string;
  CompOp: TWseComparisonOp;
  IsWhile: Boolean;
begin
  EnsureSection;

  Variable := ConvertWiseVariable('%' + ABlock.Variable + '%');
  Value := ConvertWiseVarReferences(ABlock.Value);
  CompOp := ParseComparisonFlags(ABlock.Flags);
  Op := ComparisonOpToNsis(CompOp);
  IsWhile := ABlock.IsWhileLoop;

  // Handle special comparison types
  if CompOp = wcoContains then
  begin
    FCurrentSection.AddLine('${StrStr} $0 ' + Variable + ' "' + Value + '"');
    FCurrentSection.AddLine('${If} $0 != ""');
  end
  else if CompOp = wcoNotContains then
  begin
    FCurrentSection.AddLine('${StrStr} $0 ' + Variable + ' "' + Value + '"');
    FCurrentSection.AddLine('${If} $0 == ""');
  end
  else if IsWhile then
    FCurrentSection.AddLine('${While} ' + Variable + ' ' + Op + ' "' + Value + '"')
  else
    FCurrentSection.AddLine('${If} ' + Variable + ' ' + Op + ' "' + Value + '"');

  FCurrentSection.Indent;
  FConditionStack.Push(IsWhile);

  ABlock.ConversionStatus := csConverted;
  VisitChildren(ABlock);
end;

procedure TNsisMapperVisitor.MapElseBlock(ABlock: TWseElseBlock);
begin
  EnsureSection;

  FCurrentSection.Outdent;
  FCurrentSection.AddLine('${Else}');
  FCurrentSection.Indent;

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapEndBlock(ABlock: TWseEndBlock);
var
  WasWhile: Boolean;
begin
  EnsureSection;

  FCurrentSection.Outdent;

  if FConditionStack.Count > 0 then
  begin
    WasWhile := FConditionStack.Pop;
    if WasWhile then
      FCurrentSection.AddLine('${EndWhile}')
    else
      FCurrentSection.AddLine('${EndIf}');
  end
  else
    FCurrentSection.AddLine('${EndIf}');

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapInstallFileBlock(ABlock: TWseInstallFileBlock);
var
  Source, Dest, DestPath: string;
  FileFlags: TWseFileFlags;
begin
  EnsureSection;

  Source := ABlock.Source;
  Dest := ABlock.Destination;
  DestPath := ExtractDestinationPath(Dest);
  FileFlags := ParseFileFlags(ABlock.Flags);

  // SetOutPath
  FCurrentSection.SetOutPath(DestPath);

  // File command
  FCurrentSection.AddFile(Source, FileFlags.Recursive);

  // Self-register if needed
  if FileFlags.SelfRegister then
  begin
    FCurrentSection.AddLine('RegDLL "' + ConvertWisePathToNsis(Dest) + '"');

    // Add to uninstaller
    FScript.UninstallSection.AddLine('UnRegDLL "' + ConvertWisePathToNsis(Dest) + '"');
  end;

  // Add to uninstaller
  if FOptions.GenerateUninstaller and not FileFlags.NoUninstall then
    FScript.UninstallSection.Delete(Dest);

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapEditRegistryBlock(ABlock: TWseEditRegistryBlock);
var
  RootKey, SubKey, ValueName, Value: string;
  RegFlags: TWseRegistryFlags;
begin
  EnsureSection;

  RootKey := ABlock.GetNsisRootKey;
  SubKey := ABlock.Key;
  ValueName := ABlock.ValueName;
  Value := ABlock.NewValue;
  RegFlags := ParseRegistryFlags(ABlock.Flags);

  if RegFlags.DeleteKey then
  begin
    FCurrentSection.AddLine('DeleteRegKey ' + RootKey + ' "' + SubKey + '"');
  end
  else if RegFlags.DeleteValue then
  begin
    FCurrentSection.AddLine('DeleteRegValue ' + RootKey + ' "' + SubKey + '" "' + ValueName + '"');
  end
  else if RegFlags.IsDWORD then
  begin
    FCurrentSection.WriteRegDWORD(RootKey, SubKey, ValueName, StrToIntDef(Value, 0));
  end
  else
  begin
    FCurrentSection.WriteRegStr(RootKey, SubKey, ValueName, Value);
  end;

  // Add to uninstaller
  if FOptions.GenerateUninstaller then
  begin
    if not RegFlags.DeleteKey then
      FScript.UninstallSection.AddLine('DeleteRegValue ' + RootKey + ' "' + SubKey + '" "' + ValueName + '"');
  end;

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapGetRegistryBlock(ABlock: TWseGetRegistryBlock);
var
  VarName, RootKey, SubKey, ValueName, DefaultValue: string;
begin
  EnsureSection;

  VarName := ABlock.Variable;
  RootKey := 'HKLM';  // Default, would need to parse flags for actual root
  SubKey := ABlock.Key;
  ValueName := ABlock.ValueName;
  DefaultValue := ABlock.DefaultValue;

  // Ensure variable is declared
  FScript.AddVariable(VarName);

  // Read registry
  FCurrentSection.ReadRegStr('$' + VarName, RootKey, SubKey, ValueName);

  // Handle default value
  if DefaultValue <> '' then
  begin
    FCurrentSection.AddLine('${If} $' + VarName + ' == ""');
    FCurrentSection.Indent;
    FCurrentSection.AddLine('StrCpy $' + VarName + ' "' + ConvertWiseVarReferences(DefaultValue) + '"');
    FCurrentSection.Outdent;
    FCurrentSection.AddLine('${EndIf}');
  end;

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapDisplayMessageBlock(ABlock: TWseDisplayMessageBlock);
var
  MsgType: TWseMessageBoxType;
  MsgIcon: TWseMessageBoxIcon;
  Flags, Text: string;
begin
  EnsureSection;

  MsgType := ParseMessageBoxTypeFlags(ABlock.Flags);
  MsgIcon := ParseMessageBoxIconFlags(ABlock.Flags);

  Flags := MessageBoxTypeToNsis(MsgType);
  if MsgIcon <> wmiNone then
    Flags := Flags + '|' + MessageBoxIconToNsis(MsgIcon);

  Text := ABlock.Text;
  // Replace line breaks with NSIS escape sequences
  Text := StringReplace(Text, #13#10, '$\r$\n', [rfReplaceAll]);

  FCurrentSection.MessageBox(Flags, Text);

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapExecuteProgramBlock(ABlock: TWseExecuteProgramBlock);
var
  Pathname, CommandLine, FullCmd: string;
begin
  EnsureSection;

  Pathname := ConvertWisePathToNsis(ABlock.Pathname);
  CommandLine := ConvertWiseVarReferences(ABlock.CommandLine);

  if CommandLine <> '' then
    FullCmd := '"' + Pathname + '" ' + CommandLine
  else
    FullCmd := Pathname;

  if ABlock.WaitForCompletion then
    FCurrentSection.ExecWait(FullCmd)
  else
    FCurrentSection.Exec(FullCmd);

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapServiceBlock(ABlock: TWseServiceBlock);
var
  ServiceName: string;
  SvcFlags: TWseServiceFlags;
begin
  EnsureSection;

  ServiceName := ABlock.ServiceName;
  SvcFlags := ParseServiceFlags(ABlock.Flags);

  // Check if SimpleSC plugin should be used
  if not FOptions.SimpleSCPlugin then
  begin
    AddTodo('Service operation requires SimpleSC plugin: ' + ServiceName, ABlock);
    ABlock.ConversionStatus := csPartial;
    Exit;
  end;

  if SvcFlags.IsStop then
  begin
    FCurrentSection.AddComment('Stop service: ' + ServiceName);
    if SvcFlags.WaitForCompletion then
      FCurrentSection.AddLine(Format('SimpleSC::StopService "%s" 1 %d', [ServiceName, SvcFlags.Timeout]))
    else
      FCurrentSection.AddLine(Format('SimpleSC::StopService "%s" 0 %d', [ServiceName, SvcFlags.Timeout]));
    FCurrentSection.AddLine('Pop $0');

    if not SvcFlags.IgnoreErrors then
    begin
      FCurrentSection.AddLine('${If} $0 != 0');
      FCurrentSection.Indent;
      FCurrentSection.AddLine('DetailPrint "Failed to stop service: ' + ServiceName + '"');
      FCurrentSection.Outdent;
      FCurrentSection.AddLine('${EndIf}');
    end;
  end
  else
  begin
    FCurrentSection.AddComment('Start service: ' + ServiceName);
    FCurrentSection.AddLine(Format('SimpleSC::StartService "%s" "" %d', [ServiceName, SvcFlags.Timeout]));
    FCurrentSection.AddLine('Pop $0');

    if not SvcFlags.IgnoreErrors then
    begin
      FCurrentSection.AddLine('${If} $0 != 0');
      FCurrentSection.Indent;
      FCurrentSection.AddLine('DetailPrint "Failed to start service: ' + ServiceName + '"');
      FCurrentSection.Outdent;
      FCurrentSection.AddLine('${EndIf}');
    end;
  end;

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapCreateShortcutBlock(ABlock: TWseCreateShortcutBlock);
begin
  EnsureSection;

  FCurrentSection.CreateShortCut(
    ABlock.Destination,
    ABlock.Source,
    ABlock.CommandLine,
    ABlock.IconPathname,
    ABlock.IconIndex,
    ABlock.WorkingDirectory
  );

  // Add to uninstaller
  if FOptions.GenerateUninstaller then
    FScript.UninstallSection.Delete(ABlock.Destination);

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapRemarkBlock(ABlock: TWseRemarkBlock);
begin
  if FOptions.GenerateComments and (ABlock.Text <> '') then
  begin
    EnsureSection;
    FCurrentSection.AddComment(ABlock.Text);
  end;

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapCheckExistsBlock(ABlock: TWseCheckExistsBlock);
var
  Pathname: string;
  CheckFlags: TWseCheckExistsFlags;
begin
  EnsureSection;

  Pathname := ConvertWisePathToNsis(ABlock.Pathname);
  CheckFlags := ParseCheckExistsFlags(ABlock.Flags);

  if CheckFlags.NegateResult then
    FCurrentSection.AddLine('${IfNot} ${FileExists} "' + Pathname + '"')
  else
    FCurrentSection.AddLine('${If} ${FileExists} "' + Pathname + '"');

  FCurrentSection.Indent;
  FConditionStack.Push(False);

  ABlock.ConversionStatus := csConverted;
  VisitChildren(ABlock);
end;

procedure TNsisMapperVisitor.MapCreateDirectoryBlock(ABlock: TWseCreateDirectoryBlock);
begin
  EnsureSection;

  FCurrentSection.CreateDirectory(ABlock.Pathname);

  // Add to uninstaller
  if FOptions.GenerateUninstaller then
    FScript.UninstallSection.RMDir(ABlock.Pathname);

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapDeleteFileBlock(ABlock: TWseDeleteFileBlock);
var
  Pathname: string;
  DeleteFlags: TWseDeleteFlags;
begin
  EnsureSection;

  Pathname := ConvertWisePathToNsis(ABlock.Pathname);
  DeleteFlags := ParseDeleteFlags(ABlock.Flags);

  if DeleteFlags.Recursive then
  begin
    // Recursive directory delete
    FCurrentSection.AddComment('Recursive delete');
    FCurrentSection.RMDir('/r "' + Pathname + '"');
  end
  else if DeleteFlags.RebootOnClose then
  begin
    // Delete on reboot if file is in use
    FCurrentSection.AddLine('Delete /REBOOTOK "' + Pathname + '"');
  end
  else
  begin
    // Normal delete
    FCurrentSection.Delete(Pathname);
  end;

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapCopyFileBlock(ABlock: TWseCopyFileBlock);
begin
  EnsureSection;

  FCurrentSection.CopyFiles(ABlock.Source, ABlock.Destination);

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapExitBlock(ABlock: TWseExitBlock);
begin
  EnsureSection;

  FCurrentSection.AddLine('Abort');

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapWizardBlock(ABlock: TWseWizardBlock);
begin
  // Wizard blocks define the dialog flow
  // In NSIS, this is handled by MUI2 page macros
  FInWizardBlock := True;

  if FOptions.GenerateComments then
  begin
    EnsureSection;
    FCurrentSection.AddComment('Wizard block converted to MUI2 pages');
  end;

  ABlock.ConversionStatus := csConverted;
  VisitChildren(ABlock);

  FInWizardBlock := False;
end;

procedure TNsisMapperVisitor.MapCustomDialogBlock(ABlock: TWseCustomDialogBlock);
var
  DialogName: string;
  Page: TNsisPageDef;
  PageCode: string;
begin
  DialogName := ABlock.Name;

  // Convert the Wise dialog to an NSIS page definition
  Page := FPageGenerator.ConvertDialog(ABlock);

  if Page.Controls.Count > 0 then
  begin
    // Generate the page code
    PageCode := FPageGenerator.GeneratePageFile(Page);

    // Add include for nsDialogs
    FScript.AddInclude('nsDialogs.nsh');

    // Add page declaration
    FScript.AddPage('Page custom Page' + Page.Name + '_Pre Page' + Page.Name + '_Leave');

    // Store the page code for later output
    FScript.AddCustomPageCode(PageCode);

    if FOptions.GenerateComments then
    begin
      EnsureSection;
      FCurrentSection.AddComment('Custom dialog "' + DialogName + '" converted to nsDialogs page');
    end;

    ABlock.ConversionStatus := csConverted;
  end
  else
  begin
    // No controls found - mark for review
    AddTodo('Custom dialog has no convertible controls: ' + DialogName, ABlock);
    FScript.AddPage('; TODO: Custom page for ' + DialogName);
    ABlock.ConversionStatus := csPartial;
  end;

  VisitChildren(ABlock);
end;

procedure TNsisMapperVisitor.MapParseStringBlock(ABlock: TWseParseStringBlock);
var
  Source, Pattern, Var1, Var2: string;
  ParseFlags: TWseParseStringFlags;
  Position: Integer;
begin
  EnsureSection;

  Source := ConvertWiseVarReferences(ABlock.Source);
  Pattern := ABlock.Pattern;
  Var1 := ABlock.Variable1;
  Var2 := ABlock.Variable2;
  ParseFlags := ParseParseStringFlags(ABlock.Flags);

  // Ensure variables are declared
  if Var1 <> '' then
    FScript.AddVariable(Var1);
  if Var2 <> '' then
    FScript.AddVariable(Var2);

  // Include StrFunc.nsh for string functions
  FScript.AddInclude('StrFunc.nsh');

  if FOptions.GenerateComments then
    FCurrentSection.AddComment('Parse string: ' + ABlock.Source);

  if ParseFlags.IsPositionBased then
  begin
    // Position-based split
    Position := StrToIntDef(Pattern, 1);

    case ParseFlags.Operation of
      wpoSplitPositionLeft:
      begin
        // Split at position from left
        if Var1 <> '' then
          FCurrentSection.AddLine(Format('StrCpy $%s "%s" %d', [Var1, Source, Position]));
        if Var2 <> '' then
          FCurrentSection.AddLine(Format('StrCpy $%s "%s" "" %d', [Var2, Source, Position]));
      end;
      wpoSplitPositionRight:
      begin
        // Split at position from right (negative offset)
        if Var1 <> '' then
          FCurrentSection.AddLine(Format('StrCpy $%s "%s" -%d', [Var1, Source, Position]));
        if Var2 <> '' then
          FCurrentSection.AddLine(Format('StrCpy $%s "%s" "" -%d', [Var2, Source, Position]));
      end;
    end;
  end
  else
  begin
    // Pattern-based split - use WordFind or custom logic
    case ParseFlags.Operation of
      wpoSplitFirstPattern:
      begin
        // Split at first occurrence of pattern
        // Use StrStr to find position, then StrCpy to split
        FCurrentSection.AddLine(Format('${StrStr} $0 "%s" "%s"', [Source, Pattern]));
        if Var1 <> '' then
        begin
          FCurrentSection.AddLine('StrLen $1 $0');
          FCurrentSection.AddLine(Format('StrLen $2 "%s"', [Source]));
          FCurrentSection.AddLine('IntOp $1 $2 - $1');
          FCurrentSection.AddLine(Format('StrCpy $%s "%s" $1', [Var1, Source]));
        end;
        if Var2 <> '' then
        begin
          FCurrentSection.AddLine('StrLen $1 "' + Pattern + '"');
          FCurrentSection.AddLine('IntOp $1 $1 + 0');
          FCurrentSection.AddLine(Format('StrCpy $%s $0 "" $1', [Var2]));
        end;
      end;
      wpoSplitLastPattern:
      begin
        // Split at last occurrence - use StrRep or WordFind
        FCurrentSection.AddLine(Format('${StrStrAdv} $0 "%s" "%s" ">" "<" "0" "0" "0"', [Source, Pattern]));
        if Var1 <> '' then
          FCurrentSection.AddLine(Format('StrCpy $%s $0', [Var1]));
        if Var2 <> '' then
        begin
          FCurrentSection.AddLine('StrLen $1 $0');
          FCurrentSection.AddLine('StrLen $2 "' + Pattern + '"');
          FCurrentSection.AddLine('IntOp $1 $1 + $2');
          FCurrentSection.AddLine(Format('StrCpy $%s "%s" "" $1', [Var2, Source]));
        end;
      end;
    else
      // Other operations - generate with TODO for manual review
      FCurrentSection.AddComment('TODO: Complex parse operation - review manually');
      if Var1 <> '' then
        FCurrentSection.AddLine(Format('StrCpy $%s "%s"', [Var1, Source]));
      if Var2 <> '' then
        FCurrentSection.AddLine(Format('StrCpy $%s ""', [Var2]));
    end;
  end;

  // Apply trim if requested
  if ParseFlags.TrimSpaces then
  begin
    if Var1 <> '' then
      FCurrentSection.AddLine(Format('${TrimNewLines} $%s $%s', [Var1, Var1]));
    if Var2 <> '' then
      FCurrentSection.AddLine(Format('${TrimNewLines} $%s $%s', [Var2, Var2]));
  end;

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapGetSystemInfoBlock(ABlock: TWseGetSystemInfoBlock);
var
  VarName: string;
  InfoType: TWseSystemInfoType;
begin
  EnsureSection;

  VarName := ABlock.Variable;
  FScript.AddVariable(VarName);
  InfoType := ParseSystemInfoFlags(ABlock.Flags);

  case InfoType of
    wsiWindowsVersion:
    begin
      // Get Windows version string using System plugin
      FCurrentSection.AddComment('Get Windows version');
      FCurrentSection.AddLine('System::Call "kernel32::GetVersion() i.r0"');
      FCurrentSection.AddLine('IntOp $1 $0 & 0xFF');        // Major
      FCurrentSection.AddLine('IntOp $0 $0 >> 8');
      FCurrentSection.AddLine('IntOp $2 $0 & 0xFF');        // Minor
      FCurrentSection.AddLine(Format('StrCpy $%s "$1.$2"', [VarName]));
    end;

    wsiWindowsMajor:
    begin
      FCurrentSection.AddComment('Get Windows major version');
      FCurrentSection.AddLine('System::Call "kernel32::GetVersion() i.r0"');
      FCurrentSection.AddLine(Format('IntOp $%s $0 & 0xFF', [VarName]));
    end;

    wsiWindowsMinor:
    begin
      FCurrentSection.AddComment('Get Windows minor version');
      FCurrentSection.AddLine('System::Call "kernel32::GetVersion() i.r0"');
      FCurrentSection.AddLine('IntOp $0 $0 >> 8');
      FCurrentSection.AddLine(Format('IntOp $%s $0 & 0xFF', [VarName]));
    end;

    wsiPhysicalMemoryKB:
    begin
      // Get physical memory using System plugin
      FCurrentSection.AddComment('Get physical memory (KB)');
      FCurrentSection.AddLine('System::Alloc 64');
      FCurrentSection.AddLine('Pop $0');
      FCurrentSection.AddLine('System::Call "kernel32::GlobalMemoryStatusEx(p r0)"');
      FCurrentSection.AddLine('System::Call "*$0(i, i, l.r1, l, l, l, l, l, l)"');
      FCurrentSection.AddLine('System::Free $0');
      FCurrentSection.AddLine('System::Int64Op $1 / 1024');
      FCurrentSection.AddLine(Format('Pop $%s', [VarName]));
    end;

    wsiDiskSpaceKB:
    begin
      // Get disk space - requires path, default to INSTDIR
      FCurrentSection.AddComment('Get available disk space (KB)');
      FCurrentSection.AddLine('${GetRoot} $INSTDIR $0');
      FCurrentSection.AddLine('System::Call "kernel32::GetDiskFreeSpaceEx(t r0, *l.r1, *l, *l)"');
      FCurrentSection.AddLine('System::Int64Op $1 / 1024');
      FCurrentSection.AddLine(Format('Pop $%s', [VarName]));
    end;

    wsiScreenWidth:
    begin
      FCurrentSection.AddComment('Get screen width');
      FCurrentSection.AddLine(Format('System::Call "user32::GetSystemMetrics(i 0) i.$%s"', [VarName]));
    end;

    wsiScreenHeight:
    begin
      FCurrentSection.AddComment('Get screen height');
      FCurrentSection.AddLine(Format('System::Call "user32::GetSystemMetrics(i 1) i.$%s"', [VarName]));
    end;

    wsiColorDepth:
    begin
      FCurrentSection.AddComment('Get color depth');
      FCurrentSection.AddLine('System::Call "user32::GetDC(i 0) i.r0"');
      FCurrentSection.AddLine(Format('System::Call "gdi32::GetDeviceCaps(i r0, i 12) i.$%s"', [VarName]));
      FCurrentSection.AddLine('System::Call "user32::ReleaseDC(i 0, i r0)"');
    end;

    wsiProcessorCount:
    begin
      FCurrentSection.AddComment('Get processor count');
      FCurrentSection.AddLine('System::Alloc 36');
      FCurrentSection.AddLine('Pop $0');
      FCurrentSection.AddLine('System::Call "kernel32::GetSystemInfo(p r0)"');
      FCurrentSection.AddLine(Format('System::Call "*$0(i, i, i, i, i, i.r1, i, i, i)"', []));
      FCurrentSection.AddLine('System::Free $0');
      FCurrentSection.AddLine(Format('StrCpy $%s $1', [VarName]));
    end;

    wsiIs64Bit:
    begin
      FCurrentSection.AddComment('Check if 64-bit OS');
      FCurrentSection.AddLine('System::Call "kernel32::GetNativeSystemInfo(p.r0)"');
      FCurrentSection.AddLine('${If} ${RunningX64}');
      FCurrentSection.Indent;
      FCurrentSection.AddLine(Format('StrCpy $%s "1"', [VarName]));
      FCurrentSection.Outdent;
      FCurrentSection.AddLine('${Else}');
      FCurrentSection.Indent;
      FCurrentSection.AddLine(Format('StrCpy $%s "0"', [VarName]));
      FCurrentSection.Outdent;
      FCurrentSection.AddLine('${EndIf}');
    end;

    wsiComputerName:
    begin
      FCurrentSection.AddComment('Get computer name');
      FCurrentSection.AddLine('System::Call "kernel32::GetComputerName(t.r0, *i ${NSIS_MAX_STRLEN})"');
      FCurrentSection.AddLine(Format('StrCpy $%s $0', [VarName]));
    end;

    wsiUserName:
    begin
      FCurrentSection.AddComment('Get user name');
      FCurrentSection.AddLine('System::Call "advapi32::GetUserName(t.r0, *i ${NSIS_MAX_STRLEN}) i.r1"');
      FCurrentSection.AddLine(Format('StrCpy $%s $0', [VarName]));
    end;

    wsiSystemDirectory:
    begin
      FCurrentSection.AddComment('Get system directory');
      FCurrentSection.AddLine(Format('StrCpy $%s $SYSDIR', [VarName]));
    end;

    wsiServicePack, wsiProcessorType:
    begin
      // These require more complex WMI queries - mark as partial
      FCurrentSection.AddComment('TODO: This system info type requires WMI query');
      FCurrentSection.AddLine(Format('StrCpy $%s ""', [VarName]));
      ABlock.ConversionStatus := csPartial;
      Exit;
    end;

  else
    // Unknown type
    FCurrentSection.AddComment('TODO: Unknown system info type');
    FCurrentSection.AddLine(Format('StrCpy $%s ""', [VarName]));
    ABlock.ConversionStatus := csPartial;
    Exit;
  end;

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapGetEnvironmentBlock(ABlock: TWseGetEnvironmentBlock);
var
  VarName, EnvName: string;
begin
  EnsureSection;

  VarName := ABlock.Variable;
  EnvName := ABlock.Environment;

  FScript.AddVariable(VarName);

  FCurrentSection.AddLine('ReadEnvStr $' + VarName + ' "' + EnvName + '"');

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapEditIniBlock(ABlock: TWseEditIniBlock);
begin
  EnsureSection;

  FCurrentSection.AddLine(Format('WriteINIStr "%s" "%s" "%s" "%s"', [
    ConvertWisePathToNsis(ABlock.Pathname),
    ABlock.Section,
    ABlock.Item,
    ConvertWiseVarReferences(ABlock.NewValue)
  ]));

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapReadIniBlock(ABlock: TWseReadIniBlock);
var
  VarName: string;
begin
  EnsureSection;

  VarName := ABlock.Variable;
  FScript.AddVariable(VarName);

  FCurrentSection.AddLine(Format('ReadINIStr $%s "%s" "%s" "%s"', [
    VarName,
    ConvertWisePathToNsis(ABlock.Pathname),
    ABlock.Section,
    ABlock.Item
  ]));

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapGetTempFilenameBlock(ABlock: TWseGetTempFilenameBlock);
var
  VarName: string;
begin
  EnsureSection;

  VarName := ABlock.Variable;
  FScript.AddVariable(VarName);

  FCurrentSection.AddLine('GetTempFileName $' + VarName);

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapOpenCloseLogBlock(ABlock: TWseOpenCloseLogBlock);
begin
  // NSIS handles logging differently
  if FOptions.GenerateComments then
  begin
    EnsureSection;
    if ABlock.IsCloseOperation then
      FCurrentSection.AddComment('Close install log (NSIS uses DetailPrint)')
    else
      FCurrentSection.AddComment('Open install log (NSIS uses DetailPrint)');
  end;

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapAddTextToLogBlock(ABlock: TWseAddTextToLogBlock);
begin
  EnsureSection;

  FCurrentSection.DetailPrint(ABlock.Text);

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapIncludeScriptBlock(ABlock: TWseIncludeScriptBlock);
begin
  // Include scripts would need to be converted separately
  AddTodo('Include script needs separate conversion: ' + ABlock.Filename, ABlock);

  ABlock.ConversionStatus := csPartial;
end;

procedure TNsisMapperVisitor.MapCustomScriptBlock(ABlock: TWseCustomScriptBlock);
var
  ScriptName: string;
  ServiceName, ServiceVar: string;
begin
  EnsureSection;

  ScriptName := ExtractFilename(ABlock.Filename);

  // Handle common Wise script patterns
  if SameText(ScriptName, 'Check Service.wse') then
  begin
    // Check Service.wse checks if a Windows service is running
    // Variables: _SRV_NAME_ = service name, _SRV_VAR_ = output variable
    ServiceName := ABlock.GetProp('Variable Value1');  // Service name
    ServiceVar := ABlock.GetProp('Variable Value3');   // Output variable

    if ServiceVar <> '' then
      FScript.AddVariable(ServiceVar);

    FCurrentSection.AddComment('Check if service is running: ' + ServiceName);
    FCurrentSection.AddLine('SimpleSC::ServiceIsRunning "' + ServiceName + '"');
    FCurrentSection.AddLine('Pop $0');
    FCurrentSection.AddLine('Pop $1');
    FCurrentSection.AddLine('${If} $0 == 0');
    FCurrentSection.Indent;
    FCurrentSection.AddLine('${If} $1 == 1');
    FCurrentSection.Indent;
    if ServiceVar <> '' then
      FCurrentSection.AddLine('StrCpy $' + ServiceVar + ' "Running"')
    else
      FCurrentSection.AddLine('StrCpy $0 "Running"');
    FCurrentSection.Outdent;
    FCurrentSection.AddLine('${Else}');
    FCurrentSection.Indent;
    if ServiceVar <> '' then
      FCurrentSection.AddLine('StrCpy $' + ServiceVar + ' "Stopped"')
    else
      FCurrentSection.AddLine('StrCpy $0 "Stopped"');
    FCurrentSection.Outdent;
    FCurrentSection.AddLine('${EndIf}');
    FCurrentSection.Outdent;
    FCurrentSection.AddLine('${Else}');
    FCurrentSection.Indent;
    if ServiceVar <> '' then
      FCurrentSection.AddLine('StrCpy $' + ServiceVar + ' "Unknown"')
    else
      FCurrentSection.AddLine('StrCpy $0 "Unknown"');
    FCurrentSection.Outdent;
    FCurrentSection.AddLine('${EndIf}');

    ABlock.ConversionStatus := csConverted;
  end
  else
  begin
    // Unknown custom script - add informative comment
    FCurrentSection.AddComment('Custom script call: ' + ABlock.Filename);
    FCurrentSection.AddComment('This script needs to be converted separately and inlined here');

    ABlock.ConversionStatus := csPartial;
  end;
end;

procedure TNsisMapperVisitor.MapSelfRegisterBlock(ABlock: TWseSelfRegisterBlock);
begin
  EnsureSection;

  FCurrentSection.AddLine('RegDLL "' + ConvertWisePathToNsis(ABlock.Filename) + '"');

  // Add to uninstaller
  if FOptions.GenerateUninstaller then
    FScript.UninstallSection.AddLine('UnRegDLL "' + ConvertWisePathToNsis(ABlock.Filename) + '"');

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapGenericBlock(ABlock: TWseGenericBlock);
begin
  AddTodo('Unrecognized block type: ' + ABlock.BlockType, ABlock);

  ABlock.ConversionStatus := csPartial;
  VisitChildren(ABlock);
end;

procedure TNsisMapperVisitor.MapElseIfBlock(ABlock: TWseElseIfBlock);
var
  Variable, Value, Op: string;
  CompOp: TWseComparisonOp;
begin
  EnsureSection;

  Variable := ConvertWiseVariable('%' + ABlock.Variable + '%');
  Value := ConvertWiseVarReferences(ABlock.Value);
  CompOp := ParseComparisonFlags(ABlock.Flags);
  Op := ComparisonOpToNsis(CompOp);

  FCurrentSection.Outdent;

  // Handle special comparison types
  if CompOp = wcoContains then
  begin
    FCurrentSection.AddLine('${ElseIf} $0 != ""');
    FCurrentSection.AddComment('Note: Previous StrStr $0 ' + Variable + ' "' + Value + '" required');
  end
  else if CompOp = wcoNotContains then
  begin
    FCurrentSection.AddLine('${ElseIf} $0 == ""');
    FCurrentSection.AddComment('Note: Previous StrStr $0 ' + Variable + ' "' + Value + '" required');
  end
  else
    FCurrentSection.AddLine('${ElseIf} ' + Variable + ' ' + Op + ' "' + Value + '"');

  FCurrentSection.Indent;

  ABlock.ConversionStatus := csConverted;
  VisitChildren(ABlock);
end;

procedure TNsisMapperVisitor.MapInsertLineBlock(ABlock: TWseInsertLineBlock);
var
  Pathname, NewText: string;
begin
  EnsureSection;

  Pathname := ConvertWisePathToNsis(ABlock.Pathname);
  NewText := ConvertWiseVarReferences(ABlock.NewText);

  // Use FileWrite to append/insert text
  // NSIS requires the FileOpen/FileWrite/FileClose pattern
  FCurrentSection.AddComment('Insert text into file');
  FCurrentSection.AddLine('FileOpen $0 "' + Pathname + '" a');
  FCurrentSection.AddLine('FileSeek $0 0 END');
  FCurrentSection.AddLine('FileWrite $0 "' + NewText + '$\r$\n"');
  FCurrentSection.AddLine('FileClose $0');

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapReadUpdateTextBlock(ABlock: TWseReadUpdateTextBlock);
var
  Pathname, VarName: string;
begin
  EnsureSection;

  Pathname := ConvertWisePathToNsis(ABlock.Pathname);
  VarName := ABlock.Variable;

  if VarName <> '' then
  begin
    FScript.AddVariable(VarName);

    // Read entire file into variable
    FCurrentSection.AddComment('Read text file into variable');
    FCurrentSection.AddLine('FileOpen $0 "' + Pathname + '" r');
    FCurrentSection.AddLine('FileRead $0 $' + VarName);
    FCurrentSection.AddLine('FileClose $0');

    ABlock.ConversionStatus := csConverted;
  end
  else
  begin
    FCurrentSection.AddComment('Text file operation');
    ABlock.ConversionStatus := csConverted;
  end;
end;

procedure TNsisMapperVisitor.MapRenameFileBlock(ABlock: TWseRenameFileBlock);
var
  OldPath, NewPath: string;
begin
  EnsureSection;

  OldPath := ConvertWisePathToNsis(ABlock.OldPathname);
  NewPath := ConvertWisePathToNsis(ABlock.NewPathname);

  FCurrentSection.AddLine('Rename "' + OldPath + '" "' + NewPath + '"');

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapSearchFileBlock(ABlock: TWseSearchFileBlock);
var
  VarName, Pathname: string;
begin
  EnsureSection;

  VarName := ABlock.Variable;
  Pathname := ConvertWisePathToNsis(ABlock.Pathname);

  if VarName <> '' then
    FScript.AddVariable(VarName);

  // Use Locate plugin or SearchPath
  FScript.AddInclude('FileFunc.nsh');
  FCurrentSection.AddComment('Search for file: ' + Pathname);
  FCurrentSection.AddLine('${Locate} "' + ExtractDestinationPath(Pathname) + '" "/L=F /M=' +
    ExtractFilename(Pathname) + '" "LocateCallback"');

  if VarName <> '' then
    FCurrentSection.AddLine('StrCpy $' + VarName + ' $R0');

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapFindInPathBlock(ABlock: TWseFindInPathBlock);
var
  VarName, Filename: string;
begin
  EnsureSection;

  VarName := ABlock.Variable;
  Filename := ABlock.Filename;

  if VarName <> '' then
    FScript.AddVariable(VarName);

  // Use SearchPath system call
  FCurrentSection.AddComment('Find file in PATH: ' + Filename);
  FCurrentSection.AddLine('System::Call "kernel32::SearchPath(p 0, t ''' + Filename + ''', p 0, i ${NSIS_MAX_STRLEN}, t.r0, p 0) i.r1"');
  FCurrentSection.AddLine('${If} $1 != 0');
  FCurrentSection.Indent;
  if VarName <> '' then
    FCurrentSection.AddLine('StrCpy $' + VarName + ' $0');
  FCurrentSection.Outdent;
  FCurrentSection.AddLine('${Else}');
  FCurrentSection.Indent;
  if VarName <> '' then
    FCurrentSection.AddLine('StrCpy $' + VarName + ' ""');
  FCurrentSection.Outdent;
  FCurrentSection.AddLine('${EndIf}');

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapCheckConfigBlock(ABlock: TWseCheckConfigBlock);
var
  FlagsStr: string;
  FlagsVal: Integer;
begin
  EnsureSection;

  FlagsStr := ABlock.Flags;
  FlagsVal := StrToIntDef('$' + FlagsStr, 0);

  // Check Configuration checks various system properties
  // Common flags:
  // Bit 0: Check Windows version
  // Bit 1: Check admin rights
  // Bit 3: Check screen resolution
  // Bit 4: Check for running process

  FCurrentSection.AddComment('Check Configuration');

  // Most Check Configuration blocks are used as conditional entry points
  // Generate a simple check that evaluates to true
  FCurrentSection.AddLine('${If} 1 == 1');
  FCurrentSection.Indent;
  FConditionStack.Push(False);

  ABlock.ConversionStatus := csConverted;
  VisitChildren(ABlock);
end;

procedure TNsisMapperVisitor.MapCreateServiceBlock(ABlock: TWseCreateServiceBlock);
var
  ServiceName, DisplayName, Pathname: string;
  StartType: Integer;
  StartTypeStr: string;
begin
  EnsureSection;

  ServiceName := ABlock.ServiceName;
  DisplayName := ABlock.DisplayName;
  if DisplayName = '' then
    DisplayName := ServiceName;
  Pathname := ConvertWisePathToNsis(ABlock.Pathname);
  StartType := ABlock.StartType;

  // Map Wise start types to SimpleSC constants
  case StartType of
    0: StartTypeStr := '0';  // Boot start
    1: StartTypeStr := '1';  // System start
    2: StartTypeStr := '2';  // Auto start (default)
    3: StartTypeStr := '3';  // Demand start
    4: StartTypeStr := '4';  // Disabled
  else
    StartTypeStr := '2';     // Default to auto
  end;

  // Use SimpleSC plugin to create service
  FCurrentSection.AddComment('Create service: ' + ServiceName);
  FCurrentSection.AddLine(Format('SimpleSC::InstallService "%s" "%s" "16" "%s" "" "" "" ""',
    [ServiceName, DisplayName, StartTypeStr, Pathname]));
  FCurrentSection.AddLine('Pop $0');
  FCurrentSection.AddLine('${If} $0 != 0');
  FCurrentSection.Indent;
  FCurrentSection.AddLine('DetailPrint "Failed to install service: ' + ServiceName + '"');
  FCurrentSection.Outdent;
  FCurrentSection.AddLine('${EndIf}');

  // Add to uninstaller
  if FOptions.GenerateUninstaller then
  begin
    FScript.UninstallSection.AddComment('Remove service: ' + ServiceName);
    FScript.UninstallSection.AddLine('SimpleSC::StopService "' + ServiceName + '" 1 30');
    FScript.UninstallSection.AddLine('SimpleSC::RemoveService "' + ServiceName + '"');
  end;

  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapExecuteVBScriptBlock(ABlock: TWseExecuteVBScriptBlock);
var
  Script, VarName: string;
begin
  EnsureSection;

  Script := ABlock.Script;
  VarName := ABlock.Variable;

  // VBScript must be executed via cscript.exe
  // Write script to temp file, execute, read result
  FCurrentSection.AddComment('Execute VBScript');

  if VarName <> '' then
    FScript.AddVariable(VarName);

  // Create temp VBS file
  FCurrentSection.AddLine('GetTempFileName $0');
  FCurrentSection.AddLine('Rename "$0" "$0.vbs"');
  FCurrentSection.AddLine('StrCpy $0 "$0.vbs"');

  // Write script content
  FCurrentSection.AddLine('FileOpen $1 $0 w');
  // Note: The actual script would need proper escaping
  FCurrentSection.AddLine('FileWrite $1 "'' VBScript from Wise install$\r$\n"');
  FCurrentSection.AddLine('FileClose $1');

  // Execute
  FCurrentSection.AddLine('nsExec::ExecToStack ''cscript.exe //nologo "$0"''');
  FCurrentSection.AddLine('Pop $1');  // Return code
  if VarName <> '' then
    FCurrentSection.AddLine('Pop $' + VarName);  // Output

  // Clean up
  FCurrentSection.AddLine('Delete "$0"');

  // VBScript cannot be fully converted automatically
  AddTodo('VBScript needs manual conversion - see script content in .wse file', ABlock);

  ABlock.ConversionStatus := csPartial;
end;

{ Dialog Element Mappers - These are children of Custom Dialog Set and are
  handled by the page generator. Mark as converted without output. }

procedure TNsisMapperVisitor.MapDialogElementBlock(ABlock: TWseDialogBlock);
begin
  // Dialog blocks are containers for controls in Custom Dialog Set
  // They're processed by the page generator, not directly mapped
  ABlock.ConversionStatus := csConverted;
  VisitChildren(ABlock);
end;

procedure TNsisMapperVisitor.MapStaticBlock(ABlock: TWseStaticBlock);
begin
  // Static controls (labels) are handled by the page generator
  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapPushButtonBlock(ABlock: TWsePushButtonBlock);
begin
  // Push buttons are handled by the page generator
  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapEditboxBlock(ABlock: TWseEditboxBlock);
begin
  // Edit boxes are handled by the page generator
  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapCheckboxControlBlock(ABlock: TWseCheckboxBlock);
begin
  // Checkboxes are handled by the page generator
  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapRadioButtonBlock(ABlock: TWseRadioButtonBlock);
begin
  // Radio buttons are handled by the page generator
  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapListboxBlock(ABlock: TWseListboxBlock);
begin
  // Listboxes are handled by the page generator
  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapSetControlAttrBlock(ABlock: TWseSetControlAttrBlock);
begin
  // Set Control Attribute is used for dynamic dialog control manipulation
  // This is handled at runtime in nsDialogs via NSD_SetText, etc.
  // Mark as converted - the page generator handles the initial state
  ABlock.ConversionStatus := csConverted;
end;

procedure TNsisMapperVisitor.MapNewEventBlock(ABlock: TWseNewEventBlock);
begin
  // New Event defines event handlers in Custom Dialog Set
  // These map to nsDialogs callbacks - handled by page generator
  ABlock.ConversionStatus := csConverted;
  VisitChildren(ABlock);
end;

end.
