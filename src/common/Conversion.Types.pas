unit Conversion.Types;

{******************************************************************************
  WiseToNSIS Converter - Common Types

  Defines shared types, enumerations, and constants used across the converter.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  { Conversion severity levels }
  TConversionSeverity = (
    csInfo,       // Informational message
    csWarning,    // Potential issue, conversion proceeds
    csError,      // Conversion error, item may be skipped
    csCritical    // Critical error, conversion cannot proceed
  );

  { Conversion issue category }
  TIssueCategory = (
    icParsing,         // Parser-related issues
    icMapping,         // Command mapping issues
    icUnsupported,     // Unsupported feature
    icAmbiguous,       // Requires user review
    icDeprecated,      // Deprecated feature warning
    icGeneration       // NSIS generation issues
  );

  { Conversion status for individual items }
  TConversionStatus = (
    csNotConverted,    // Not yet processed
    csConverted,       // Successfully converted
    csPartial,         // Partially converted, needs review
    csSkipped,         // Skipped (remarked or disabled)
    csFailed           // Conversion failed
  );

  { NSIS root registry key }
  TNsisRootKey = (
    nrkHKCR,  // HKEY_CLASSES_ROOT
    nrkHKCU,  // HKEY_CURRENT_USER
    nrkHKLM,  // HKEY_LOCAL_MACHINE
    nrkHKU    // HKEY_USERS
  );

  { Variable scope }
  TVariableScope = (
    vsGlobal,     // Global variable (defined in Global block)
    vsLocal,      // Local variable (script-level)
    vsSystem      // System variable (predefined)
  );

  { Forward declarations }
  TConversionIssue = class;
  TConversionIssueList = class;

  { Conversion issue - represents a single issue found during conversion }
  TConversionIssue = class
  private
    FSeverity: TConversionSeverity;
    FCategory: TIssueCategory;
    FMessage: string;
    FSourceLine: Integer;
    FSourceColumn: Integer;
    FBlockType: string;
    FSuggestedFix: string;
    FContext: string;
  public
    constructor Create(ASeverity: TConversionSeverity; ACategory: TIssueCategory;
      const AMessage: string; ASourceLine: Integer = 0; ASourceColumn: Integer = 0);

    property Severity: TConversionSeverity read FSeverity;
    property Category: TIssueCategory read FCategory;
    property Message: string read FMessage;
    property SourceLine: Integer read FSourceLine;
    property SourceColumn: Integer read FSourceColumn;
    property BlockType: string read FBlockType write FBlockType;
    property SuggestedFix: string read FSuggestedFix write FSuggestedFix;
    property Context: string read FContext write FContext;
  end;

  { List of conversion issues }
  TConversionIssueList = class(TObjectList<TConversionIssue>)
  public
    procedure AddIssue(ASeverity: TConversionSeverity; ACategory: TIssueCategory;
      const AMessage: string; ASourceLine: Integer = 0; ASourceColumn: Integer = 0);
    function HasErrors: Boolean;
    function HasCritical: Boolean;
    function ErrorCount: Integer;
    function WarningCount: Integer;
  end;

  { Variable information }
  TVariableInfo = class
  private
    FName: string;
    FDefaultValue: string;
    FFlags: string;
    FScope: TVariableScope;
    FIndex: Integer;
  public
    constructor Create(const AName, ADefaultValue, AFlags: string;
      AScope: TVariableScope; AIndex: Integer);

    property Name: string read FName;
    property DefaultValue: string read FDefaultValue;
    property Flags: string read FFlags;
    property Scope: TVariableScope read FScope;
    property Index: Integer read FIndex;
  end;

  { Variable dictionary }
  TVariableDictionary = class(TObjectDictionary<string, TVariableInfo>)
  public
    constructor Create;
    procedure AddVariable(const AName, ADefaultValue, AFlags: string;
      AScope: TVariableScope; AIndex: Integer = 0);
    function GetVariable(const AName: string): TVariableInfo;
    function VariableExists(const AName: string): Boolean;
  end;

const
  { Wise system variables mapping to NSIS }
  WiseSystemVars: array[0..9] of record
    WiseName: string;
    NsisName: string;
    Description: string;
  end = (
    (WiseName: '_SYS_';         NsisName: '$SYSDIR';        Description: 'System32 directory'),
    (WiseName: '_WIN_';         NsisName: '$WINDIR';        Description: 'Windows directory'),
    (WiseName: 'MAINDIR';       NsisName: '$INSTDIR';       Description: 'Installation directory'),
    (WiseName: 'PROGRAM_FILES'; NsisName: '$PROGRAMFILES';  Description: 'Program Files'),
    (WiseName: 'COMMON';        NsisName: '$COMMONFILES';   Description: 'Common Files'),
    (WiseName: 'TEMP';          NsisName: '$TEMP';          Description: 'Temp directory'),
    (WiseName: 'GROUPDIR';      NsisName: '$SMPROGRAMS';    Description: 'Start Menu Programs'),
    (WiseName: 'INST';          NsisName: '$EXEDIR';        Description: 'Installer directory'),
    (WiseName: 'CMDLINE';       NsisName: '$CMDLINE';       Description: 'Command line arguments'),
    (WiseName: 'DESKTOP';       NsisName: '$DESKTOP';       Description: 'Desktop directory')
  );

  { Wise registry root key values }
  WiseRootKeys: array[0..3] of record
    WiseValue: Integer;
    NsisKey: string;
  end = (
    (WiseValue: 0; NsisKey: 'HKCR'),
    (WiseValue: 1; NsisKey: 'HKCU'),
    (WiseValue: 2; NsisKey: 'HKLM'),
    (WiseValue: 3; NsisKey: 'HKU')
  );

  { Severity names }
  SeverityNames: array[TConversionSeverity] of string = (
    'Info', 'Warning', 'Error', 'Critical'
  );

  { Category names }
  CategoryNames: array[TIssueCategory] of string = (
    'Parsing', 'Mapping', 'Unsupported', 'Ambiguous', 'Deprecated', 'Generation'
  );

{ Helper functions }
function SeverityToStr(ASeverity: TConversionSeverity): string;
function CategoryToStr(ACategory: TIssueCategory): string;
function WiseRootToNsis(AWiseRoot: Integer): string;
function ConvertWiseVariable(const AWiseVar: string): string;
function IsSystemVariable(const AVarName: string): Boolean;

implementation

{ Helper functions }

function SeverityToStr(ASeverity: TConversionSeverity): string;
begin
  Result := SeverityNames[ASeverity];
end;

function CategoryToStr(ACategory: TIssueCategory): string;
begin
  Result := CategoryNames[ACategory];
end;

function WiseRootToNsis(AWiseRoot: Integer): string;
begin
  if (AWiseRoot >= Low(WiseRootKeys)) and (AWiseRoot <= High(WiseRootKeys)) then
    Result := WiseRootKeys[AWiseRoot].NsisKey
  else
    Result := 'HKLM';  // Default to HKLM
end;

function ConvertWiseVariable(const AWiseVar: string): string;
var
  i: Integer;
  VarName: string;
begin
  // Remove % delimiters if present
  VarName := AWiseVar;
  if (Length(VarName) > 1) and (VarName[1] = '%') then
    VarName := Copy(VarName, 2, Length(VarName) - 2);

  // Check for system variable mapping
  for i := Low(WiseSystemVars) to High(WiseSystemVars) do
  begin
    if SameText(WiseSystemVars[i].WiseName, VarName) then
    begin
      Result := WiseSystemVars[i].NsisName;
      Exit;
    end;
  end;

  // Custom variable - prefix with $
  Result := '$' + VarName;
end;

function IsSystemVariable(const AVarName: string): Boolean;
var
  i: Integer;
begin
  for i := Low(WiseSystemVars) to High(WiseSystemVars) do
  begin
    if SameText(WiseSystemVars[i].WiseName, AVarName) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

{ TConversionIssue }

constructor TConversionIssue.Create(ASeverity: TConversionSeverity;
  ACategory: TIssueCategory; const AMessage: string; ASourceLine, ASourceColumn: Integer);
begin
  inherited Create;
  FSeverity := ASeverity;
  FCategory := ACategory;
  FMessage := AMessage;
  FSourceLine := ASourceLine;
  FSourceColumn := ASourceColumn;
  FBlockType := '';
  FSuggestedFix := '';
  FContext := '';
end;

{ TConversionIssueList }

procedure TConversionIssueList.AddIssue(ASeverity: TConversionSeverity;
  ACategory: TIssueCategory; const AMessage: string; ASourceLine, ASourceColumn: Integer);
begin
  Add(TConversionIssue.Create(ASeverity, ACategory, AMessage, ASourceLine, ASourceColumn));
end;

function TConversionIssueList.HasErrors: Boolean;
var
  Issue: TConversionIssue;
begin
  for Issue in Self do
    if Issue.Severity in [csError, csCritical] then
      Exit(True);
  Result := False;
end;

function TConversionIssueList.HasCritical: Boolean;
var
  Issue: TConversionIssue;
begin
  for Issue in Self do
    if Issue.Severity = csCritical then
      Exit(True);
  Result := False;
end;

function TConversionIssueList.ErrorCount: Integer;
var
  Issue: TConversionIssue;
begin
  Result := 0;
  for Issue in Self do
    if Issue.Severity in [csError, csCritical] then
      Inc(Result);
end;

function TConversionIssueList.WarningCount: Integer;
var
  Issue: TConversionIssue;
begin
  Result := 0;
  for Issue in Self do
    if Issue.Severity = csWarning then
      Inc(Result);
end;

{ TVariableInfo }

constructor TVariableInfo.Create(const AName, ADefaultValue, AFlags: string;
  AScope: TVariableScope; AIndex: Integer);
begin
  inherited Create;
  FName := AName;
  FDefaultValue := ADefaultValue;
  FFlags := AFlags;
  FScope := AScope;
  FIndex := AIndex;
end;

{ TVariableDictionary }

constructor TVariableDictionary.Create;
begin
  inherited Create([doOwnsValues]);
end;

procedure TVariableDictionary.AddVariable(const AName, ADefaultValue, AFlags: string;
  AScope: TVariableScope; AIndex: Integer);
begin
  if not ContainsKey(UpperCase(AName)) then
    Add(UpperCase(AName), TVariableInfo.Create(AName, ADefaultValue, AFlags, AScope, AIndex));
end;

function TVariableDictionary.GetVariable(const AName: string): TVariableInfo;
begin
  if not TryGetValue(UpperCase(AName), Result) then
    Result := nil;
end;

function TVariableDictionary.VariableExists(const AName: string): Boolean;
begin
  Result := ContainsKey(UpperCase(AName));
end;

end.
