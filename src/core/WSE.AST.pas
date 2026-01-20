unit WSE.AST;

{******************************************************************************
  WiseToNSIS Converter - Abstract Syntax Tree

  Defines the AST node hierarchy for representing parsed Wise Install scripts.
  Design inspired by WAnt's TScriptElement hierarchy.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Conversion.Types;

type
  { Forward declarations }
  TWseNode = class;
  TWseDocument = class;
  TWseBlock = class;
  TWseNodeClass = class of TWseNode;

  { Visitor interface for AST traversal }
  IWseVisitor = interface
    ['{B7E8F1A2-3C4D-5E6F-7A8B-9C0D1E2F3A4B}']
    procedure VisitDocument(ANode: TWseDocument);
    procedure VisitBlock(ANode: TWseBlock);
  end;

  { Base AST node }
  TWseNode = class
  private
    FParent: TWseNode;
    FChildren: TObjectList<TWseNode>;
    FLine: Integer;
    FColumn: Integer;
    FIsRemarked: Boolean;
    FConversionStatus: TConversionStatus;
    FNsisOutput: string;
    function GetChild(Index: Integer): TWseNode;
    function GetChildCount: Integer;
  protected
    procedure SetParent(AParent: TWseNode);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    { Child management }
    procedure AddChild(AChild: TWseNode);
    procedure RemoveChild(AChild: TWseNode);
    procedure ClearChildren;
    function FindChildByType(AClass: TWseNodeClass): TWseNode;
    function FindAllChildrenByType(AClass: TWseNodeClass): TArray<TWseNode>;

    { Visitor pattern }
    procedure Accept(AVisitor: IWseVisitor); virtual;

    { Properties }
    property Parent: TWseNode read FParent;
    property Children[Index: Integer]: TWseNode read GetChild;
    property ChildCount: Integer read GetChildCount;
    property Line: Integer read FLine write FLine;
    property Column: Integer read FColumn write FColumn;
    property IsRemarked: Boolean read FIsRemarked write FIsRemarked;
    property ConversionStatus: TConversionStatus read FConversionStatus write FConversionStatus;
    property NsisOutput: string read FNsisOutput write FNsisOutput;
  end;

  { Document root node }
  TWseDocument = class(TWseNode)
  private
    FDocumentType: string;
    FFilePath: string;
    FVariables: TVariableDictionary;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Accept(AVisitor: IWseVisitor); override;

    { Find the Global block }
    function GetGlobalBlock: TWseBlock;

    property DocumentType: string read FDocumentType write FDocumentType;
    property FilePath: string read FFilePath write FFilePath;
    property Variables: TVariableDictionary read FVariables;
  end;

  { Property key-value pair }
  TWseProperty = class
  private
    FKey: string;
    FValue: string;
    FLanguage: string;  // For multi-language text (e.g., 'French', 'German')
  public
    constructor Create(const AKey, AValue: string; const ALanguage: string = '');

    property Key: string read FKey;
    property Value: string read FValue write FValue;
    property Language: string read FLanguage;
  end;

  { Property list }
  TWsePropertyList = class(TObjectList<TWseProperty>)
  public
    function GetValue(const AKey: string): string;
    function GetValueWithLanguage(const AKey, ALanguage: string): string;
    function TryGetValue(const AKey: string; out AValue: string): Boolean;
    function HasKey(const AKey: string): Boolean;
    procedure SetValue(const AKey, AValue: string);
    function GetInteger(const AKey: string; ADefault: Integer = 0): Integer;
    function GetBoolean(const AKey: string; ADefault: Boolean = False): Boolean;
    function GetAllValues(const AKey: string): TArray<string>;
  end;

  { Base block class - represents an item: ... end block }
  TWseBlock = class(TWseNode)
  private
    FBlockType: string;
    FProperties: TWsePropertyList;
  protected
    function GetTagName: string; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Accept(AVisitor: IWseVisitor); override;

    { Property access shortcuts }
    function GetProp(const AKey: string): string;
    function GetPropInt(const AKey: string; ADefault: Integer = 0): Integer;
    function HasProp(const AKey: string): Boolean;

    { Class method for tag name synthesis (like WAnt) }
    class function TagName: string; virtual;

    property BlockType: string read FBlockType write FBlockType;
    property Properties: TWsePropertyList read FProperties;
  end;

  { Global block - document metadata and variables }
  TWseGlobalBlock = class(TWseBlock)
  private
    FVersion: string;
    FTitle: string;
    FFlags: string;
    FExeFilename: string;
    FRequestedExecutionLevel: string;
    FLogPathname: string;
    FVariableCount: Integer;
  public
    constructor Create; override;

    class function TagName: string; override;

    { Extract variables from Global block properties }
    procedure ExtractVariables(AVarDict: TVariableDictionary);

    property Version: string read FVersion write FVersion;
    property Title: string read FTitle write FTitle;
    property Flags: string read FFlags write FFlags;
    property ExeFilename: string read FExeFilename write FExeFilename;
    property RequestedExecutionLevel: string read FRequestedExecutionLevel write FRequestedExecutionLevel;
    property LogPathname: string read FLogPathname write FLogPathname;
    property VariableCount: Integer read FVariableCount write FVariableCount;
  end;

  { Set Variable block }
  TWseSetVariableBlock = class(TWseBlock)
  private
    function GetVariable: string;
    function GetValue: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    property Variable: string read GetVariable;
    property Value: string read GetValue;
    property Flags: string read GetFlags;
  end;

  { If/While Statement block }
  TWseIfWhileBlock = class(TWseBlock)
  private
    function GetVariable: string;
    function GetValue: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    function IsWhileLoop: Boolean;
    function GetComparisonOperator: string;

    property Variable: string read GetVariable;
    property Value: string read GetValue;
    property Flags: string read GetFlags;
  end;

  { Else Statement block }
  TWseElseBlock = class(TWseBlock)
  public
    class function TagName: string; override;
  end;

  { End Block }
  TWseEndBlock = class(TWseBlock)
  public
    class function TagName: string; override;
  end;

  { Install File block }
  TWseInstallFileBlock = class(TWseBlock)
  private
    function GetSource: string;
    function GetDestination: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    function IsRecursive: Boolean;

    property Source: string read GetSource;
    property Destination: string read GetDestination;
    property Flags: string read GetFlags;
  end;

  { Edit Registry block }
  TWseEditRegistryBlock = class(TWseBlock)
  private
    function GetKey: string;
    function GetValueName: string;
    function GetNewValue: string;
    function GetRoot: Integer;
    function GetFlags: string;
    function GetTotalKeys: Integer;
  public
    class function TagName: string; override;

    function GetNsisRootKey: string;
    function IsDeleteOperation: Boolean;

    property Key: string read GetKey;
    property ValueName: string read GetValueName;
    property NewValue: string read GetNewValue;
    property Root: Integer read GetRoot;
    property Flags: string read GetFlags;
    property TotalKeys: Integer read GetTotalKeys;
  end;

  { Get Registry Key Value block }
  TWseGetRegistryBlock = class(TWseBlock)
  private
    function GetVariable: string;
    function GetKey: string;
    function GetValueName: string;
    function GetDefaultValue: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    property Variable: string read GetVariable;
    property Key: string read GetKey;
    property ValueName: string read GetValueName;
    property DefaultValue: string read GetDefaultValue;
    property Flags: string read GetFlags;
  end;

  { Display Message block }
  TWseDisplayMessageBlock = class(TWseBlock)
  private
    function GetTitle: string;
    function GetText: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    function GetMessageBoxType: string;

    property Title: string read GetTitle;
    property Text: string read GetText;
    property Flags: string read GetFlags;
  end;

  { Execute Program block }
  TWseExecuteProgramBlock = class(TWseBlock)
  private
    function GetPathname: string;
    function GetCommandLine: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    function WaitForCompletion: Boolean;

    property Pathname: string read GetPathname;
    property CommandLine: string read GetCommandLine;
    property Flags: string read GetFlags;
  end;

  { Start/Stop Service block }
  TWseServiceBlock = class(TWseBlock)
  private
    function GetServiceName: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    function IsStartOperation: Boolean;

    property ServiceName: string read GetServiceName;
    property Flags: string read GetFlags;
  end;

  { Create Shortcut block }
  TWseCreateShortcutBlock = class(TWseBlock)
  private
    function GetSource: string;
    function GetDestination: string;
    function GetWorkingDirectory: string;
    function GetCommandLine: string;
    function GetIconPathname: string;
    function GetIconIndex: Integer;
  public
    class function TagName: string; override;

    property Source: string read GetSource;
    property Destination: string read GetDestination;
    property WorkingDirectory: string read GetWorkingDirectory;
    property CommandLine: string read GetCommandLine;
    property IconPathname: string read GetIconPathname;
    property IconIndex: Integer read GetIconIndex;
  end;

  { Remark block }
  TWseRemarkBlock = class(TWseBlock)
  private
    function GetText: string;
  public
    class function TagName: string; override;

    property Text: string read GetText;
  end;

  { Check if File/Dir Exists block }
  TWseCheckExistsBlock = class(TWseBlock)
  private
    function GetPathname: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    function IsDirectoryCheck: Boolean;
    function IsNegatedCheck: Boolean;

    property Pathname: string read GetPathname;
    property Flags: string read GetFlags;
  end;

  { Create Directory block }
  TWseCreateDirectoryBlock = class(TWseBlock)
  private
    function GetPathname: string;
  public
    class function TagName: string; override;

    property Pathname: string read GetPathname;
  end;

  { Delete File block }
  TWseDeleteFileBlock = class(TWseBlock)
  private
    function GetPathname: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    property Pathname: string read GetPathname;
    property Flags: string read GetFlags;
  end;

  { Copy Local File block }
  TWseCopyFileBlock = class(TWseBlock)
  private
    function GetSource: string;
    function GetDestination: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    property Source: string read GetSource;
    property Destination: string read GetDestination;
    property Flags: string read GetFlags;
  end;

  { Exit Installation block }
  TWseExitBlock = class(TWseBlock)
  public
    class function TagName: string; override;
  end;

  { Wizard Block }
  TWseWizardBlock = class(TWseBlock)
  private
    function GetDirectionVariable: string;
    function GetDisplayVariable: string;
    function GetBitmapPathname: string;
  public
    class function TagName: string; override;

    property DirectionVariable: string read GetDirectionVariable;
    property DisplayVariable: string read GetDisplayVariable;
    property BitmapPathname: string read GetBitmapPathname;
  end;

  { Custom Dialog Set block }
  TWseCustomDialogBlock = class(TWseBlock)
  private
    function GetName: string;
    function GetDisplayVariable: string;
  public
    class function TagName: string; override;

    property Name: string read GetName;
    property DisplayVariable: string read GetDisplayVariable;
  end;

  { Parse String block }
  TWseParseStringBlock = class(TWseBlock)
  private
    function GetSource: string;
    function GetPattern: string;
    function GetVariable1: string;
    function GetVariable2: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    property Source: string read GetSource;
    property Pattern: string read GetPattern;
    property Variable1: string read GetVariable1;
    property Variable2: string read GetVariable2;
    property Flags: string read GetFlags;
  end;

  { Get System Information block }
  TWseGetSystemInfoBlock = class(TWseBlock)
  private
    function GetVariable: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    property Variable: string read GetVariable;
    property Flags: string read GetFlags;
  end;

  { Get Environment Variable block }
  TWseGetEnvironmentBlock = class(TWseBlock)
  private
    function GetVariable: string;
    function GetEnvironment: string;
  public
    class function TagName: string; override;

    property Variable: string read GetVariable;
    property Environment: string read GetEnvironment;
  end;

  { Edit INI File block }
  TWseEditIniBlock = class(TWseBlock)
  private
    function GetPathname: string;
    function GetSection: string;
    function GetItem: string;
    function GetNewValue: string;
  public
    class function TagName: string; override;

    property Pathname: string read GetPathname;
    property Section: string read GetSection;
    property Item: string read GetItem;
    property NewValue: string read GetNewValue;
  end;

  { Read INI Value block }
  TWseReadIniBlock = class(TWseBlock)
  private
    function GetVariable: string;
    function GetPathname: string;
    function GetSection: string;
    function GetItem: string;
  public
    class function TagName: string; override;

    property Variable: string read GetVariable;
    property Pathname: string read GetPathname;
    property Section: string read GetSection;
    property Item: string read GetItem;
  end;

  { Get Temporary Filename block }
  TWseGetTempFilenameBlock = class(TWseBlock)
  private
    function GetVariable: string;
  public
    class function TagName: string; override;

    property Variable: string read GetVariable;
  end;

  { Open/Close INSTALL.LOG block }
  TWseOpenCloseLogBlock = class(TWseBlock)
  private
    function GetFlags: string;
  public
    class function TagName: string; override;

    function IsCloseOperation: Boolean;

    property Flags: string read GetFlags;
  end;

  { Add Text to INSTALL.LOG block }
  TWseAddTextToLogBlock = class(TWseBlock)
  private
    function GetText: string;
  public
    class function TagName: string; override;

    property Text: string read GetText;
  end;

  { Include Script block }
  TWseIncludeScriptBlock = class(TWseBlock)
  private
    function GetFilename: string;
  public
    class function TagName: string; override;

    property Filename: string read GetFilename;
  end;

  { Custom Script Item block }
  TWseCustomScriptBlock = class(TWseBlock)
  private
    function GetFilename: string;
  public
    class function TagName: string; override;

    property Filename: string read GetFilename;
  end;

  { Self-Register OCXs/DLLs block }
  TWseSelfRegisterBlock = class(TWseBlock)
  private
    function GetFilename: string;
  public
    class function TagName: string; override;

    property Filename: string read GetFilename;
  end;

  { ElseIf Statement block }
  TWseElseIfBlock = class(TWseBlock)
  private
    function GetVariable: string;
    function GetValue: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    property Variable: string read GetVariable;
    property Value: string read GetValue;
    property Flags: string read GetFlags;
  end;

  { Insert Line into Text File block }
  TWseInsertLineBlock = class(TWseBlock)
  private
    function GetPathname: string;
    function GetNewText: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    property Pathname: string read GetPathname;
    property NewText: string read GetNewText;
    property Flags: string read GetFlags;
  end;

  { Read/Update Text File block }
  TWseReadUpdateTextBlock = class(TWseBlock)
  private
    function GetPathname: string;
    function GetVariable: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    property Pathname: string read GetPathname;
    property Variable: string read GetVariable;
    property Flags: string read GetFlags;
  end;

  { Rename File/Directory block }
  TWseRenameFileBlock = class(TWseBlock)
  private
    function GetOldPathname: string;
    function GetNewPathname: string;
  public
    class function TagName: string; override;

    property OldPathname: string read GetOldPathname;
    property NewPathname: string read GetNewPathname;
  end;

  { Search for File block }
  TWseSearchFileBlock = class(TWseBlock)
  private
    function GetVariable: string;
    function GetPathname: string;
    function GetFlags: string;
  public
    class function TagName: string; override;

    property Variable: string read GetVariable;
    property Pathname: string read GetPathname;
    property Flags: string read GetFlags;
  end;

  { Find File in Path block }
  TWseFindInPathBlock = class(TWseBlock)
  private
    function GetVariable: string;
    function GetFilename: string;
    function GetSearchPath: string;
  public
    class function TagName: string; override;

    property Variable: string read GetVariable;
    property Filename: string read GetFilename;
    property SearchPath: string read GetSearchPath;
  end;

  { Check Configuration block }
  TWseCheckConfigBlock = class(TWseBlock)
  private
    function GetFlags: string;
  public
    class function TagName: string; override;

    property Flags: string read GetFlags;
  end;

  { Create Service block }
  TWseCreateServiceBlock = class(TWseBlock)
  private
    function GetServiceName: string;
    function GetDisplayName: string;
    function GetPathname: string;
    function GetStartType: Integer;
    function GetFlags: string;
  public
    class function TagName: string; override;

    property ServiceName: string read GetServiceName;
    property DisplayName: string read GetDisplayName;
    property Pathname: string read GetPathname;
    property StartType: Integer read GetStartType;
    property Flags: string read GetFlags;
  end;

  { Execute VBScript block }
  TWseExecuteVBScriptBlock = class(TWseBlock)
  private
    function GetScript: string;
    function GetVariable: string;
  public
    class function TagName: string; override;

    property Script: string read GetScript;
    property Variable: string read GetVariable;
  end;

  { Generic block for unrecognized block types }
  TWseGenericBlock = class(TWseBlock)
  public
    class function TagName: string; override;
  end;

  { Dialog element block (nested in Custom Dialog Set) }
  TWseDialogBlock = class(TWseBlock)
  private
    function GetTitle: string;
    function GetWidth: Integer;
    function GetHeight: Integer;
  public
    class function TagName: string; override;

    property Title: string read GetTitle;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  { Static control element }
  TWseStaticBlock = class(TWseBlock)
  private
    function GetRectangle: string;
    function GetText: string;
  public
    class function TagName: string; override;

    property Rectangle: string read GetRectangle;
    property Text: string read GetText;
  end;

  { Push Button element }
  TWsePushButtonBlock = class(TWseBlock)
  private
    function GetRectangle: string;
    function GetVariable: string;
    function GetValue: string;
    function GetText: string;
    function GetAction: Integer;
  public
    class function TagName: string; override;

    property Rectangle: string read GetRectangle;
    property Variable: string read GetVariable;
    property Value: string read GetValue;
    property Text: string read GetText;
    property Action: Integer read GetAction;
  end;

  { Editbox element }
  TWseEditboxBlock = class(TWseBlock)
  private
    function GetRectangle: string;
    function GetVariable: string;
    function GetValue: string;
  public
    class function TagName: string; override;

    property Rectangle: string read GetRectangle;
    property Variable: string read GetVariable;
    property Value: string read GetValue;
  end;

  { Checkbox element }
  TWseCheckboxBlock = class(TWseBlock)
  private
    function GetRectangle: string;
    function GetVariable: string;
    function GetText: string;
  public
    class function TagName: string; override;

    property Rectangle: string read GetRectangle;
    property Variable: string read GetVariable;
    property Text: string read GetText;
  end;

  { Radio Button element }
  TWseRadioButtonBlock = class(TWseBlock)
  private
    function GetRectangle: string;
    function GetVariable: string;
    function GetText: string;
    function GetValue: string;
  public
    class function TagName: string; override;

    property Rectangle: string read GetRectangle;
    property Variable: string read GetVariable;
    property Text: string read GetText;
    property Value: string read GetValue;
  end;

  { Listbox element }
  TWseListboxBlock = class(TWseBlock)
  private
    function GetRectangle: string;
    function GetVariable: string;
  public
    class function TagName: string; override;

    property Rectangle: string read GetRectangle;
    property Variable: string read GetVariable;
  end;

  { Set Control Attribute block }
  TWseSetControlAttrBlock = class(TWseBlock)
  private
    function GetControlName: string;
    function GetAttribute: string;
    function GetValue: string;
  public
    class function TagName: string; override;

    property ControlName: string read GetControlName;
    property Attribute: string read GetAttribute;
    property Value: string read GetValue;
  end;

  { New Event block }
  TWseNewEventBlock = class(TWseBlock)
  private
    function GetEvent: string;
  public
    class function TagName: string; override;

    property Event: string read GetEvent;
  end;

implementation

{ TWseNode }

constructor TWseNode.Create;
begin
  inherited Create;
  FChildren := TObjectList<TWseNode>.Create(True);
  FLine := 0;
  FColumn := 0;
  FIsRemarked := False;
  FConversionStatus := csNotConverted;
  FNsisOutput := '';
end;

destructor TWseNode.Destroy;
begin
  FChildren.Free;
  inherited Destroy;
end;

procedure TWseNode.AddChild(AChild: TWseNode);
begin
  FChildren.Add(AChild);
  AChild.SetParent(Self);
end;

procedure TWseNode.RemoveChild(AChild: TWseNode);
begin
  AChild.SetParent(nil);
  FChildren.Extract(AChild);
end;

procedure TWseNode.ClearChildren;
begin
  FChildren.Clear;
end;

procedure TWseNode.SetParent(AParent: TWseNode);
begin
  FParent := AParent;
end;

function TWseNode.GetChild(Index: Integer): TWseNode;
begin
  Result := FChildren[Index];
end;

function TWseNode.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

function TWseNode.FindChildByType(AClass: TWseNodeClass): TWseNode;
var
  i: Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    if FChildren[i] is AClass then
      Exit(FChildren[i]);
  Result := nil;
end;

function TWseNode.FindAllChildrenByType(AClass: TWseNodeClass): TArray<TWseNode>;
var
  i: Integer;
  List: TList<TWseNode>;
begin
  List := TList<TWseNode>.Create;
  try
    for i := 0 to FChildren.Count - 1 do
      if FChildren[i] is AClass then
        List.Add(FChildren[i]);
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

procedure TWseNode.Accept(AVisitor: IWseVisitor);
begin
  // Base implementation does nothing
end;

{ TWseDocument }

constructor TWseDocument.Create;
begin
  inherited Create;
  FDocumentType := 'WSE';
  FFilePath := '';
  FVariables := TVariableDictionary.Create;
end;

destructor TWseDocument.Destroy;
begin
  FVariables.Free;
  inherited Destroy;
end;

procedure TWseDocument.Accept(AVisitor: IWseVisitor);
begin
  AVisitor.VisitDocument(Self);
end;

function TWseDocument.GetGlobalBlock: TWseBlock;
begin
  Result := TWseBlock(FindChildByType(TWseGlobalBlock));
end;

{ TWseProperty }

constructor TWseProperty.Create(const AKey, AValue: string; const ALanguage: string);
begin
  inherited Create;
  FKey := AKey;
  FValue := AValue;
  FLanguage := ALanguage;
end;

{ TWsePropertyList }

function TWsePropertyList.GetValue(const AKey: string): string;
var
  Prop: TWseProperty;
begin
  for Prop in Self do
    if SameText(Prop.Key, AKey) and (Prop.Language = '') then
      Exit(Prop.Value);
  Result := '';
end;

function TWsePropertyList.GetValueWithLanguage(const AKey, ALanguage: string): string;
var
  Prop: TWseProperty;
begin
  for Prop in Self do
    if SameText(Prop.Key, AKey) and SameText(Prop.Language, ALanguage) then
      Exit(Prop.Value);
  Result := '';
end;

function TWsePropertyList.TryGetValue(const AKey: string; out AValue: string): Boolean;
var
  Prop: TWseProperty;
begin
  for Prop in Self do
    if SameText(Prop.Key, AKey) and (Prop.Language = '') then
    begin
      AValue := Prop.Value;
      Exit(True);
    end;
  AValue := '';
  Result := False;
end;

function TWsePropertyList.HasKey(const AKey: string): Boolean;
var
  Prop: TWseProperty;
begin
  for Prop in Self do
    if SameText(Prop.Key, AKey) then
      Exit(True);
  Result := False;
end;

procedure TWsePropertyList.SetValue(const AKey, AValue: string);
var
  Prop: TWseProperty;
begin
  for Prop in Self do
    if SameText(Prop.Key, AKey) and (Prop.Language = '') then
    begin
      Prop.Value := AValue;
      Exit;
    end;
  Add(TWseProperty.Create(AKey, AValue));
end;

function TWsePropertyList.GetInteger(const AKey: string; ADefault: Integer): Integer;
var
  S: string;
begin
  S := GetValue(AKey);
  if S = '' then
    Result := ADefault
  else
    Result := StrToIntDef(S, ADefault);
end;

function TWsePropertyList.GetBoolean(const AKey: string; ADefault: Boolean): Boolean;
var
  S: string;
begin
  S := GetValue(AKey);
  if S = '' then
    Result := ADefault
  else
    Result := SameText(S, 'True') or SameText(S, '1') or SameText(S, 'Yes');
end;

function TWsePropertyList.GetAllValues(const AKey: string): TArray<string>;
var
  Prop: TWseProperty;
  List: TList<string>;
begin
  List := TList<string>.Create;
  try
    for Prop in Self do
      if SameText(Prop.Key, AKey) and (Prop.Language = '') then
        List.Add(Prop.Value);
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

{ TWseBlock }

constructor TWseBlock.Create;
begin
  inherited Create;
  FBlockType := '';
  FProperties := TWsePropertyList.Create(True);
end;

destructor TWseBlock.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

procedure TWseBlock.Accept(AVisitor: IWseVisitor);
begin
  AVisitor.VisitBlock(Self);
end;

class function TWseBlock.TagName: string;
begin
  Result := 'Block';
end;

function TWseBlock.GetTagName: string;
begin
  Result := TagName;
end;

function TWseBlock.GetProp(const AKey: string): string;
begin
  Result := FProperties.GetValue(AKey);
end;

function TWseBlock.GetPropInt(const AKey: string; ADefault: Integer): Integer;
begin
  Result := FProperties.GetInteger(AKey, ADefault);
end;

function TWseBlock.HasProp(const AKey: string): Boolean;
begin
  Result := FProperties.HasKey(AKey);
end;

{ TWseGlobalBlock }

constructor TWseGlobalBlock.Create;
begin
  inherited Create;
  FBlockType := 'Global';
  FVariableCount := 0;
end;

class function TWseGlobalBlock.TagName: string;
begin
  Result := 'Global';
end;

procedure TWseGlobalBlock.ExtractVariables(AVarDict: TVariableDictionary);
var
  i: Integer;
  VarName, VarDefault, VarFlags: string;
begin
  // Extract named variables (Variable Name1, Variable Default1, etc.)
  i := 1;
  while True do
  begin
    VarName := Properties.GetValue('Variable Name' + IntToStr(i));
    if VarName = '' then
      Break;

    VarDefault := Properties.GetValue('Variable Default' + IntToStr(i));
    VarFlags := Properties.GetValue('Variable Flags' + IntToStr(i));

    AVarDict.AddVariable(VarName, VarDefault, VarFlags, vsGlobal, i);
    Inc(i);
  end;
  FVariableCount := i - 1;
end;

{ TWseSetVariableBlock }

class function TWseSetVariableBlock.TagName: string;
begin
  Result := 'Set Variable';
end;

function TWseSetVariableBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseSetVariableBlock.GetValue: string;
begin
  Result := GetProp('Value');
end;

function TWseSetVariableBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

{ TWseIfWhileBlock }

class function TWseIfWhileBlock.TagName: string;
begin
  Result := 'If/While Statement';
end;

function TWseIfWhileBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseIfWhileBlock.GetValue: string;
begin
  Result := GetProp('Value');
end;

function TWseIfWhileBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

function TWseIfWhileBlock.IsWhileLoop: Boolean;
begin
  // Flags bit 7 indicates while loop
  Result := (Length(Flags) >= 8) and (Flags[8] = '1');
end;

function TWseIfWhileBlock.GetComparisonOperator: string;
var
  FlagsStr: string;
begin
  FlagsStr := Flags;
  if Length(FlagsStr) < 2 then
    Result := '=='
  else
  begin
    case FlagsStr[Length(FlagsStr)] of
      '0': Result := '==';       // Equals
      '1': Result := '!=';       // Not equals
      '2': Result := '>';        // Greater than
      '3': Result := 'contains'; // String contains
    else
      Result := '==';
    end;
  end;
end;

{ TWseElseBlock }

class function TWseElseBlock.TagName: string;
begin
  Result := 'Else Statement';
end;

{ TWseEndBlock }

class function TWseEndBlock.TagName: string;
begin
  Result := 'End Block';
end;

{ TWseInstallFileBlock }

class function TWseInstallFileBlock.TagName: string;
begin
  Result := 'Install File';
end;

function TWseInstallFileBlock.GetSource: string;
begin
  Result := GetProp('Source');
end;

function TWseInstallFileBlock.GetDestination: string;
begin
  Result := GetProp('Destination');
end;

function TWseInstallFileBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

function TWseInstallFileBlock.IsRecursive: Boolean;
begin
  // Flags bit indicates recursive copy
  Result := (Length(Flags) >= 12) and (Flags[12] = '1');
end;

{ TWseEditRegistryBlock }

class function TWseEditRegistryBlock.TagName: string;
begin
  Result := 'Edit Registry';
end;

function TWseEditRegistryBlock.GetKey: string;
begin
  Result := GetProp('Key');
end;

function TWseEditRegistryBlock.GetValueName: string;
begin
  Result := GetProp('Value Name');
end;

function TWseEditRegistryBlock.GetNewValue: string;
begin
  Result := GetProp('New Value');
end;

function TWseEditRegistryBlock.GetRoot: Integer;
begin
  Result := GetPropInt('Root', 2);  // Default to HKLM
end;

function TWseEditRegistryBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

function TWseEditRegistryBlock.GetTotalKeys: Integer;
begin
  Result := GetPropInt('Total Keys', 1);
end;

function TWseEditRegistryBlock.GetNsisRootKey: string;
begin
  Result := WiseRootToNsis(Root);
end;

function TWseEditRegistryBlock.IsDeleteOperation: Boolean;
begin
  // Flags bit 0 indicates delete
  Result := (Length(Flags) >= 1) and (Flags[Length(Flags)] = '1');
end;

{ TWseGetRegistryBlock }

class function TWseGetRegistryBlock.TagName: string;
begin
  Result := 'Get Registry Key Value';
end;

function TWseGetRegistryBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseGetRegistryBlock.GetKey: string;
begin
  Result := GetProp('Key');
end;

function TWseGetRegistryBlock.GetValueName: string;
begin
  Result := GetProp('Value Name');
end;

function TWseGetRegistryBlock.GetDefaultValue: string;
begin
  Result := GetProp('Default');
end;

function TWseGetRegistryBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

{ TWseDisplayMessageBlock }

class function TWseDisplayMessageBlock.TagName: string;
begin
  Result := 'Display Message';
end;

function TWseDisplayMessageBlock.GetTitle: string;
begin
  Result := GetProp('Title');
end;

function TWseDisplayMessageBlock.GetText: string;
var
  TextLines: TArray<string>;
begin
  TextLines := Properties.GetAllValues('Text');
  Result := String.Join(#13#10, TextLines);
end;

function TWseDisplayMessageBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

function TWseDisplayMessageBlock.GetMessageBoxType: string;
var
  FlagsStr: string;
begin
  FlagsStr := Flags;
  if Length(FlagsStr) < 2 then
    Result := 'MB_OK'
  else
  begin
    case FlagsStr[Length(FlagsStr)] of
      '0': Result := 'MB_OK';
      '1': Result := 'MB_OKCANCEL';
      '2': Result := 'MB_YESNO';
      '3': Result := 'MB_YESNOCANCEL';
    else
      Result := 'MB_OK';
    end;
  end;
end;

{ TWseExecuteProgramBlock }

class function TWseExecuteProgramBlock.TagName: string;
begin
  Result := 'Execute Program';
end;

function TWseExecuteProgramBlock.GetPathname: string;
begin
  Result := GetProp('Pathname');
end;

function TWseExecuteProgramBlock.GetCommandLine: string;
begin
  Result := GetProp('Command Line');
end;

function TWseExecuteProgramBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

function TWseExecuteProgramBlock.WaitForCompletion: Boolean;
begin
  // Default is to wait; check flags for no-wait
  Result := not ((Length(Flags) >= 1) and (Flags[Length(Flags)] = '1'));
end;

{ TWseServiceBlock }

class function TWseServiceBlock.TagName: string;
begin
  Result := 'Start/Stop Service';
end;

function TWseServiceBlock.GetServiceName: string;
begin
  Result := GetProp('Service Name');
end;

function TWseServiceBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

function TWseServiceBlock.IsStartOperation: Boolean;
begin
  // Flags bit 0: 0=start, 1=stop
  Result := not ((Length(Flags) >= 1) and (Flags[Length(Flags)] = '1'));
end;

{ TWseCreateShortcutBlock }

class function TWseCreateShortcutBlock.TagName: string;
begin
  Result := 'Create Shortcut';
end;

function TWseCreateShortcutBlock.GetSource: string;
begin
  Result := GetProp('Source');
end;

function TWseCreateShortcutBlock.GetDestination: string;
begin
  Result := GetProp('Destination');
end;

function TWseCreateShortcutBlock.GetWorkingDirectory: string;
begin
  Result := GetProp('Working Directory');
end;

function TWseCreateShortcutBlock.GetCommandLine: string;
begin
  Result := GetProp('Command Line');
end;

function TWseCreateShortcutBlock.GetIconPathname: string;
begin
  Result := GetProp('Icon Pathname');
end;

function TWseCreateShortcutBlock.GetIconIndex: Integer;
begin
  Result := GetPropInt('Icon Index', 0);
end;

{ TWseRemarkBlock }

class function TWseRemarkBlock.TagName: string;
begin
  Result := 'Remark';
end;

function TWseRemarkBlock.GetText: string;
begin
  Result := GetProp('Text');
end;

{ TWseCheckExistsBlock }

class function TWseCheckExistsBlock.TagName: string;
begin
  Result := 'Check if File/Dir Exists';
end;

function TWseCheckExistsBlock.GetPathname: string;
begin
  Result := GetProp('Pathname');
end;

function TWseCheckExistsBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

function TWseCheckExistsBlock.IsDirectoryCheck: Boolean;
begin
  // Flags bit indicates directory check
  Result := (Length(Flags) >= 4) and (Flags[4] = '1');
end;

function TWseCheckExistsBlock.IsNegatedCheck: Boolean;
begin
  // Flags bit indicates negated (not exists)
  Result := (Length(Flags) >= 8) and (Flags[8] = '1');
end;

{ TWseCreateDirectoryBlock }

class function TWseCreateDirectoryBlock.TagName: string;
begin
  Result := 'Create Directory';
end;

function TWseCreateDirectoryBlock.GetPathname: string;
begin
  Result := GetProp('Pathname');
end;

{ TWseDeleteFileBlock }

class function TWseDeleteFileBlock.TagName: string;
begin
  Result := 'Delete File';
end;

function TWseDeleteFileBlock.GetPathname: string;
begin
  Result := GetProp('Pathname');
end;

function TWseDeleteFileBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

{ TWseCopyFileBlock }

class function TWseCopyFileBlock.TagName: string;
begin
  Result := 'Copy Local File';
end;

function TWseCopyFileBlock.GetSource: string;
begin
  Result := GetProp('Source');
end;

function TWseCopyFileBlock.GetDestination: string;
begin
  Result := GetProp('Destination');
end;

function TWseCopyFileBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

{ TWseExitBlock }

class function TWseExitBlock.TagName: string;
begin
  Result := 'Exit Installation';
end;

{ TWseWizardBlock }

class function TWseWizardBlock.TagName: string;
begin
  Result := 'Wizard Block';
end;

function TWseWizardBlock.GetDirectionVariable: string;
begin
  Result := GetProp('Direction Variable');
end;

function TWseWizardBlock.GetDisplayVariable: string;
begin
  Result := GetProp('Display Variable');
end;

function TWseWizardBlock.GetBitmapPathname: string;
begin
  Result := GetProp('Bitmap Pathname');
end;

{ TWseCustomDialogBlock }

class function TWseCustomDialogBlock.TagName: string;
begin
  Result := 'Custom Dialog Set';
end;

function TWseCustomDialogBlock.GetName: string;
begin
  Result := GetProp('Name');
end;

function TWseCustomDialogBlock.GetDisplayVariable: string;
begin
  Result := GetProp('Display Variable');
end;

{ TWseParseStringBlock }

class function TWseParseStringBlock.TagName: string;
begin
  Result := 'Parse String';
end;

function TWseParseStringBlock.GetSource: string;
begin
  Result := GetProp('Source');
end;

function TWseParseStringBlock.GetPattern: string;
begin
  Result := GetProp('Pattern');
end;

function TWseParseStringBlock.GetVariable1: string;
begin
  Result := GetProp('Variable1');
end;

function TWseParseStringBlock.GetVariable2: string;
begin
  Result := GetProp('Variable2');
end;

function TWseParseStringBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

{ TWseGetSystemInfoBlock }

class function TWseGetSystemInfoBlock.TagName: string;
begin
  Result := 'Get System Information';
end;

function TWseGetSystemInfoBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseGetSystemInfoBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

{ TWseGetEnvironmentBlock }

class function TWseGetEnvironmentBlock.TagName: string;
begin
  Result := 'Get Environment Variable';
end;

function TWseGetEnvironmentBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseGetEnvironmentBlock.GetEnvironment: string;
begin
  Result := GetProp('Environment');
end;

{ TWseEditIniBlock }

class function TWseEditIniBlock.TagName: string;
begin
  Result := 'Edit INI File';
end;

function TWseEditIniBlock.GetPathname: string;
begin
  Result := GetProp('Pathname');
end;

function TWseEditIniBlock.GetSection: string;
begin
  Result := GetProp('Section');
end;

function TWseEditIniBlock.GetItem: string;
begin
  Result := GetProp('Item');
end;

function TWseEditIniBlock.GetNewValue: string;
begin
  Result := GetProp('New Value');
end;

{ TWseReadIniBlock }

class function TWseReadIniBlock.TagName: string;
begin
  Result := 'Read INI Value';
end;

function TWseReadIniBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseReadIniBlock.GetPathname: string;
begin
  Result := GetProp('Pathname');
end;

function TWseReadIniBlock.GetSection: string;
begin
  Result := GetProp('Section');
end;

function TWseReadIniBlock.GetItem: string;
begin
  Result := GetProp('Item');
end;

{ TWseGetTempFilenameBlock }

class function TWseGetTempFilenameBlock.TagName: string;
begin
  Result := 'Get Temporary Filename';
end;

function TWseGetTempFilenameBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

{ TWseOpenCloseLogBlock }

class function TWseOpenCloseLogBlock.TagName: string;
begin
  Result := 'Open/Close INSTALL.LOG';
end;

function TWseOpenCloseLogBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

function TWseOpenCloseLogBlock.IsCloseOperation: Boolean;
begin
  // Flags bit 0: 0=open, 1=close
  Result := (Length(Flags) >= 1) and (Flags[Length(Flags)] = '1');
end;

{ TWseAddTextToLogBlock }

class function TWseAddTextToLogBlock.TagName: string;
begin
  Result := 'Add Text to INSTALL.LOG';
end;

function TWseAddTextToLogBlock.GetText: string;
begin
  Result := GetProp('Text');
end;

{ TWseIncludeScriptBlock }

class function TWseIncludeScriptBlock.TagName: string;
begin
  Result := 'Include Script';
end;

function TWseIncludeScriptBlock.GetFilename: string;
begin
  Result := GetProp('Pathname');
end;

{ TWseCustomScriptBlock }

class function TWseCustomScriptBlock.TagName: string;
begin
  Result := 'Custom Script Item';
end;

function TWseCustomScriptBlock.GetFilename: string;
begin
  Result := GetProp('Filename');
end;

{ TWseSelfRegisterBlock }

class function TWseSelfRegisterBlock.TagName: string;
begin
  Result := 'Self-Register OCXs/DLLs';
end;

function TWseSelfRegisterBlock.GetFilename: string;
begin
  Result := GetProp('Filename');
end;

{ TWseElseIfBlock }

class function TWseElseIfBlock.TagName: string;
begin
  Result := 'ElseIf Statement';
end;

function TWseElseIfBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseElseIfBlock.GetValue: string;
begin
  Result := GetProp('Value');
end;

function TWseElseIfBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

{ TWseInsertLineBlock }

class function TWseInsertLineBlock.TagName: string;
begin
  Result := 'Insert Line into Text File';
end;

function TWseInsertLineBlock.GetPathname: string;
begin
  Result := GetProp('Pathname');
end;

function TWseInsertLineBlock.GetNewText: string;
begin
  Result := GetProp('New Text');
end;

function TWseInsertLineBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

{ TWseReadUpdateTextBlock }

class function TWseReadUpdateTextBlock.TagName: string;
begin
  Result := 'Read/Update Text File';
end;

function TWseReadUpdateTextBlock.GetPathname: string;
begin
  Result := GetProp('Pathname');
end;

function TWseReadUpdateTextBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseReadUpdateTextBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

{ TWseRenameFileBlock }

class function TWseRenameFileBlock.TagName: string;
begin
  Result := 'Rename File/Directory';
end;

function TWseRenameFileBlock.GetOldPathname: string;
begin
  Result := GetProp('Old Pathname');
end;

function TWseRenameFileBlock.GetNewPathname: string;
begin
  Result := GetProp('New Pathname');
end;

{ TWseSearchFileBlock }

class function TWseSearchFileBlock.TagName: string;
begin
  Result := 'Search for File';
end;

function TWseSearchFileBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseSearchFileBlock.GetPathname: string;
begin
  Result := GetProp('Pathname');
end;

function TWseSearchFileBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

{ TWseFindInPathBlock }

class function TWseFindInPathBlock.TagName: string;
begin
  Result := 'Find File in Path';
end;

function TWseFindInPathBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseFindInPathBlock.GetFilename: string;
begin
  Result := GetProp('Filename');
end;

function TWseFindInPathBlock.GetSearchPath: string;
begin
  Result := GetProp('Path Variable');
end;

{ TWseCheckConfigBlock }

class function TWseCheckConfigBlock.TagName: string;
begin
  Result := 'Check Configuration';
end;

function TWseCheckConfigBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

{ TWseCreateServiceBlock }

class function TWseCreateServiceBlock.TagName: string;
begin
  Result := 'Create Service';
end;

function TWseCreateServiceBlock.GetServiceName: string;
begin
  Result := GetProp('Service Name');
end;

function TWseCreateServiceBlock.GetDisplayName: string;
begin
  Result := GetProp('Display Name');
end;

function TWseCreateServiceBlock.GetPathname: string;
begin
  Result := GetProp('Pathname');
end;

function TWseCreateServiceBlock.GetStartType: Integer;
begin
  Result := GetPropInt('Start Type', 2);  // Default auto-start
end;

function TWseCreateServiceBlock.GetFlags: string;
begin
  Result := GetProp('Flags');
end;

{ TWseExecuteVBScriptBlock }

class function TWseExecuteVBScriptBlock.TagName: string;
begin
  Result := 'Execute VBScript';
end;

function TWseExecuteVBScriptBlock.GetScript: string;
begin
  Result := GetProp('Script');
end;

function TWseExecuteVBScriptBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

{ TWseGenericBlock }

class function TWseGenericBlock.TagName: string;
begin
  Result := 'Generic';
end;

{ TWseDialogBlock }

class function TWseDialogBlock.TagName: string;
begin
  Result := 'Dialog';
end;

function TWseDialogBlock.GetTitle: string;
begin
  Result := GetProp('Title');
end;

function TWseDialogBlock.GetWidth: Integer;
begin
  Result := GetPropInt('Width', 290);
end;

function TWseDialogBlock.GetHeight: Integer;
begin
  Result := GetPropInt('Height', 238);
end;

{ TWseStaticBlock }

class function TWseStaticBlock.TagName: string;
begin
  Result := 'Static';
end;

function TWseStaticBlock.GetRectangle: string;
begin
  Result := GetProp('Rectangle');
end;

function TWseStaticBlock.GetText: string;
begin
  Result := GetProp('Text');
end;

{ TWsePushButtonBlock }

class function TWsePushButtonBlock.TagName: string;
begin
  Result := 'Push Button';
end;

function TWsePushButtonBlock.GetRectangle: string;
begin
  Result := GetProp('Rectangle');
end;

function TWsePushButtonBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWsePushButtonBlock.GetValue: string;
begin
  Result := GetProp('Value');
end;

function TWsePushButtonBlock.GetText: string;
begin
  Result := GetProp('Text');
end;

function TWsePushButtonBlock.GetAction: Integer;
begin
  Result := GetPropInt('Action', 0);
end;

{ TWseEditboxBlock }

class function TWseEditboxBlock.TagName: string;
begin
  Result := 'Editbox';
end;

function TWseEditboxBlock.GetRectangle: string;
begin
  Result := GetProp('Rectangle');
end;

function TWseEditboxBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseEditboxBlock.GetValue: string;
begin
  Result := GetProp('Value');
end;

{ TWseCheckboxBlock }

class function TWseCheckboxBlock.TagName: string;
begin
  Result := 'Checkbox';
end;

function TWseCheckboxBlock.GetRectangle: string;
begin
  Result := GetProp('Rectangle');
end;

function TWseCheckboxBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseCheckboxBlock.GetText: string;
begin
  Result := GetProp('Text');
end;

{ TWseRadioButtonBlock }

class function TWseRadioButtonBlock.TagName: string;
begin
  Result := 'Radio Button';
end;

function TWseRadioButtonBlock.GetRectangle: string;
begin
  Result := GetProp('Rectangle');
end;

function TWseRadioButtonBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

function TWseRadioButtonBlock.GetText: string;
begin
  Result := GetProp('Text');
end;

function TWseRadioButtonBlock.GetValue: string;
begin
  Result := GetProp('Value');
end;

{ TWseListboxBlock }

class function TWseListboxBlock.TagName: string;
begin
  Result := 'Listbox';
end;

function TWseListboxBlock.GetRectangle: string;
begin
  Result := GetProp('Rectangle');
end;

function TWseListboxBlock.GetVariable: string;
begin
  Result := GetProp('Variable');
end;

{ TWseSetControlAttrBlock }

class function TWseSetControlAttrBlock.TagName: string;
begin
  Result := 'Set Control Attribute';
end;

function TWseSetControlAttrBlock.GetControlName: string;
begin
  Result := GetProp('Control Name');
end;

function TWseSetControlAttrBlock.GetAttribute: string;
begin
  Result := GetProp('Attribute');
end;

function TWseSetControlAttrBlock.GetValue: string;
begin
  Result := GetProp('Value');
end;

{ TWseNewEventBlock }

class function TWseNewEventBlock.TagName: string;
begin
  Result := 'New Event';
end;

function TWseNewEventBlock.GetEvent: string;
begin
  Result := GetProp('Event');
end;

end.
