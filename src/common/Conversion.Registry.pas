unit Conversion.Registry;

{******************************************************************************
  WiseToNSIS Converter - Block Registry

  Dynamic registry for WSE block types, inspired by WAnt's element registry.
  Allows registration of block classes and factory lookup by tag name.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  WSE.AST;

type
  { Block class reference type }
  TWseBlockClass = class of TWseBlock;

  { Registry entry record }
  TBlockRegistryEntry = record
    TagName: string;
    BlockClass: TWseBlockClass;
    Description: string;
  end;

{ Registration procedures }
procedure RegisterBlock(ABlockClass: TWseBlockClass; const ADescription: string = '');
procedure RegisterBlocks(const ABlockClasses: array of TWseBlockClass);

{ Lookup functions }
function FindBlockClass(const ATagName: string): TWseBlockClass;
function CreateBlock(const ATagName: string): TWseBlock;
function IsBlockRegistered(const ATagName: string): Boolean;

{ Registry enumeration }
function GetRegisteredBlockCount: Integer;
function GetRegisteredBlocks: TArray<TBlockRegistryEntry>;
function GetRegisteredBlockNames: TArray<string>;

implementation

var
  { Global block registry }
  __BlockRegistry: TArray<TBlockRegistryEntry>;

procedure RegisterBlock(ABlockClass: TWseBlockClass; const ADescription: string);
var
  Pos: Integer;
  Entry: TBlockRegistryEntry;
begin
  Assert(Assigned(ABlockClass), 'Block class cannot be nil');

  // Check if already registered
  if IsBlockRegistered(ABlockClass.TagName) then
    Exit;

  // Add to registry
  Pos := Length(__BlockRegistry);
  SetLength(__BlockRegistry, Pos + 1);

  Entry.TagName := LowerCase(ABlockClass.TagName);
  Entry.BlockClass := ABlockClass;
  Entry.Description := ADescription;

  __BlockRegistry[Pos] := Entry;
end;

procedure RegisterBlocks(const ABlockClasses: array of TWseBlockClass);
var
  i: Integer;
begin
  for i := Low(ABlockClasses) to High(ABlockClasses) do
    RegisterBlock(ABlockClasses[i]);
end;

function FindBlockClass(const ATagName: string): TWseBlockClass;
var
  i: Integer;
  NormalizedTag: string;
begin
  NormalizedTag := LowerCase(ATagName);

  // Search from high to low (allows overriding)
  for i := High(__BlockRegistry) downto Low(__BlockRegistry) do
  begin
    if __BlockRegistry[i].TagName = NormalizedTag then
    begin
      Result := __BlockRegistry[i].BlockClass;
      Exit;
    end;
  end;

  Result := nil;
end;

function CreateBlock(const ATagName: string): TWseBlock;
var
  BlockClass: TWseBlockClass;
begin
  BlockClass := FindBlockClass(ATagName);

  if Assigned(BlockClass) then
    Result := BlockClass.Create
  else
  begin
    // Return generic block for unknown types
    Result := TWseGenericBlock.Create;
    Result.BlockType := ATagName;
  end;

  Result.BlockType := ATagName;
end;

function IsBlockRegistered(const ATagName: string): Boolean;
begin
  Result := Assigned(FindBlockClass(ATagName));
end;

function GetRegisteredBlockCount: Integer;
begin
  Result := Length(__BlockRegistry);
end;

function GetRegisteredBlocks: TArray<TBlockRegistryEntry>;
begin
  Result := Copy(__BlockRegistry);
end;

function GetRegisteredBlockNames: TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, Length(__BlockRegistry));
  for i := 0 to High(__BlockRegistry) do
    Result[i] := __BlockRegistry[i].TagName;
end;

initialization
  __BlockRegistry := nil;

  // Register all known block types
  RegisterBlocks([
    // Core blocks
    TWseGlobalBlock,
    TWseSetVariableBlock,
    TWseIfWhileBlock,
    TWseElseBlock,
    TWseEndBlock,

    // File operations
    TWseInstallFileBlock,
    TWseCopyFileBlock,
    TWseDeleteFileBlock,
    TWseCreateDirectoryBlock,
    TWseCheckExistsBlock,

    // Registry operations
    TWseEditRegistryBlock,
    TWseGetRegistryBlock,

    // UI blocks
    TWseDisplayMessageBlock,
    TWseWizardBlock,
    TWseCustomDialogBlock,
    TWseDialogBlock,
    TWseStaticBlock,
    TWsePushButtonBlock,
    TWseEditboxBlock,
    TWseCheckboxBlock,
    TWseRadioButtonBlock,
    TWseListboxBlock,
    TWseSetControlAttrBlock,
    TWseNewEventBlock,

    // Execution blocks
    TWseExecuteProgramBlock,
    TWseServiceBlock,
    TWseExitBlock,

    // Shortcut blocks
    TWseCreateShortcutBlock,

    // Comments and logging
    TWseRemarkBlock,
    TWseOpenCloseLogBlock,
    TWseAddTextToLogBlock,

    // String/variable operations
    TWseParseStringBlock,
    TWseGetSystemInfoBlock,
    TWseGetEnvironmentBlock,
    TWseGetTempFilenameBlock,

    // INI file operations
    TWseEditIniBlock,
    TWseReadIniBlock,

    // Script includes
    TWseIncludeScriptBlock,
    TWseCustomScriptBlock,

    // Registration
    TWseSelfRegisterBlock,

    // Flow control
    TWseElseIfBlock,

    // Text file operations
    TWseInsertLineBlock,
    TWseReadUpdateTextBlock,

    // File operations
    TWseRenameFileBlock,
    TWseSearchFileBlock,
    TWseFindInPathBlock,

    // System operations
    TWseCheckConfigBlock,
    TWseCreateServiceBlock,
    TWseExecuteVBScriptBlock,

    // Generic fallback
    TWseGenericBlock
  ]);

finalization
  __BlockRegistry := nil;

end.
