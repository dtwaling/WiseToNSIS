# CLAUDE.md - WiseToNSIS Converter

## Project Overview

**WiseToNSIS** is a Delphi 12 application that converts Wise Install scripts (.wse) to NSIS scripts (.nsi/.nsh). It provides both CLI and GUI interfaces with an assisted/interactive conversion approach.

---

## Quick Reference

### Build Commands

```cmd
# Build GUI application
msbuild WiseToNSIS.dpr

# Build CLI application
msbuild WiseToNSISCLI.dpr

# Build all projects
msbuild WiseToNSIS.groupproj

# Run tests
msbuild test\WiseToNSIS.Tests.dpr
```

### CLI Usage

```cmd
# Basic conversion
WiseToNSISCLI.exe input.wse output.nsi

# Batch conversion
WiseToNSISCLI.exe -b -o output_dir *.wse

# With options
WiseToNSISCLI.exe --verbose --report report.txt input.wse
```

---

## Directory Structure

```
WiseToNSIS/
├── WiseToNSIS.dpr              # GUI application
├── WiseToNSISCLI.dpr           # CLI application
├── WiseToNSIS.groupproj        # Project group
├── src/
│   ├── core/                   # Conversion engine
│   │   ├── WSE.Parser.pas      # Lexer/parser for .wse
│   │   ├── WSE.AST.pas         # AST node classes
│   │   ├── WSE.Flags.pas       # Binary flag interpreters
│   │   ├── NSIS.Generator.pas  # Script generator
│   │   ├── NSIS.Mapper.pas     # Command mapping (visitor)
│   │   ├── NSIS.Pages.pas      # nsDialogs page template generator
│   │   └── Converter.Engine.pas # Main orchestrator
│   ├── cli/                    # CLI-specific
│   │   ├── CLI.Runner.pas      # Command-line runner
│   │   ├── CLI.Options.pas     # Argument parsing
│   │   └── CLI.Reporter.pas    # Console output
│   ├── gui/                    # GUI-specific
│   │   └── Main.Form.pas       # Main application form
│   └── common/                 # Shared utilities
│       ├── Conversion.Types.pas
│       └── Conversion.Registry.pas
├── test/                       # DUnit tests
│   ├── WiseToNSIS.Tests.dpr
│   ├── Parser.Tests.pas
│   └── Mapper.Tests.pas
├── doc/                        # Documentation
│   └── Wise-to-NSIS-Conversion-Guide.md
├── templates/                  # Sample, reference, and NSIS templates
│   ├── WantClasses.pas
│   └── WiseSample.wse
└── ISResources/                # Wise and NSIS software files (zipped)
    ├── NSIS.zip
    └── Wise.zip
```

---

## Architecture

### Core Flow

```
.wse file → TWseLexer → TWseParser → TWseDocument (AST)
                                           ↓
                                    TNsisMapperVisitor
                                           ↓
                                    TNsisScript → .nsi file
```

### Key Classes

| Class | Purpose |
|-------|---------|
| `TWseParser` | Parses .wse files into AST |
| `TWseDocument` | Root AST node |
| `TWseBlock` | Base block class (30+ subclasses) |
| `TNsisMapperVisitor` | Visitor that transforms AST to NSIS |
| `TNsisScript` | NSIS script builder |
| `TNsisPageGenerator` | Converts Custom Dialogs to nsDialogs pages |
| `TConverterEngine` | Main orchestrator |

### Block Type Hierarchy

```
TWseNode
├── TWseDocument
└── TWseBlock (abstract)
    ├── TWseGlobalBlock
    ├── TWseSetVariableBlock
    ├── TWseIfWhileBlock
    ├── TWseInstallFileBlock
    ├── TWseEditRegistryBlock
    ├── TWseServiceBlock
    ├── TWseWizardBlock
    ├── TWseCustomDialogBlock
    └── ... (30+ block types)
```

---

## Conversion Mapping

### Variable Conversion

| Wise | NSIS |
|------|------|
| `%VAR%` | `$VAR` |
| `%MAINDIR%` | `$INSTDIR` |
| `%_SYS_%` | `$SYSDIR` |
| `%_WIN_%` | `$WINDIR` |
| `%TEMP%` | `$TEMP` |
| `%PROGRAM_FILES%` | `$PROGRAMFILES` |

### Command Mapping

| Wise Command | NSIS Equivalent |
|--------------|-----------------|
| `Set Variable` | `StrCpy` |
| `If/While Statement` | `${If}` / `${While}` |
| `Install File` | `SetOutPath` + `File` |
| `Edit Registry` | `WriteRegStr` / `WriteRegDWORD` |
| `Display Message` | `MessageBox` |
| `Start/Stop Service` | `SimpleSC::*` (with timeout/error handling) |
| `Execute Program` | `ExecWait` / `Exec` |
| `Create Directory` | `CreateDirectory` |
| `Delete File` | `Delete` / `Delete /REBOOTOK` |
| `Parse String` | `StrCpy` / `${StrStr}` / StrFunc.nsh |
| `Get System Info` | `System::Call` (kernel32, user32) |
| `Custom Dialog Set` | `nsDialogs` page templates |

---

## Key Implementation Details

### Block Registration (Conversion.Registry.pas)

Uses a dynamic registry pattern inspired by WAnt:

```pascal
initialization
  RegisterBlocks([
    TWseGlobalBlock,
    TWseSetVariableBlock,
    TWseIfWhileBlock,
    // ...
  ]);
```

### Visitor Pattern (NSIS.Mapper.pas)

The mapper uses the visitor pattern to transform each block type:

```pascal
procedure TNsisMapperVisitor.VisitBlock(ANode: TWseBlock);
begin
  if ANode is TWseSetVariableBlock then
    MapSetVariableBlock(TWseSetVariableBlock(ANode))
  else if ANode is TWseIfWhileBlock then
    MapIfWhileBlock(TWseIfWhileBlock(ANode))
  // ...
end;
```

### Flag Interpretation (WSE.Flags.pas)

Wise uses binary strings for flags. The flag interpreter parses these:

```pascal
function GetFlagBit(const AFlags: string; ABitIndex: Integer): Boolean;
// e.g., "00010101" - bit 0 is rightmost
```

---

## Testing

### Running Tests

```cmd
# GUI test runner
test\WiseToNSIS.Tests.exe

# Console test runner
test\WiseToNSIS.Tests.exe -c
```

### Test Files

- `Parser.Tests.pas` - Tests for WSE parsing
- `Mapper.Tests.pas` - Tests for NSIS mapping

### Integration Testing

Test against real .wse files in `.\templates\`:
- `WiseSample.wse`
- etc.

---

## Items Requiring Manual Review

Most Wise commands are now fully converted. These items may still require review:

1. **Custom Dialog Events** - Complex dialog event scripting (INIT/VERIFY/UPDATE)
2. **Custom Script Items** - VBScript/JScript embedded scripts need manual conversion
3. **Include Scripts** - Referenced .wse files need separate conversion
4. **VBScript Expressions** - Complex expressions in If/While statements
5. **Service Pack/Processor Type** - System info requiring WMI queries

### Fully Automated Conversions (Previously Manual)

- **Custom Dialog Sets** - Now converted to nsDialogs page templates
- **Parse String** - Position and pattern-based splits now use StrFunc.nsh
- **Get System Info** - 12+ system info types now use System::Call
- **Delete File with Reboot** - /REBOOTOK flag properly handled
- **Service Operations** - Timeout and error handling now included

---

## Coding Standards

- Follow Delphi 12 conventions
- Use strong typing with generics
- RTTI enabled on AST classes for potential reflection
- Visitor pattern for extensibility
- Comprehensive error handling with issue tracking

---

## Reference Files

| Purpose | Path |
|---------|------|
| Conversion guide | `.\doc\Wise-to-NSIS-Conversion-Guide.md` |
| Sample .wse files | `.\templates\WiseSample.wse` |
| Wise reference | `.\ISResources\Wise.zip` |
| NSIS reference | `.\ISResources\NSIS.zip` |
| WAnt (registry pattern) | `.\templates\WantClasses.pas` |
