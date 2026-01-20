# Wise Install to NSIS Conversion Guide

**Document Version:** 1.0
**Date:** January 2026
**Purpose:** Comprehensive guide for converting Symantec Wise Install scripts (.wse) to NSIS scripts (.nsi/.nsh)

**Source:** [Github](https://github.com/dtwaling/WiseToNSIS)

---

## Table of Contents

1. [Overview](#1-overview)
2. [File Format Comparison](#2-file-format-comparison)
3. [Architecture Differences](#3-architecture-differences)
4. [Variable System Conversion](#4-variable-system-conversion)
5. [Command Mapping Reference](#5-command-mapping-reference)
6. [Control Flow Conversion](#6-control-flow-conversion)
7. [Dialog/UI Conversion](#7-dialogui-conversion)
8. [File Operations](#8-file-operations)
9. [Registry Operations](#9-registry-operations)
10. [Service Management](#10-service-management)
11. [Multilingual Support](#11-multilingual-support)
12. [Conversion Process Steps](#12-conversion-process-steps)
13. [SomeTechCo-Specific Patterns](#13-SomeTechCo-specific-patterns)
14. [Appendix A: Complete Command Mapping Table](#appendix-a-complete-command-mapping-table)
15. [Appendix B: Sample Conversion](#appendix-b-sample-conversion)

---

## 1. Overview

### 1.1 Source Technology: Symantec Wise Install

**Version:** Wise Installation Studio 7 (Script Version 9.02)
**File Extension:** `.wse` (WiseScript Editor)
**Format:** Plain-text declarative script with `item: ... end` block structure
**Location:** `<YourSourceDir>\InstallScripts\`
**Wise Software** `WiseToNSIS\ISResources\Wise.zip`

**Key Characteristics:**
- Block-based declarative syntax
- 32-bit binary flags for command options
- Built-in multilingual support (10 languages)
- Wizard-based dialog system
- `%VARIABLE%` syntax for variable references

### 1.2 Target Technology: NSIS

**Version:** NSIS 3.x
**File Extensions:** `.nsi` (main script), `.nsh` (include/header files)
**Format:** Procedural script with sections, functions, and macros
**Location:** `WiseToNSIS\ISResources\NSIS.zip`
**NSIS Software** `WiseToNSIS\ISResources\NSIS.zip`

**Key Characteristics:**
- Procedural/imperative syntax
- Section-based component organization
- Modern UI 2 framework for dialogs
- Plugin architecture for extensibility
- `$Variable` syntax for variable references
- Macro system for code reuse

### 1.3 Scope of Conversion

**Scripts to Convert (in `<YourSourceDir>\InstallScripts\`):**

| Script | Purpose | Complexity |
|--------|---------|------------|
| `*.wse` | \{Add your list of WSE files to this table.\} | High |
| `Wkstn\WorkstationInstall.wse` | STC client app installation | High |
| `Server\ServerInstall.wse` | STC Server installation | High |

---

## 2. File Format Comparison

### 2.1 Wise Script Structure

```
Document Type: WSE

item: Global
  Version=9.02
  Title=Application Name
  Flags=00010101
  Variable Name1=MAINDIR
  Variable Default1=C:\Program Files\App
end

item: Set Variable
  Variable=MYVAR
  Value=some value
end

item: If/While Statement
  Variable=CONDITION
  Value=expected
  Flags=00000011
end
  item: Install File
    Source=C:\src\file.exe
    Destination=%MAINDIR%\file.exe
  end
item: End Block
end
```

### 2.2 NSIS Script Structure

```nsis
; Metadata
Unicode True
Name "Application Name"
OutFile "Setup.exe"
InstallDir "$PROGRAMFILES\App"
RequestExecutionLevel admin

; Includes
!include "MUI2.nsh"
!include "LogicLib.nsh"

; Variables
Var MYVAR

; Pages
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_LANGUAGE "English"

; Sections
Section "Main"
  StrCpy $MYVAR "some value"

  ${If} $CONDITION == "expected"
    SetOutPath "$INSTDIR"
    File "C:\src\file.exe"
  ${EndIf}
SectionEnd
```

### 2.3 Key Structural Differences

| Aspect | Wise | NSIS |
|--------|------|------|
| Paradigm | Declarative (block-based) | Procedural (imperative) |
| Variables | `%VAR%` | `$VAR` |
| Conditionals | `item: If/While ... end` | `${If} ... ${EndIf}` |
| Sections | Implicit | Explicit `Section ... SectionEnd` |
| Comments | `item: Remark` | `; comment` or `# comment` |
| Includes | `item: Include Script` | `!include "file.nsh"` |
| Indentation | 2 spaces per level | Convention only |

---

## 3. Architecture Differences

### 3.1 Execution Model

**Wise:**
- Linear execution through script blocks
- Wizard framework manages dialog flow
- `Direction Variable` controls navigation (N=Next, B=Back)
- Events: INIT, VERIFY, UPDATE for dialog scripting

**NSIS:**
- Section-based execution
- Callback functions (`.onInit`, `.onInstSuccess`, etc.)
- Page flow managed by `!insertmacro MUI_PAGE_*`
- Custom page functions for advanced logic

### 3.2 Component Organization

**Wise:**
```
item: Set Variable
  Variable=COMPONENTS
  Flags=10000000
end
item: Check Disk Space
  Component=COMPONENTS
end
```

**NSIS:**
```nsis
InstType "Full"
InstType "Minimal"

Section "Core Files" SecCore
  SectionIn RO  ; Read-only (required)
  ; ...
SectionEnd

Section /o "Optional Component" SecOptional
  SectionIn 1   ; Include in "Full" install type
  ; ...
SectionEnd
```

### 3.3 Uninstaller Generation

**Wise:**
- Uses `%_WISE_%\INCLUDE\uninstal.wse` include
- Logged to `INSTALL.LOG`
- Automatic rollback support

**NSIS:**
```nsis
Section "Install"
  WriteUninstaller "$INSTDIR\uninstall.exe"
SectionEnd

Section "Uninstall"
  Delete "$INSTDIR\*.*"
  RMDir "$INSTDIR"
  DeleteRegKey HKLM "Software\App"
SectionEnd
```

---

## 4. Variable System Conversion

### 4.1 Syntax Conversion

| Wise Syntax | NSIS Syntax | Notes |
|-------------|-------------|-------|
| `%VARNAME%` | `$VARNAME` | Standard variable reference |
| `%_SYS_%` | `$SYSDIR` | System32 directory |
| `%_WIN_%` | `$WINDIR` | Windows directory |
| `%PROGRAM_FILES%` | `$PROGRAMFILES` | Program Files |
| `%COMMON%` | `$COMMONFILES` | Common Files |
| `%TEMP%` | `$TEMP` | Temp directory |
| `%MAINDIR%` | `$INSTDIR` | Installation directory |
| `%GROUPDIR%` | `$SMPROGRAMS` | Start Menu Programs |
| `%INST%` | `$EXEDIR` | Installer directory |
| `%CMDLINE%` | `$CMDLINE` | Command line arguments |

### 4.2 Variable Declaration

**Wise (in Global section):**
```
item: Global
  Variable Name1=MYVAR
  Variable Default1=default_value
  Variable Flags1=00001000
end
```

**NSIS:**
```nsis
Var MYVAR

Function .onInit
  StrCpy $MYVAR "default_value"
FunctionEnd
```

### 4.3 Variable Assignment

**Wise:**
```
item: Set Variable
  Variable=MYVAR
  Value=new value
end
```

**NSIS:**
```nsis
StrCpy $MYVAR "new value"
```

### 4.4 Reading Registry into Variable

**Wise:**
```
item: Get Registry Key Value
  Variable=VERSION
  Key=Software\App
  Value Name=Version
  Default=1.0
  Flags=00000100
end
```

**NSIS:**
```nsis
ReadRegStr $VERSION HKLM "Software\App" "Version"
${If} $VERSION == ""
  StrCpy $VERSION "1.0"
${EndIf}
```

### 4.5 Environment Variable Reading

**Wise:**
```
item: Get Environment Variable
  Variable=COMPUTERNAME
  Environment=COMPUTERNAME
end
```

**NSIS:**
```nsis
ReadEnvStr $COMPUTERNAME "COMPUTERNAME"
```

---

## 5. Command Mapping Reference

### 5.1 File Operations

| Wise Command | NSIS Equivalent |
|--------------|-----------------|
| `Install File` | `SetOutPath` + `File` |
| `Copy Local File` | `CopyFiles` |
| `Delete File` | `Delete` |
| `Create Directory` | `CreateDirectory` |
| `Check if File/Dir Exists` | `IfFileExists` or `${FileExists}` |
| `Rename File/Directory` | `Rename` |

### 5.2 Registry Operations

| Wise Command | NSIS Equivalent |
|--------------|-----------------|
| `Edit Registry` (string) | `WriteRegStr` |
| `Edit Registry` (DWORD) | `WriteRegDWORD` |
| `Get Registry Key Value` | `ReadRegStr` / `ReadRegDWORD` |
| Delete registry key | `DeleteRegKey` |
| Delete registry value | `DeleteRegValue` |

### 5.3 INI File Operations

| Wise Command | NSIS Equivalent |
|--------------|-----------------|
| `Edit INI File` | `WriteINIStr` |
| `Read INI Value` | `ReadINIStr` |
| Delete INI entry | `DeleteINIStr` |

### 5.4 Shortcuts

| Wise Command | NSIS Equivalent |
|--------------|-----------------|
| `Create Shortcut` | `CreateShortCut` |
| Create folder | `CreateDirectory "$SMPROGRAMS\Folder"` |

### 5.5 Program Execution

| Wise Command | NSIS Equivalent |
|--------------|-----------------|
| `Execute Program` (wait) | `ExecWait` |
| `Execute Program` (no wait) | `Exec` |
| `Execute Program` (shell) | `ExecShell` |

### 5.6 Service Operations

| Wise Command | NSIS Equivalent (SimpleSC plugin) |
|--------------|-----------------------------------|
| `Start/Stop Service` (start) | `SimpleSC::StartService` |
| `Start/Stop Service` (stop) | `SimpleSC::StopService` |
| `Check Service` | `SimpleSC::ExistsService` |
| Create service | `SimpleSC::InstallService` |
| Remove service | `SimpleSC::RemoveService` |

### 5.7 User Interface

| Wise Command | NSIS Equivalent |
|--------------|-----------------|
| `Display Message` | `MessageBox` |
| `Custom Dialog Set` | `nsDialogs` functions |
| `Wizard Block` | MUI2 page macros |

### 5.8 Control Flow

| Wise Command | NSIS Equivalent |
|--------------|-----------------|
| `If/While Statement` | `${If}` / `${While}` |
| `Else Statement` | `${Else}` |
| `ElseIf Statement` | `${ElseIf}` |
| `End Block` | `${EndIf}` / `${EndWhile}` |
| `Exit Installation` | `Abort` or `Quit` |

### 5.9 Logging

| Wise Command | NSIS Equivalent |
|--------------|-----------------|
| `Open/Close INSTALL.LOG` | Custom logging (see STCLogging.nsh) |
| `Add Text to INSTALL.LOG` | `DetailPrint` or custom log function |
| `Remark` | `; comment` |

---

## 6. Control Flow Conversion

### 6.1 If Statement

**Wise:**
```
item: If/While Statement
  Variable=OSVERSION
  Value=Windows 10
  Flags=00000011
end
  item: Display Message
    Title=Info
    Text=Windows 10 detected
  end
item: Else Statement
end
  item: Display Message
    Title=Info
    Text=Other Windows version
  end
item: End Block
end
```

**NSIS:**
```nsis
${If} $OSVERSION == "Windows 10"
  MessageBox MB_OK "Windows 10 detected"
${Else}
  MessageBox MB_OK "Other Windows version"
${EndIf}
```

### 6.2 Flag-Based Comparison Operators

| Wise Flags | Meaning | NSIS Equivalent |
|------------|---------|-----------------|
| `00000000` | Equals | `==` |
| `00000001` | Not equals | `!=` |
| `00000010` | Greater than | `>` (with IntCmp) |
| `00000011` | String contains | `${StrStr}` check |
| `00001000` | String NOT contains | `${StrStr}` with negation |

### 6.3 While Loop

**Wise:**
```
item: Set Variable
  Variable=COUNTER
  Value=0
end
item: If/While Statement
  Variable=COUNTER
  Value=10
  Flags=00000010
end
  ; loop body
  item: Set Variable
    Variable=COUNTER
    Value=%COUNTER%+1
  end
item: End Block
end
```

**NSIS:**
```nsis
StrCpy $COUNTER 0
${While} $COUNTER < 10
  ; loop body
  IntOp $COUNTER $COUNTER + 1
${EndWhile}
```

### 6.4 Nested Conditions

**Wise:**
```
item: If/While Statement
  Variable=CONDITION1
  Value=TRUE
end
  item: If/While Statement
    Variable=CONDITION2
    Value=TRUE
  end
    ; nested action
  item: End Block
  end
item: End Block
end
```

**NSIS:**
```nsis
${If} $CONDITION1 == "TRUE"
  ${If} $CONDITION2 == "TRUE"
    ; nested action
  ${EndIf}
${EndIf}
```

---

## 7. Dialog/UI Conversion

### 7.1 Wizard Block to MUI2 Pages

**Wise Wizard Block:**
```
item: Wizard Block
  Direction Variable=DIRECTION
  Display Variable=DISPLAY
  Bitmap Pathname=%_WISE_%\DIALOGS\TEMPLATE\TopImage.bmp
end
  item: Custom Dialog Set
    Name=Welcome
    ; dialog contents
  end
  item: Custom Dialog Set
    Name=License
    ; dialog contents
  end
  item: Custom Dialog Set
    Name=SelectDir
    ; dialog contents
  end
item: End Block
end
```

**NSIS Equivalent:**
```nsis
!include "MUI2.nsh"

; Page configuration
!define MUI_WELCOMEPAGE_TITLE "Welcome"
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_BITMAP "header.bmp"

; Pages
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "license.txt"
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_LANGUAGE "English"
```

### 7.2 Custom Dialog Conversion

**Wise Custom Dialog:**
```
item: Custom Dialog Set
  Name=CustomPage
  item: Dialog
    Title=Custom Settings
    Width=290
    Height=238

    item: Static
      Rectangle=10 10 280 30
      Text=Enter your settings:
    end

    item: Editbox
      Rectangle=10 40 280 55
      Value=%USERNAME%
      Variable=USERNAME
    end

    item: Checkbox
      Rectangle=10 60 280 75
      Variable=OPTION1
      Text=Enable feature
    end

    item: Push Button
      Rectangle=186 200 228 214
      Variable=DIRECTION
      Value=N
      Text=&Next >
    end
  end
end
```

**NSIS Equivalent:**
```nsis
Var USERNAME
Var OPTION1
Var hCtl_Username
Var hCtl_Option1

Function CustomPageCreate
  !insertmacro MUI_HEADER_TEXT "Custom Settings" "Enter your settings"

  nsDialogs::Create 1018
  Pop $0

  ${NSD_CreateLabel} 0 0 100% 20u "Enter your settings:"
  Pop $0

  ${NSD_CreateText} 0 25u 100% 12u "$USERNAME"
  Pop $hCtl_Username

  ${NSD_CreateCheckbox} 0 45u 100% 12u "Enable feature"
  Pop $hCtl_Option1
  ${If} $OPTION1 == "1"
    ${NSD_Check} $hCtl_Option1
  ${EndIf}

  nsDialogs::Show
FunctionEnd

Function CustomPageLeave
  ${NSD_GetText} $hCtl_Username $USERNAME
  ${NSD_GetState} $hCtl_Option1 $OPTION1
FunctionEnd

; In page declarations:
Page custom CustomPageCreate CustomPageLeave
```

### 7.3 Message Box Conversion

**Wise:**
```
item: Display Message
  Title=Error
  Text=Installation failed!
  Flags=00100000
end
```

**NSIS:**
```nsis
MessageBox MB_OK|MB_ICONSTOP "Installation failed!"
```

**Flag Mapping:**

| Wise Flags | NSIS Equivalent |
|------------|-----------------|
| `00000000` | `MB_OK` |
| `00000001` | `MB_OKCANCEL` |
| `00000010` | `MB_YESNO` |
| `00000011` | `MB_YESNOCANCEL` |
| `00010000` | `MB_ICONINFORMATION` |
| `00100000` | `MB_ICONSTOP` |
| `00110000` | `MB_ICONEXCLAMATION` |

---

## 8. File Operations

### 8.1 Install File

**Wise:**
```
item: Install File
  Source=C:\Build\Output\app.exe
  Destination=%MAINDIR%\bin\app.exe
  Flags=0000000100000010
end
```

**NSIS:**
```nsis
SetOutPath "$INSTDIR\bin"
File "C:\Build\Output\app.exe"
```

### 8.2 Wildcard File Copy

**Wise:**
```
item: Install File
  Source=C:\Build\Output\*.dll
  Destination=%MAINDIR%\bin\
  Flags=0000000100000010
end
```

**NSIS:**
```nsis
SetOutPath "$INSTDIR\bin"
File "C:\Build\Output\*.dll"
```

### 8.3 Recursive Directory Copy

**Wise:**
```
item: Install File
  Source=C:\Build\Output\*.*
  Destination=%MAINDIR%\
  Flags=0000000100010010
end
```

**NSIS:**
```nsis
SetOutPath "$INSTDIR"
File /r "C:\Build\Output\*.*"
```

### 8.4 Copy Local File (Runtime)

**Wise:**
```
item: Copy Local File
  Source=%TEMP%\config.bak
  Destination=%MAINDIR%\config.ini
  Flags=0000000011000010
end
```

**NSIS:**
```nsis
CopyFiles /SILENT "$TEMP\config.bak" "$INSTDIR\config.ini"
```

### 8.5 Delete File

**Wise:**
```
item: Delete File
  Pathname=%MAINDIR%\oldfile.exe
end
```

**NSIS:**
```nsis
Delete "$INSTDIR\oldfile.exe"
; Or with reboot flag for locked files:
Delete /REBOOTOK "$INSTDIR\oldfile.exe"
```

### 8.6 File Existence Check

**Wise:**
```
item: Check if File/Dir Exists
  Pathname=%MAINDIR%\required.dll
  Flags=01000100
end
  ; actions if exists
item: Else Statement
end
  ; actions if not exists
item: End Block
end
```

**NSIS:**
```nsis
IfFileExists "$INSTDIR\required.dll" file_exists file_not_exists
file_exists:
  ; actions if exists
  Goto done
file_not_exists:
  ; actions if not exists
done:

; Or with LogicLib:
${If} ${FileExists} "$INSTDIR\required.dll"
  ; actions if exists
${Else}
  ; actions if not exists
${EndIf}
```

---

## 9. Registry Operations

### 9.1 Write String Value

**Wise:**
```
item: Edit Registry
  Total Keys=1
  Key=Software\SomeTechCo\App
  New Value=%MAINDIR%
  Value Name=InstallPath
  Root=2
end
```

**NSIS:**
```nsis
WriteRegStr HKLM "Software\SomeTechCo\App" "InstallPath" "$INSTDIR"
```

### 9.2 Root Key Mapping

| Wise Root | NSIS Key |
|-----------|----------|
| 0 | `HKCR` (HKEY_CLASSES_ROOT) |
| 1 | `HKCU` (HKEY_CURRENT_USER) |
| 2 | `HKLM` (HKEY_LOCAL_MACHINE) |
| 3 | `HKU` (HKEY_USERS) |

### 9.3 Read Registry Value

**Wise:**
```
item: Get Registry Key Value
  Variable=EXISTING_PATH
  Key=Software\SomeTechCo\App
  Value Name=InstallPath
  Default=
  Flags=00000100
end
```

**NSIS:**
```nsis
ReadRegStr $EXISTING_PATH HKLM "Software\SomeTechCo\App" "InstallPath"
```

### 9.4 Delete Registry Key

**Wise:**
```
item: Edit Registry
  Total Keys=1
  Key=Software\SomeTechCo\App
  Flags=00000001
  Root=2
end
```

**NSIS:**
```nsis
DeleteRegKey HKLM "Software\SomeTechCo\App"
```

### 9.5 Add/Remove Programs Entry

**Wise (typically in Global or via registry):**
```
item: Edit Registry
  Total Keys=5
  Key=Software\Microsoft\Windows\CurrentVersion\Uninstall\AppName
  New Value=App Display Name
  Value Name=DisplayName
  Root=2
end
```

**NSIS:**
```nsis
!define UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\AppName"

WriteRegStr HKLM "${UNINST_KEY}" "DisplayName" "App Display Name"
WriteRegStr HKLM "${UNINST_KEY}" "UninstallString" "$INSTDIR\uninstall.exe"
WriteRegStr HKLM "${UNINST_KEY}" "InstallLocation" "$INSTDIR"
WriteRegStr HKLM "${UNINST_KEY}" "Publisher" "Company Name"
WriteRegStr HKLM "${UNINST_KEY}" "DisplayVersion" "1.0.0"
WriteRegDWORD HKLM "${UNINST_KEY}" "NoModify" 1
WriteRegDWORD HKLM "${UNINST_KEY}" "NoRepair" 1
```

---

## 10. Service Management

### 10.1 Plugin Requirement

NSIS requires the **SimpleSC** plugin for service management. Ensure it's available:
```nsis
!addplugindir /x86-unicode "path\to\plugins"
```

### 10.2 Start Service

**Wise:**
```
item: Start/Stop Service
  Service Name=MyService
  Flags=00000000
end
```

**NSIS:**
```nsis
SimpleSC::StartService "MyService" "" 30
Pop $0
${If} $0 != 0
  MessageBox MB_OK "Failed to start service"
${EndIf}
```

### 10.3 Stop Service

**Wise:**
```
item: Start/Stop Service
  Service Name=MyService
  Flags=00000001
end
```

**NSIS:**
```nsis
SimpleSC::StopService "MyService" 1 30
Pop $0
```

### 10.4 Check Service Status

**Wise (via custom script):**
```
item: Custom Script Item
  Filename=Check Service.wse
  Variable Name1=_SRV_NAME_
  Variable Value1=MyService
  Variable Name3=_SRV_VAR_
  Variable Value3=SERVICE_STATUS
end
```

**NSIS:**
```nsis
SimpleSC::ServiceIsRunning "MyService"
Pop $0  ; error code
Pop $1  ; 1=running, 0=stopped
${If} $1 == 1
  DetailPrint "Service is running"
${Else}
  DetailPrint "Service is stopped"
${EndIf}
```

### 10.5 Install Service

**Wise (via Execute Program + InstallUtil):**
```
item: Execute Program
  Pathname=%NETROOT%\InstallUtil.exe
  Command Line="%MAINDIR%\MyService.exe" /i
end
```

**NSIS:**
```nsis
SimpleSC::InstallService "MyService" "My Service Display Name" "16" "2" "$INSTDIR\MyService.exe" "" "" ""
Pop $0
; Parameters: ServiceName, DisplayName, ServiceType, StartType, BinaryPath, Dependencies, Username, Password
; ServiceType: 16 = SERVICE_WIN32_OWN_PROCESS
; StartType: 2 = SERVICE_AUTO_START, 3 = SERVICE_DEMAND_START
```

### 10.6 Remove Service

**Wise (via Execute Program + InstallUtil):**
```
item: Execute Program
  Pathname=%NETROOT%\InstallUtil.exe
  Command Line="%MAINDIR%\MyService.exe" /u
end
```

**NSIS:**
```nsis
SimpleSC::RemoveService "MyService"
Pop $0
```

---

## 11. Multilingual Support

### 11.1 Wise Language Support

Wise supports inline translations for 10 languages:
```
item: Static
  Text=English text
  Text French=Texte français
  Text German=Deutscher Text
  Text Spanish=Texto español
end
```

### 11.2 NSIS Language Support

**Method 1: Language Strings**
```nsis
!include "MUI2.nsh"

; Define strings for each language
LangString DESC_MainSection ${LANG_ENGLISH} "Main application files"
LangString DESC_MainSection ${LANG_FRENCH} "Fichiers d'application principaux"
LangString DESC_MainSection ${LANG_GERMAN} "Hauptanwendungsdateien"

; Include languages
!insertmacro MUI_LANGUAGE "English"
!insertmacro MUI_LANGUAGE "French"
!insertmacro MUI_LANGUAGE "German"

; Use strings
Section "Main" SecMain
  DetailPrint $(DESC_MainSection)
SectionEnd
```

**Method 2: External Language Files**
```nsis
!include "English.nsh"
!include "French.nsh"

; In English.nsh:
LangString MSG_WELCOME ${LANG_ENGLISH} "Welcome to the installer"

; In French.nsh:
LangString MSG_WELCOME ${LANG_FRENCH} "Bienvenue dans l'installateur"
```

### 11.3 Language Mapping

| Wise Language | NSIS Language |
|---------------|---------------|
| (default/English) | `${LANG_ENGLISH}` |
| French | `${LANG_FRENCH}` |
| German | `${LANG_GERMAN}` |
| Spanish | `${LANG_SPANISH}` |
| Italian | `${LANG_ITALIAN}` |
| Portuguese | `${LANG_PORTUGUESE}` |
| Dutch | `${LANG_DUTCH}` |
| Danish | `${LANG_DANISH}` |
| Norwegian | `${LANG_NORWEGIAN}` |
| Swedish | `${LANG_SWEDISH}` |

---

## 12. Conversion Process Steps

### 12.1 Phase 1: Analysis

1. **Parse the .wse file** - Extract all `item: ... end` blocks
2. **Identify Global section** - Extract metadata, variables, output path
3. **Map variables** - Create list of all custom variables
4. **Identify components** - List all major installation sections
5. **Catalog dialogs** - Document all Custom Dialog Sets

### 12.2 Phase 2: Structure Creation

1. **Create .nsi skeleton:**
   ```nsis
   Unicode True
   !include "MUI2.nsh"
   !include "LogicLib.nsh"

   Name "Application Name"
   OutFile "Setup.exe"
   InstallDir "$PROGRAMFILES\App"
   RequestExecutionLevel admin

   ; Variables
   Var VAR1
   Var VAR2

   ; Pages
   !insertmacro MUI_PAGE_WELCOME
   ; ... more pages

   ; Sections
   Section "Main"
   SectionEnd
   ```

2. **Create supporting .nsh files** for shared code

### 12.3 Phase 3: Command Translation

1. **Translate commands** using mapping tables
2. **Convert variable references** (`%VAR%` → `$VAR`)
3. **Convert conditionals** to LogicLib syntax
4. **Convert flags** to NSIS equivalents

### 12.4 Phase 4: Dialog Conversion

1. **Convert Wizard Blocks** to MUI2 pages
2. **Convert Custom Dialogs** to nsDialogs functions
3. **Map control types** (Static→Label, Editbox→Text, etc.)
4. **Implement event handlers** as page functions

### 12.5 Phase 5: Testing & Validation

1. **Compile with makensis.exe**
2. **Test installation flow**
3. **Verify file operations**
4. **Test registry changes**
5. **Verify service operations**
6. **Test uninstaller**

---

## 13. SomeTechCo-Specific Patterns

### 13.1 Standard Include Files

Create these standard includes for SomeTechCo NSIS scripts:

```
Lib\nsis\include\
├── STCInstallerCore.nsh    ; Core framework
├── bmtDefinesAndVars.nsh   ; Constants & variables
├── bmtLogging.nsh          ; Logging functions
├── bmtUtilities.nsh        ; Utility functions
├── bmtSystemInfo.nsh       ; System detection
└── bmtISMUninstall.nsh     ; Uninstall handling
```

### 13.2 Registry Keys Pattern

**Wise pattern:**
```
Software\SomeTechCo\
Software\SomeTechCo\StcNet\
Software\SomeTechCo\Version 4.0 Install
```

**NSIS defines:**
```nsis
!define STC_REG_ROOT "Software\SomeTechCo"
!define STC_NET_REG_KEY "${STC_REG_ROOT}\StcNet"
!define STC_VERSION_KEY "${STC_REG_ROOT}\Version 6.0 Install"
```

### 13.3 Path Variables Pattern

```nsis
!define STC_INSTALL_BASE "$PROGRAMFILES\SomeTechCo"
Var STC_BinDir
Var STC_SettingsDir

Function .onInit
  StrCpy $STC_BinDir "$INSTDIR\bin"
  StrCpy $STC_SettingsDir "$INSTDIR\bin\Settings"
FunctionEnd
```

### 13.4 Service Names

```nsis
!define SVC_STC_MONITOR "svcStcServiceMonitor"
!define SVC_STC_HOST "svcStcNetHost"
```

### 13.5 .NET Framework Detection

**Wise pattern (registry check):**
```
item: Get Registry Key Value
  Variable=NET48INSTALLED
  Key=SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full
  Value Name=Release
  Flags=00000100
end
```

**NSIS equivalent:**
```nsis
ReadRegDWORD $0 HKLM "SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full" "Release"
${If} $0 >= 528040
  ; .NET 4.8 or later installed
${Else}
  ; .NET 4.8 not installed
${EndIf}
```

---

## Appendix A: Complete Command Mapping Table

| # | Wise Command | NSIS Equivalent | Notes |
|---|--------------|-----------------|-------|
| 1 | `Global` | Script header + `.onInit` | Metadata, variables |
| 2 | `Set Variable` | `StrCpy $VAR "value"` | |
| 3 | `Get Registry Key Value` | `ReadRegStr` / `ReadRegDWORD` | |
| 4 | `Get System Information` | Various methods | See WinVer.nsh |
| 5 | `Get Environment Variable` | `ReadEnvStr` | |
| 6 | `Get Temporary Filename` | `GetTempFileName` | |
| 7 | `If/While Statement` | `${If}` / `${While}` | LogicLib.nsh |
| 8 | `Else Statement` | `${Else}` | |
| 9 | `ElseIf Statement` | `${ElseIf}` | |
| 10 | `End Block` | `${EndIf}` / `${EndWhile}` | |
| 11 | `Install File` | `SetOutPath` + `File` | |
| 12 | `Copy Local File` | `CopyFiles` | |
| 13 | `Delete File` | `Delete` | |
| 14 | `Create Directory` | `CreateDirectory` | |
| 15 | `Create Shortcut` | `CreateShortCut` | |
| 16 | `Edit Registry` | `WriteRegStr` / `WriteRegDWORD` | |
| 17 | `Read INI Value` | `ReadINIStr` | |
| 18 | `Edit INI File` | `WriteINIStr` | |
| 19 | `Read/Update Text File` | `FileRead` / `FileWrite` | |
| 20 | `Insert Line into Text File` | Custom function | |
| 21 | `Check if File/Dir Exists` | `IfFileExists` / `${FileExists}` | |
| 22 | `Check Configuration` | Various checks | |
| 23 | `Execute Program` | `ExecWait` / `Exec` | |
| 24 | `Start/Stop Service` | `SimpleSC::StartService` / `StopService` | Plugin |
| 25 | `Check Disk Space` | `GetDiskFreeSpace` | |
| 26 | `Self-Register OCXs/DLLs` | `RegDLL` / `UnRegDLL` | |
| 27 | `Display Message` | `MessageBox` | |
| 28 | `Check In-use File` | `LockedList` plugin | |
| 29 | `Check Service` | `SimpleSC::ExistsService` | Plugin |
| 30 | `Parse String` | `${StrStr}` / `${WordFind}` | StrFunc.nsh |
| 31 | `Custom Dialog Set` | `nsDialogs` functions | |
| 32 | `Wizard Block` | MUI2 page macros | |
| 33 | `Open/Close INSTALL.LOG` | Custom logging | |
| 34 | `Add Text to INSTALL.LOG` | `DetailPrint` / custom | |
| 35 | `Remark` | `; comment` | |
| 36 | `Include Script` | `!include "file.nsh"` | |
| 37 | `Exit Installation` | `Abort` / `Quit` | |

---

## Appendix B: Sample Conversion

### B.1 Wise Source (Simplified StcNetSetup.wse excerpt)

```
Document Type: WSE

item: Global
  Version=9.02
  Title=STC.NET Service Architecture
  EXE Filename=..\..\SCM\BuildOutput\Build\Installations\StcNet\Setup.exe
  Variable Name1=MAINDIR
  Variable Default1=C:\Program Files\SomeTechCo\StcNet
  Requested Execution Level=requireAdministrator
end

item: Get Registry Key Value
  Variable=EXISTING_INSTALL
  Key=Software\SomeTechCo\StcNet
  Value Name=RootInstPath
  Flags=00000100
end

item: If/While Statement
  Variable=EXISTING_INSTALL
  Value=
  Flags=00000001
end
  item: Display Message
    Title=Upgrade Detected
    Text=An existing installation was found at %EXISTING_INSTALL%
    Flags=00010000
  end
item: End Block
end

item: Start/Stop Service
  Service Name=svcStcServiceMonitor
  Flags=00000001
end

item: Install File
  Source=..\..\Output\StcServiceMonitor.exe
  Destination=%MAINDIR%\bin\StcServiceMonitor.exe
  Flags=0000000100000010
end

item: Edit Registry
  Total Keys=1
  Key=Software\SomeTechCo\StcNet
  New Value=%MAINDIR%
  Value Name=RootInstPath
  Root=2
end

item: Start/Stop Service
  Service Name=svcStcServiceMonitor
  Flags=00000000
end
```

### B.2 NSIS Conversion

```nsis
;--------------------------------------------------
; STC.NET Service Architecture Installer
; Converted from StcNetSetup.wse
;--------------------------------------------------

Unicode True
!include "MUI2.nsh"
!include "LogicLib.nsh"

; Metadata
Name "STC.NET Service Architecture"
OutFile "..\..\SCM\BuildOutput\Build\Installations\StcNet\Setup.exe"
InstallDir "$PROGRAMFILES\SomeTechCo\StcNet"
InstallDirRegKey HKLM "Software\SomeTechCo\StcNet" "RootInstPath"
RequestExecutionLevel admin

; Variables
Var EXISTING_INSTALL

; Modern UI Configuration
!define MUI_ABORTWARNING

; Pages
!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

!insertmacro MUI_LANGUAGE "English"

;--------------------------------------------------
; Initialization
;--------------------------------------------------
Function .onInit
  ; Check for existing installation
  ReadRegStr $EXISTING_INSTALL HKLM "Software\SomeTechCo\StcNet" "RootInstPath"

  ${If} $EXISTING_INSTALL != ""
    MessageBox MB_OK|MB_ICONINFORMATION "An existing installation was found at $EXISTING_INSTALL"
  ${EndIf}
FunctionEnd

;--------------------------------------------------
; Main Installation Section
;--------------------------------------------------
Section "STC.NET Services" SecMain
  SectionIn RO

  ; Stop service before update
  SimpleSC::StopService "svcStcServiceMonitor" 1 30
  Pop $0

  ; Install files
  SetOutPath "$INSTDIR\bin"
  File "..\..\Output\StcServiceMonitor.exe"

  ; Update registry
  WriteRegStr HKLM "Software\SomeTechCo\StcNet" "RootInstPath" "$INSTDIR"

  ; Start service
  SimpleSC::StartService "svcStcServiceMonitor" "" 30
  Pop $0

  ; Create uninstaller
  WriteUninstaller "$INSTDIR\uninstall.exe"
SectionEnd

;--------------------------------------------------
; Uninstaller Section
;--------------------------------------------------
Section "Uninstall"
  SimpleSC::StopService "svcStcServiceMonitor" 1 30

  Delete "$INSTDIR\bin\StcServiceMonitor.exe"
  Delete "$INSTDIR\uninstall.exe"
  RMDir "$INSTDIR\bin"
  RMDir "$INSTDIR"

  DeleteRegKey HKLM "Software\SomeTechCo\StcNet"
SectionEnd
```

---

## Summary

This guide provides a comprehensive framework for converting Wise Install scripts to NSIS. The key steps are:

1. **Understand both formats** - Wise is declarative, NSIS is procedural
2. **Map variables** - `%VAR%` → `$VAR`
3. **Convert commands** - Use the mapping tables
4. **Convert dialogs** - Wizard Blocks → MUI2 pages, Custom Dialogs → nsDialogs
5. **Handle services** - Use SimpleSC plugin
6. **Test thoroughly** - Verify all functionality

For automation of this conversion process, consider building a utility application that:
- Parses .wse files into an AST (Abstract Syntax Tree)
- Transforms the AST to NSIS structure
- Generates .nsi/.nsh output files
- Handles edge cases and SomeTechCo-specific patterns

---

*Document End*
