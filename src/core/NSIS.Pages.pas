unit NSIS.Pages;

{******************************************************************************
  WiseToNSIS Converter - NSIS Page Template Generator

  Generates nsDialogs page templates from Wise Custom Dialog Sets.
  Based on patterns from Bmt.SageSql NSIS installer scripts.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  WSE.AST;

type
  { Control type for nsDialogs }
  TNsisControlType = (
    nctLabel,
    nctText,
    nctPassword,
    nctButton,
    nctCheckbox,
    nctRadioButton,
    nctComboBox,
    nctListBox,
    nctBitmap,
    nctGroupBox
  );

  { Control definition }
  TNsisControl = class
  private
    FName: string;
    FControlType: TNsisControlType;
    FX, FY, FWidth, FHeight: Integer;
    FText: string;
    FVariable: string;
    FValue: string;
    FHasClickHandler: Boolean;
    FHasChangeHandler: Boolean;
  public
    constructor Create(const AName: string; AType: TNsisControlType);

    property Name: string read FName write FName;
    property ControlType: TNsisControlType read FControlType write FControlType;
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Text: string read FText write FText;
    property Variable: string read FVariable write FVariable;
    property Value: string read FValue write FValue;
    property HasClickHandler: Boolean read FHasClickHandler write FHasClickHandler;
    property HasChangeHandler: Boolean read FHasChangeHandler write FHasChangeHandler;
  end;

  { Page definition }
  TNsisPageDef = class
  private
    FName: string;
    FTitle: string;
    FSubtitle: string;
    FWidth: Integer;
    FHeight: Integer;
    FControls: TObjectList<TNsisControl>;
    FPreFunction: string;
    FShowFunction: string;
    FLeaveFunction: string;
    FVariables: TStringList;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure AddControl(AControl: TNsisControl);
    function GenerateHeader: string;
    function GeneratePreFunction: string;
    function GenerateShowFunction: string;
    function GenerateLeaveFunction: string;

    property Name: string read FName write FName;
    property Title: string read FTitle write FTitle;
    property Subtitle: string read FSubtitle write FSubtitle;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Controls: TObjectList<TNsisControl> read FControls;
    property Variables: TStringList read FVariables;
  end;

  { Page template generator }
  TNsisPageGenerator = class
  private
    FPages: TObjectList<TNsisPageDef>;
    FOutput: TStringList;

    procedure GenerateControlCreation(APage: TNsisPageDef);
    procedure GenerateEventHandlers(APage: TNsisPageDef);
    function ControlTypeToNsis(AType: TNsisControlType): string;
    function ConvertRectangle(const ARect: string; out X, Y, W, H: Integer): Boolean;
    function ScaleDialogUnits(AValue: Integer; AIsWidth: Boolean): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    { Convert Wise dialog to NSIS page }
    function ConvertDialog(ADialog: TWseCustomDialogBlock): TNsisPageDef;

    { Generate complete page template file }
    function GeneratePageFile(APage: TNsisPageDef): string;

    { Generate all pages as single .nsh file }
    function GenerateAllPages: string;

    property Pages: TObjectList<TNsisPageDef> read FPages;
  end;

{ Utility functions }
function SanitizeNsisName(const AName: string): string;

implementation

uses
  System.StrUtils, Conversion.Types;

const
  WISE_DIALOG_WIDTH = 290;
  WISE_DIALOG_HEIGHT = 238;
  NSIS_PAGE_WIDTH = 300;  // Standard MUI2 page width in DLU
  NSIS_PAGE_HEIGHT = 140; // Standard MUI2 page height in DLU

function SanitizeNsisName(const AName: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(AName) do
  begin
    if CharInSet(AName[i], ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      Result := Result + AName[i]
    else if AName[i] = ' ' then
      Result := Result + '_';
  end;
  if (Length(Result) > 0) and CharInSet(Result[1], ['0'..'9']) then
    Result := '_' + Result;
end;

{ TNsisControl }

constructor TNsisControl.Create(const AName: string; AType: TNsisControlType);
begin
  inherited Create;
  FName := AName;
  FControlType := AType;
  FX := 0;
  FY := 0;
  FWidth := 100;
  FHeight := 12;
  FText := '';
  FVariable := '';
  FValue := '';
  FHasClickHandler := False;
  FHasChangeHandler := False;
end;

{ TNsisPageDef }

constructor TNsisPageDef.Create(const AName: string);
begin
  inherited Create;
  FName := SanitizeNsisName(AName);
  FTitle := AName;
  FSubtitle := '';
  FWidth := NSIS_PAGE_WIDTH;
  FHeight := NSIS_PAGE_HEIGHT;
  FControls := TObjectList<TNsisControl>.Create(True);
  FVariables := TStringList.Create;
  FVariables.Duplicates := dupIgnore;
  FVariables.Sorted := True;
end;

destructor TNsisPageDef.Destroy;
begin
  FControls.Free;
  FVariables.Free;
  inherited Destroy;
end;

procedure TNsisPageDef.AddControl(AControl: TNsisControl);
begin
  FControls.Add(AControl);
  if AControl.Variable <> '' then
    FVariables.Add(AControl.Variable);
end;

function TNsisPageDef.GenerateHeader: string;
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('; =============================================================================');
    SL.Add('; Page: ' + FName);
    SL.Add('; Generated from Wise Custom Dialog');
    SL.Add('; =============================================================================');
    SL.Add('');

    // Variable declarations
    SL.Add('; Page variables');
    SL.Add('Var Dialog_' + FName);
    for i := 0 to FControls.Count - 1 do
      SL.Add('Var ' + FControls[i].Name);
    SL.Add('');

    // User variables
    if FVariables.Count > 0 then
    begin
      SL.Add('; User variables');
      for i := 0 to FVariables.Count - 1 do
        SL.Add('Var ' + FVariables[i]);
      SL.Add('');
    end;

    // Page declaration
    SL.Add('; Page declaration');
    SL.Add('Page custom Page' + FName + '_Pre Page' + FName + '_Leave');
    SL.Add('');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TNsisPageDef.GeneratePreFunction: string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('Function Page' + FName + '_Pre');
    SL.Add('  !insertmacro MUI_HEADER_TEXT "' + FTitle + '" "' + FSubtitle + '"');
    SL.Add('');
    SL.Add('  nsDialogs::Create 1018');
    SL.Add('  Pop $Dialog_' + FName);
    SL.Add('  ${If} $Dialog_' + FName + ' == error');
    SL.Add('    Abort');
    SL.Add('  ${EndIf}');
    SL.Add('');
    SL.Add('  ; Control creation will be added here');
    SL.Add('');
    SL.Add('  nsDialogs::Show');
    SL.Add('FunctionEnd');
    SL.Add('');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TNsisPageDef.GenerateShowFunction: string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add('Function Page' + FName + '_Show');
    SL.Add('  ; Called after dialog is created');
    SL.Add('FunctionEnd');
    SL.Add('');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TNsisPageDef.GenerateLeaveFunction: string;
var
  SL: TStringList;
  Ctrl: TNsisControl;
begin
  SL := TStringList.Create;
  try
    SL.Add('Function Page' + FName + '_Leave');
    SL.Add('  ; Validate and store values');

    for Ctrl in FControls do
    begin
      if (Ctrl.Variable <> '') and (Ctrl.ControlType in [nctText, nctPassword, nctComboBox]) then
      begin
        SL.Add('  ${NSD_GetText} $' + Ctrl.Name + ' $' + Ctrl.Variable);
      end
      else if (Ctrl.Variable <> '') and (Ctrl.ControlType = nctCheckbox) then
      begin
        SL.Add('  ${NSD_GetState} $' + Ctrl.Name + ' $' + Ctrl.Variable);
      end;
    end;

    SL.Add('FunctionEnd');
    SL.Add('');

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

{ TNsisPageGenerator }

constructor TNsisPageGenerator.Create;
begin
  inherited Create;
  FPages := TObjectList<TNsisPageDef>.Create(True);
  FOutput := TStringList.Create;
end;

destructor TNsisPageGenerator.Destroy;
begin
  FOutput.Free;
  FPages.Free;
  inherited Destroy;
end;

function TNsisPageGenerator.ControlTypeToNsis(AType: TNsisControlType): string;
begin
  case AType of
    nctLabel:       Result := 'Label';
    nctText:        Result := 'Text';
    nctPassword:    Result := 'Password';
    nctButton:      Result := 'Button';
    nctCheckbox:    Result := 'Checkbox';
    nctRadioButton: Result := 'RadioButton';
    nctComboBox:    Result := 'ComboBox';
    nctListBox:     Result := 'ListBox';
    nctBitmap:      Result := 'Bitmap';
    nctGroupBox:    Result := 'GroupBox';
  else
    Result := 'Label';
  end;
end;

function TNsisPageGenerator.ConvertRectangle(const ARect: string; out X, Y, W, H: Integer): Boolean;
var
  Parts: TArray<string>;
  X1, Y1, X2, Y2: Integer;
begin
  Result := False;
  X := 0; Y := 0; W := 100; H := 12;

  // Rectangle format: "X1 Y1 X2 Y2"
  Parts := ARect.Split([' ']);
  if Length(Parts) < 4 then
    Exit;

  if not TryStrToInt(Trim(Parts[0]), X1) then Exit;
  if not TryStrToInt(Trim(Parts[1]), Y1) then Exit;
  if not TryStrToInt(Trim(Parts[2]), X2) then Exit;
  if not TryStrToInt(Trim(Parts[3]), Y2) then Exit;

  X := ScaleDialogUnits(X1, True);
  Y := ScaleDialogUnits(Y1, False);
  W := ScaleDialogUnits(X2 - X1, True);
  H := ScaleDialogUnits(Y2 - Y1, False);

  Result := True;
end;

function TNsisPageGenerator.ScaleDialogUnits(AValue: Integer; AIsWidth: Boolean): Integer;
begin
  // Scale from Wise dialog units to NSIS dialog units
  // Wise: 290x238, NSIS: 300x140
  if AIsWidth then
    Result := Round(AValue * NSIS_PAGE_WIDTH / WISE_DIALOG_WIDTH)
  else
    Result := Round(AValue * NSIS_PAGE_HEIGHT / WISE_DIALOG_HEIGHT);
end;

function TNsisPageGenerator.ConvertDialog(ADialog: TWseCustomDialogBlock): TNsisPageDef;

  { Recursively find all children of a specific type }
  procedure FindControlsRecursive(ANode: TWseNode; AClass: TWseNodeClass;
    var AList: TList<TWseNode>);
  var
    i: Integer;
  begin
    for i := 0 to ANode.ChildCount - 1 do
    begin
      if ANode.Children[i] is AClass then
        AList.Add(ANode.Children[i]);
      FindControlsRecursive(ANode.Children[i], AClass, AList);
    end;
  end;

var
  Page: TNsisPageDef;
  Ctrl: TNsisControl;
  Child: TWseNode;
  Block: TWseBlock;
  CtrlIndex: Integer;
  X, Y, W, H: Integer;
  CtrlType: TNsisControlType;
  CtrlName: string;
  FoundControls: TList<TWseNode>;
begin
  Page := TNsisPageDef.Create(ADialog.Name);
  Page.Title := ADialog.Name;

  FoundControls := TList<TWseNode>.Create;
  try
    // Find Dialog child block for dimensions (search recursively)
    FoundControls.Clear;
    FindControlsRecursive(ADialog, TWseDialogBlock, FoundControls);
    for Child in FoundControls do
    begin
      Block := TWseBlock(Child);
      if Block.GetProp('Title') <> '' then
        Page.Title := Block.GetProp('Title');
      Page.Width := Block.GetPropInt('Width', NSIS_PAGE_WIDTH);
      Page.Height := Block.GetPropInt('Height', NSIS_PAGE_HEIGHT);
    end;

    CtrlIndex := 0;

    // Convert Static controls (labels) - search recursively
    FoundControls.Clear;
    FindControlsRecursive(ADialog, TWseStaticBlock, FoundControls);
    for Child in FoundControls do
    begin
      Block := TWseBlock(Child);
      CtrlName := Format('Lbl%s_%d', [SanitizeNsisName(Page.Name), CtrlIndex]);

      Ctrl := TNsisControl.Create(CtrlName, nctLabel);
      if ConvertRectangle(Block.GetProp('Rectangle'), X, Y, W, H) then
      begin
        Ctrl.X := X;
        Ctrl.Y := Y;
        Ctrl.Width := W;
        Ctrl.Height := H;
      end;
      Ctrl.Text := Block.GetProp('Text');
      Page.AddControl(Ctrl);
      Inc(CtrlIndex);
    end;

    // Convert Push Button controls - search recursively
    FoundControls.Clear;
    FindControlsRecursive(ADialog, TWsePushButtonBlock, FoundControls);
    for Child in FoundControls do
    begin
      Block := TWseBlock(Child);
      CtrlName := Format('Btn%s_%d', [SanitizeNsisName(Page.Name), CtrlIndex]);

      Ctrl := TNsisControl.Create(CtrlName, nctButton);
      if ConvertRectangle(Block.GetProp('Rectangle'), X, Y, W, H) then
      begin
        Ctrl.X := X;
        Ctrl.Y := Y;
        Ctrl.Width := W;
        Ctrl.Height := H;
      end;
      Ctrl.Text := Block.GetProp('Text');
      Ctrl.Variable := Block.GetProp('Variable');
      Ctrl.Value := Block.GetProp('Value');
      Ctrl.HasClickHandler := True;
      Page.AddControl(Ctrl);
      Inc(CtrlIndex);
    end;

    // Convert Editbox controls - search recursively
    FoundControls.Clear;
    FindControlsRecursive(ADialog, TWseEditboxBlock, FoundControls);
    for Child in FoundControls do
    begin
      Block := TWseBlock(Child);
      CtrlName := Format('Edit%s_%d', [SanitizeNsisName(Page.Name), CtrlIndex]);

      Ctrl := TNsisControl.Create(CtrlName, nctText);
      if ConvertRectangle(Block.GetProp('Rectangle'), X, Y, W, H) then
      begin
        Ctrl.X := X;
        Ctrl.Y := Y;
        Ctrl.Width := W;
        Ctrl.Height := H;
      end;
      Ctrl.Variable := Block.GetProp('Variable');
      Ctrl.Value := Block.GetProp('Value');
      Ctrl.HasChangeHandler := True;
      Page.AddControl(Ctrl);
      Inc(CtrlIndex);
    end;

    // Convert Checkbox controls - search recursively
    FoundControls.Clear;
    FindControlsRecursive(ADialog, TWseCheckboxBlock, FoundControls);
    for Child in FoundControls do
    begin
      Block := TWseBlock(Child);
      CtrlName := Format('Chk%s_%d', [SanitizeNsisName(Page.Name), CtrlIndex]);

      Ctrl := TNsisControl.Create(CtrlName, nctCheckbox);
      if ConvertRectangle(Block.GetProp('Rectangle'), X, Y, W, H) then
      begin
        Ctrl.X := X;
        Ctrl.Y := Y;
        Ctrl.Width := W;
        Ctrl.Height := H;
      end;
      Ctrl.Text := Block.GetProp('Text');
      Ctrl.Variable := Block.GetProp('Variable');
      Ctrl.HasClickHandler := True;
      Page.AddControl(Ctrl);
      Inc(CtrlIndex);
    end;

    // Convert Radio Button controls - search recursively
    FoundControls.Clear;
    FindControlsRecursive(ADialog, TWseRadioButtonBlock, FoundControls);
    for Child in FoundControls do
    begin
      Block := TWseBlock(Child);
      CtrlName := Format('Rad%s_%d', [SanitizeNsisName(Page.Name), CtrlIndex]);

      Ctrl := TNsisControl.Create(CtrlName, nctRadioButton);
      if ConvertRectangle(Block.GetProp('Rectangle'), X, Y, W, H) then
      begin
        Ctrl.X := X;
        Ctrl.Y := Y;
        Ctrl.Width := W;
        Ctrl.Height := H;
      end;
      Ctrl.Text := Block.GetProp('Text');
      Ctrl.Variable := Block.GetProp('Variable');
      Ctrl.Value := Block.GetProp('Value');
      Ctrl.HasClickHandler := True;
      Page.AddControl(Ctrl);
      Inc(CtrlIndex);
    end;

    // Convert Listbox controls - search recursively
    FoundControls.Clear;
    FindControlsRecursive(ADialog, TWseListboxBlock, FoundControls);
    for Child in FoundControls do
    begin
      Block := TWseBlock(Child);
      CtrlName := Format('Lst%s_%d', [SanitizeNsisName(Page.Name), CtrlIndex]);

      Ctrl := TNsisControl.Create(CtrlName, nctListBox);
      if ConvertRectangle(Block.GetProp('Rectangle'), X, Y, W, H) then
      begin
        Ctrl.X := X;
        Ctrl.Y := Y;
        Ctrl.Width := W;
        Ctrl.Height := H;
      end;
      Ctrl.Variable := Block.GetProp('Variable');
      Ctrl.HasChangeHandler := True;
      Page.AddControl(Ctrl);
      Inc(CtrlIndex);
    end;

  finally
    FoundControls.Free;
  end;

  FPages.Add(Page);
  Result := Page;
end;

procedure TNsisPageGenerator.GenerateControlCreation(APage: TNsisPageDef);
var
  Ctrl: TNsisControl;
  CtrlType: string;
begin
  for Ctrl in APage.Controls do
  begin
    CtrlType := ControlTypeToNsis(Ctrl.ControlType);

    // Generate control creation
    FOutput.Add(Format('  ${NSD_Create%s} %du %du %du %du "%s"', [
      CtrlType,
      Ctrl.X,
      Ctrl.Y,
      Ctrl.Width,
      Ctrl.Height,
      Ctrl.Text
    ]));
    FOutput.Add('  Pop $' + Ctrl.Name);

    // Set initial value if applicable
    if (Ctrl.Value <> '') and (Ctrl.ControlType in [nctText, nctPassword]) then
      FOutput.Add(Format('  ${NSD_SetText} $%s "%s"', [Ctrl.Name, Ctrl.Value]));

    // Register event handlers
    if Ctrl.HasClickHandler then
      FOutput.Add(Format('  ${NSD_OnClick} $%s On%sClick', [Ctrl.Name, Ctrl.Name]));

    if Ctrl.HasChangeHandler then
      FOutput.Add(Format('  ${NSD_OnChange} $%s On%sChange', [Ctrl.Name, Ctrl.Name]));

    FOutput.Add('');
  end;
end;

procedure TNsisPageGenerator.GenerateEventHandlers(APage: TNsisPageDef);
var
  Ctrl: TNsisControl;
begin
  for Ctrl in APage.Controls do
  begin
    if Ctrl.HasClickHandler then
    begin
      FOutput.Add('Function On' + Ctrl.Name + 'Click');
      FOutput.Add('  Pop $0  ; Control handle');
      if Ctrl.Variable <> '' then
        FOutput.Add(Format('  StrCpy $%s "%s"', [Ctrl.Variable, Ctrl.Value]));
      FOutput.Add('FunctionEnd');
      FOutput.Add('');
    end;

    if Ctrl.HasChangeHandler then
    begin
      FOutput.Add('Function On' + Ctrl.Name + 'Change');
      FOutput.Add('  Pop $0  ; Control handle');
      if Ctrl.Variable <> '' then
        FOutput.Add(Format('  ${NSD_GetText} $0 $%s', [Ctrl.Variable]));
      FOutput.Add('FunctionEnd');
      FOutput.Add('');
    end;
  end;
end;

function TNsisPageGenerator.GeneratePageFile(APage: TNsisPageDef): string;
var
  Ctrl: TNsisControl;
begin
  FOutput.Clear;

  // File header
  FOutput.Add('; =============================================================================');
  FOutput.Add('; ' + APage.Name + '.nsh');
  FOutput.Add('; Custom page generated from Wise Custom Dialog');
  FOutput.Add('; =============================================================================');
  FOutput.Add('');
  FOutput.Add('!include "MUI2.nsh"');
  FOutput.Add('!include "nsDialogs.nsh"');
  FOutput.Add('!include "LogicLib.nsh"');
  FOutput.Add('');

  // Add header content
  FOutput.Add(APage.GenerateHeader);

  // Pre function with control creation
  FOutput.Add('Function Page' + APage.Name + '_Pre');
  FOutput.Add('  !insertmacro MUI_HEADER_TEXT "' + APage.Title + '" "' + APage.Subtitle + '"');
  FOutput.Add('');
  FOutput.Add('  nsDialogs::Create 1018');
  FOutput.Add('  Pop $Dialog_' + APage.Name);
  FOutput.Add('  ${If} $Dialog_' + APage.Name + ' == error');
  FOutput.Add('    Abort');
  FOutput.Add('  ${EndIf}');
  FOutput.Add('');

  // Control creation
  GenerateControlCreation(APage);

  FOutput.Add('  nsDialogs::Show');
  FOutput.Add('FunctionEnd');
  FOutput.Add('');

  // Leave function
  FOutput.Add(APage.GenerateLeaveFunction);

  // Event handlers
  GenerateEventHandlers(APage);

  Result := FOutput.Text;
end;

function TNsisPageGenerator.GenerateAllPages: string;
var
  Page: TNsisPageDef;
begin
  FOutput.Clear;

  FOutput.Add('; =============================================================================');
  FOutput.Add('; CustomPages.nsh');
  FOutput.Add('; Custom pages generated from Wise Custom Dialogs');
  FOutput.Add('; =============================================================================');
  FOutput.Add('');
  FOutput.Add('!include "MUI2.nsh"');
  FOutput.Add('!include "nsDialogs.nsh"');
  FOutput.Add('!include "LogicLib.nsh"');
  FOutput.Add('');

  for Page in FPages do
  begin
    FOutput.Add(GeneratePageFile(Page));
    FOutput.Add('');
  end;

  Result := FOutput.Text;
end;

end.
