unit WSE.Parser;

{******************************************************************************
  WiseToNSIS Converter - WSE File Parser

  Line-oriented lexer and parser for Wise Install .wse script files.
  Parses the block-based declarative format into an AST.
******************************************************************************}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Conversion.Types, Conversion.Registry, WSE.AST;

type
  { Token types for lexer }
  TWseTokenType = (
    ttNone,
    ttDocumentType,     // Document Type: WSE
    ttItemStart,        // item: BlockType
    ttRemarkedItem,     // remarked item: BlockType
    ttItemEnd,          // end
    ttProperty,         // Key=Value
    ttBlankLine,        // Empty line
    ttEOF               // End of file
  );

  { Token record }
  TWseToken = record
    TokenType: TWseTokenType;
    Line: Integer;
    Column: Integer;
    BlockType: string;      // For ttItemStart
    PropertyKey: string;    // For ttProperty
    PropertyValue: string;  // For ttProperty
    Language: string;       // For multi-language properties
    IsRemarked: Boolean;
    RawText: string;        // Original line text
  end;

  { Forward declaration }
  TWseParser = class;

  { Lexer for tokenizing .wse files }
  TWseLexer = class
  private
    FLines: TStringList;
    FCurrentLine: Integer;
    FIssues: TConversionIssueList;

    function GetCurrentLineText: string;
    function GetIndentLevel(const ALine: string): Integer;
    function ParsePropertyLine(const ALine: string; out AKey, AValue, ALanguage: string): Boolean;
    function ExtractBlockType(const ALine: string): string;
    function IsRemarkedItem(const ALine: string): Boolean;
  public
    constructor Create(AIssues: TConversionIssueList);
    destructor Destroy; override;

    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromString(const AContent: string);

    function NextToken: TWseToken;
    function PeekToken: TWseToken;
    procedure Reset;

    property CurrentLine: Integer read FCurrentLine;
    property Lines: TStringList read FLines;
    property Issues: TConversionIssueList read FIssues;
  end;

  { Parser for building AST from tokens }
  TWseParser = class
  private
    FLexer: TWseLexer;
    FDocument: TWseDocument;
    FIssues: TConversionIssueList;
    FCurrentToken: TWseToken;

    procedure Advance;
    function Match(ATokenType: TWseTokenType): Boolean;
    function Expect(ATokenType: TWseTokenType): Boolean;

    procedure ParseDocument;
    procedure ParseDocumentType;
    procedure ParseBlock(AParent: TWseNode);
    procedure ParseProperties(ABlock: TWseBlock);
    procedure ParseNestedBlocks(ABlock: TWseBlock);

    function CreateBlockFromToken(const AToken: TWseToken): TWseBlock;
    procedure ConfigureGlobalBlock(ABlock: TWseGlobalBlock);
  public
    constructor Create;
    destructor Destroy; override;

    function Parse(const AFilename: string): TWseDocument; overload;
    function ParseString(const AContent: string; const ASourceName: string = ''): TWseDocument; overload;

    property Document: TWseDocument read FDocument;
    property Issues: TConversionIssueList read FIssues;
  end;

  { Parser exception }
  EWseParseError = class(Exception)
  private
    FLine: Integer;
    FColumn: Integer;
  public
    constructor Create(const AMessage: string; ALine, AColumn: Integer);
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
  end;

implementation

uses
  System.StrUtils, System.RegularExpressions;

const
  ITEM_PREFIX = 'item:';
  REMARKED_PREFIX = 'remarked item:';
  END_KEYWORD = 'end';
  DOCUMENT_TYPE_PREFIX = 'Document Type:';

{ EWseParseError }

constructor EWseParseError.Create(const AMessage: string; ALine, AColumn: Integer);
begin
  inherited Create(Format('[%d:%d] %s', [ALine, AColumn, AMessage]));
  FLine := ALine;
  FColumn := AColumn;
end;

{ TWseLexer }

constructor TWseLexer.Create(AIssues: TConversionIssueList);
begin
  inherited Create;
  FLines := TStringList.Create;
  FCurrentLine := 0;
  FIssues := AIssues;
end;

destructor TWseLexer.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TWseLexer.LoadFromFile(const AFilename: string);
begin
  FLines.LoadFromFile(AFilename);
  FCurrentLine := 0;
end;

procedure TWseLexer.LoadFromString(const AContent: string);
begin
  FLines.Text := AContent;
  FCurrentLine := 0;
end;

procedure TWseLexer.Reset;
begin
  FCurrentLine := 0;
end;

function TWseLexer.GetCurrentLineText: string;
begin
  if FCurrentLine < FLines.Count then
    Result := FLines[FCurrentLine]
  else
    Result := '';
end;

function TWseLexer.GetIndentLevel(const ALine: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(ALine) do
  begin
    if ALine[i] = ' ' then
      Inc(Result)
    else
      Break;
  end;
end;

function TWseLexer.ParsePropertyLine(const ALine: string; out AKey, AValue, ALanguage: string): Boolean;
var
  TrimmedLine: string;
  EqPos, SpacePos: Integer;
  BaseKey: string;
begin
  TrimmedLine := Trim(ALine);
  EqPos := Pos('=', TrimmedLine);

  if EqPos > 0 then
  begin
    AKey := Copy(TrimmedLine, 1, EqPos - 1);
    AValue := Copy(TrimmedLine, EqPos + 1, MaxInt);

    // Check for language suffix (e.g., "Text French", "Title German")
    ALanguage := '';
    SpacePos := Pos(' ', AKey);
    if SpacePos > 0 then
    begin
      BaseKey := Copy(AKey, 1, SpacePos - 1);
      ALanguage := Copy(AKey, SpacePos + 1, MaxInt);

      // Only treat as language if it's a known language name
      if SameText(ALanguage, 'French') or SameText(ALanguage, 'German') or
         SameText(ALanguage, 'Spanish') or SameText(ALanguage, 'Italian') or
         SameText(ALanguage, 'Portuguese') or SameText(ALanguage, 'Dutch') or
         SameText(ALanguage, 'Danish') or SameText(ALanguage, 'Norwegian') or
         SameText(ALanguage, 'Swedish') then
      begin
        AKey := BaseKey;
      end
      else
        ALanguage := '';
    end;

    Result := True;
  end
  else
  begin
    AKey := '';
    AValue := '';
    ALanguage := '';
    Result := False;
  end;
end;

function TWseLexer.ExtractBlockType(const ALine: string): string;
var
  TrimmedLine: string;
  StartPos: Integer;
begin
  TrimmedLine := Trim(ALine);

  if StartsText(REMARKED_PREFIX, TrimmedLine) then
    StartPos := Length(REMARKED_PREFIX) + 1
  else if StartsText(ITEM_PREFIX, TrimmedLine) then
    StartPos := Length(ITEM_PREFIX) + 1
  else
  begin
    Result := '';
    Exit;
  end;

  Result := Trim(Copy(TrimmedLine, StartPos, MaxInt));
end;

function TWseLexer.IsRemarkedItem(const ALine: string): Boolean;
begin
  Result := StartsText(REMARKED_PREFIX, Trim(ALine));
end;

function TWseLexer.NextToken: TWseToken;
var
  LineText, TrimmedLine: string;
  Key, Value, Lang: string;
begin
  // Initialize token
  Result.TokenType := ttNone;
  Result.Line := FCurrentLine + 1;
  Result.Column := 1;
  Result.BlockType := '';
  Result.PropertyKey := '';
  Result.PropertyValue := '';
  Result.Language := '';
  Result.IsRemarked := False;
  Result.RawText := '';

  // Check for EOF
  if FCurrentLine >= FLines.Count then
  begin
    Result.TokenType := ttEOF;
    Exit;
  end;

  LineText := FLines[FCurrentLine];
  TrimmedLine := Trim(LineText);
  Result.RawText := LineText;
  Result.Column := GetIndentLevel(LineText) + 1;

  // Skip blank lines
  if TrimmedLine = '' then
  begin
    Result.TokenType := ttBlankLine;
    Inc(FCurrentLine);
    Exit;
  end;

  // Check for Document Type
  if StartsText(DOCUMENT_TYPE_PREFIX, TrimmedLine) then
  begin
    Result.TokenType := ttDocumentType;
    Result.PropertyValue := Trim(Copy(TrimmedLine, Length(DOCUMENT_TYPE_PREFIX) + 1, MaxInt));
    Inc(FCurrentLine);
    Exit;
  end;

  // Check for remarked item
  if StartsText(REMARKED_PREFIX, TrimmedLine) then
  begin
    Result.TokenType := ttRemarkedItem;
    Result.BlockType := ExtractBlockType(TrimmedLine);
    Result.IsRemarked := True;
    Inc(FCurrentLine);
    Exit;
  end;

  // Check for item start
  if StartsText(ITEM_PREFIX, TrimmedLine) then
  begin
    Result.TokenType := ttItemStart;
    Result.BlockType := ExtractBlockType(TrimmedLine);
    Inc(FCurrentLine);
    Exit;
  end;

  // Check for end
  if SameText(TrimmedLine, END_KEYWORD) then
  begin
    Result.TokenType := ttItemEnd;
    Inc(FCurrentLine);
    Exit;
  end;

  // Must be a property line
  if ParsePropertyLine(TrimmedLine, Key, Value, Lang) then
  begin
    Result.TokenType := ttProperty;
    Result.PropertyKey := Key;
    Result.PropertyValue := Value;
    Result.Language := Lang;
    Inc(FCurrentLine);
    Exit;
  end;

  // Unknown line - treat as property without value
  Result.TokenType := ttProperty;
  Result.PropertyKey := TrimmedLine;
  Result.PropertyValue := '';
  Inc(FCurrentLine);

  // Log warning for unrecognized line
  FIssues.AddIssue(csWarning, icParsing,
    Format('Unrecognized line format: %s', [TrimmedLine]),
    Result.Line, Result.Column);
end;

function TWseLexer.PeekToken: TWseToken;
var
  SavedLine: Integer;
begin
  SavedLine := FCurrentLine;
  Result := NextToken;
  FCurrentLine := SavedLine;
end;

{ TWseParser }

constructor TWseParser.Create;
begin
  inherited Create;
  FIssues := TConversionIssueList.Create(True);
  FLexer := TWseLexer.Create(FIssues);
  FDocument := nil;
end;

destructor TWseParser.Destroy;
begin
  FLexer.Free;
  FIssues.Free;
  // Note: FDocument is owned by caller
  inherited Destroy;
end;

function TWseParser.Parse(const AFilename: string): TWseDocument;
begin
  FLexer.LoadFromFile(AFilename);

  // Create new document
  FDocument := TWseDocument.Create;
  FDocument.FilePath := AFilename;

  try
    ParseDocument;
    Result := FDocument;
  except
    on E: Exception do
    begin
      FDocument.Free;
      FDocument := nil;
      raise;
    end;
  end;
end;

function TWseParser.ParseString(const AContent: string; const ASourceName: string): TWseDocument;
begin
  FLexer.LoadFromString(AContent);

  // Create new document
  FDocument := TWseDocument.Create;
  FDocument.FilePath := ASourceName;

  try
    ParseDocument;
    Result := FDocument;
  except
    on E: Exception do
    begin
      FDocument.Free;
      FDocument := nil;
      raise;
    end;
  end;
end;

procedure TWseParser.Advance;
begin
  FCurrentToken := FLexer.NextToken;
end;

function TWseParser.Match(ATokenType: TWseTokenType): Boolean;
begin
  Result := FCurrentToken.TokenType = ATokenType;
end;

function TWseParser.Expect(ATokenType: TWseTokenType): Boolean;
begin
  if not Match(ATokenType) then
  begin
    FIssues.AddIssue(csError, icParsing,
      Format('Expected %d but found %d', [Ord(ATokenType), Ord(FCurrentToken.TokenType)]),
      FCurrentToken.Line, FCurrentToken.Column);
    Result := False;
  end
  else
    Result := True;
end;

procedure TWseParser.ParseDocument;
begin
  Advance;

  // Parse document type if present
  if Match(ttDocumentType) then
    ParseDocumentType;

  // Parse all top-level blocks
  while not Match(ttEOF) do
  begin
    case FCurrentToken.TokenType of
      ttItemStart, ttRemarkedItem:
        ParseBlock(FDocument);
      ttBlankLine:
        Advance;
      ttProperty:
        begin
          // Stray property at document level - warning
          FIssues.AddIssue(csWarning, icParsing,
            Format('Property outside of block: %s', [FCurrentToken.PropertyKey]),
            FCurrentToken.Line, FCurrentToken.Column);
          Advance;
        end;
    else
      Advance;
    end;
  end;

  // Extract variables from Global block
  if FDocument.GetGlobalBlock <> nil then
    TWseGlobalBlock(FDocument.GetGlobalBlock).ExtractVariables(FDocument.Variables);
end;

procedure TWseParser.ParseDocumentType;
begin
  FDocument.DocumentType := FCurrentToken.PropertyValue;
  Advance;
end;

procedure TWseParser.ParseBlock(AParent: TWseNode);
var
  Block: TWseBlock;
  StartToken: TWseToken;
  NestingLevel: Integer;
begin
  StartToken := FCurrentToken;

  // Create the appropriate block type
  Block := CreateBlockFromToken(StartToken);
  Block.Line := StartToken.Line;
  Block.Column := StartToken.Column;
  Block.IsRemarked := StartToken.IsRemarked;

  // Add to parent
  AParent.AddChild(Block);

  Advance;

  // Parse properties and nested blocks
  NestingLevel := 1;

  while not Match(ttEOF) and (NestingLevel > 0) do
  begin
    case FCurrentToken.TokenType of
      ttProperty:
        begin
          // Add property to current block
          Block.Properties.Add(TWseProperty.Create(
            FCurrentToken.PropertyKey,
            FCurrentToken.PropertyValue,
            FCurrentToken.Language
          ));
          Advance;
        end;

      ttItemStart, ttRemarkedItem:
        begin
          // Nested block
          Inc(NestingLevel);
          ParseBlock(Block);
          Dec(NestingLevel);
        end;

      ttItemEnd:
        begin
          Dec(NestingLevel);
          Advance;
        end;

      ttBlankLine:
        Advance;

    else
      Advance;
    end;
  end;

  // Configure specialized blocks
  if Block is TWseGlobalBlock then
    ConfigureGlobalBlock(TWseGlobalBlock(Block));
end;

function TWseParser.CreateBlockFromToken(const AToken: TWseToken): TWseBlock;
begin
  Result := CreateBlock(AToken.BlockType);
end;

procedure TWseParser.ParseProperties(ABlock: TWseBlock);
begin
  while Match(ttProperty) do
  begin
    ABlock.Properties.Add(TWseProperty.Create(
      FCurrentToken.PropertyKey,
      FCurrentToken.PropertyValue,
      FCurrentToken.Language
    ));
    Advance;
  end;
end;

procedure TWseParser.ParseNestedBlocks(ABlock: TWseBlock);
begin
  while Match(ttItemStart) or Match(ttRemarkedItem) do
    ParseBlock(ABlock);
end;

procedure TWseParser.ConfigureGlobalBlock(ABlock: TWseGlobalBlock);
begin
  // Extract key properties into typed fields
  ABlock.Version := ABlock.GetProp('Version');
  ABlock.Title := ABlock.GetProp('Title');
  ABlock.Flags := ABlock.GetProp('Flags');
  ABlock.ExeFilename := ABlock.GetProp('EXE Filename');
  ABlock.RequestedExecutionLevel := ABlock.GetProp('Requested Execution Level');
  ABlock.LogPathname := ABlock.GetProp('Log Pathname');
end;

end.
