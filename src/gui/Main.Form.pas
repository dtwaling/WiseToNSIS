unit Main.Form;

{******************************************************************************
  WiseToNSIS Converter - Main GUI Form

  Main application window with AST tree, NSIS preview, and issues list.
******************************************************************************}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Menus, Vcl.ActnList, System.Actions,
  Conversion.Types, Converter.Engine, WSE.AST, NSIS.Generator;

type
  TfrmMain = class(TForm)
    pnlTop: TPanel;
    pnlMain: TPanel;
    splLeft: TSplitter;
    splRight: TSplitter;
    pnlLeft: TPanel;
    pnlCenter: TPanel;
    pnlRight: TPanel;
    pnlStatus: TPanel;
    tvAST: TTreeView;
    mmoPreview: TMemo;
    lvIssues: TListView;
    btnOpen: TButton;
    btnConvert: TButton;
    btnSave: TButton;
    lblStatus: TLabel;
    pbProgress: TProgressBar;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    lblASTTitle: TLabel;
    lblPreviewTitle: TLabel;
    lblIssuesTitle: TLabel;
    ActionList: TActionList;
    actOpen: TAction;
    actConvert: TAction;
    actSave: TAction;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actConvertExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure tvASTChange(Sender: TObject; Node: TTreeNode);
    procedure lvIssuesDblClick(Sender: TObject);

  private
    FEngine: TConverterEngine;
    FCurrentFile: string;
    FIsConverted: Boolean;

    procedure HandleProgress(Sender: TObject; APhase: TConversionPhase;
      AProgress: Integer; const AMessage: string);
    procedure HandleIssue(Sender: TObject; AIssue: TConversionIssue);

    procedure LoadASTToTree(ADocument: TWseDocument);
    procedure AddNodeToTree(AParentNode: TTreeNode; ANode: TWseNode);
    procedure LoadIssuesToList;
    procedure UpdateUI;
    procedure UpdateStatus(const AMessage: string);
    procedure ClearAll;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  Vcl.Clipbrd;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FEngine := TConverterEngine.Create;
  FEngine.OnProgress := HandleProgress;
  FEngine.OnIssue := HandleIssue;
  FCurrentFile := '';
  FIsConverted := False;

  // Set up list view columns
  lvIssues.ViewStyle := vsReport;
  with lvIssues.Columns.Add do
  begin
    Caption := 'Line';
    Width := 50;
  end;
  with lvIssues.Columns.Add do
  begin
    Caption := 'Severity';
    Width := 70;
  end;
  with lvIssues.Columns.Add do
  begin
    Caption := 'Category';
    Width := 80;
  end;
  with lvIssues.Columns.Add do
  begin
    Caption := 'Message';
    Width := 400;
    AutoSize := True;
  end;

  // Set up preview memo
  mmoPreview.Font.Name := 'Consolas';
  mmoPreview.Font.Size := 9;
  mmoPreview.ScrollBars := ssBoth;
  mmoPreview.WordWrap := False;

  UpdateUI;
  UpdateStatus('Ready. Open a .wse file to begin.');
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FEngine.Free;
end;

procedure TfrmMain.HandleProgress(Sender: TObject; APhase: TConversionPhase;
  AProgress: Integer; const AMessage: string);
begin
  pbProgress.Position := AProgress;
  UpdateStatus(AMessage);
  Application.ProcessMessages;
end;

procedure TfrmMain.HandleIssue(Sender: TObject; AIssue: TConversionIssue);
begin
  // Issues are collected and displayed after conversion
end;

procedure TfrmMain.actOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    FCurrentFile := OpenDialog.FileName;
    ClearAll;
    UpdateStatus('Loaded: ' + ExtractFileName(FCurrentFile));
    UpdateUI;
  end;
end;

procedure TfrmMain.actConvertExecute(Sender: TObject);
begin
  if FCurrentFile = '' then
  begin
    ShowMessage('Please open a .wse file first.');
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    pbProgress.Position := 0;

    if FEngine.ConvertFile(FCurrentFile, '') then
    begin
      FIsConverted := True;

      // Load AST to tree
      LoadASTToTree(FEngine.Document);

      // Load preview
      mmoPreview.Text := FEngine.GetNsisScript;

      // Load issues
      LoadIssuesToList;

      UpdateStatus(Format('Conversion complete: %d/%d blocks converted (%.1f%%)',
        [FEngine.Stats.ConvertedBlocks, FEngine.Stats.TotalBlocks, FEngine.Stats.SuccessRate]));
    end
    else
    begin
      LoadIssuesToList;
      UpdateStatus('Conversion failed. See issues for details.');
    end;

    UpdateUI;
  finally
    Screen.Cursor := crDefault;
    pbProgress.Position := 100;
  end;
end;

procedure TfrmMain.actSaveExecute(Sender: TObject);
begin
  if not FIsConverted then
  begin
    ShowMessage('Please convert a file first.');
    Exit;
  end;

  SaveDialog.FileName := ChangeFileExt(ExtractFileName(FCurrentFile), '.nsi');

  if SaveDialog.Execute then
  begin
    FEngine.SaveToFile(SaveDialog.FileName);
    UpdateStatus('Saved: ' + SaveDialog.FileName);
  end;
end;

procedure TfrmMain.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.About1Click(Sender: TObject);
begin
  ShowMessage('WiseToNSIS Converter' + sLineBreak +
    'Version 1.0.0' + sLineBreak + sLineBreak +
    'Converts Wise Install scripts (.wse) to NSIS scripts (.nsi)' + sLineBreak + sLineBreak +
    'Copyright (c) 2026 ECI Software Solutions');
end;

procedure TfrmMain.tvASTChange(Sender: TObject; Node: TTreeNode);
var
  WseNode: TWseNode;
  Block: TWseBlock;
  Info: TStringList;
  Prop: TWseProperty;
begin
  if (Node = nil) or (Node.Data = nil) then
    Exit;

  WseNode := TWseNode(Node.Data);

  Info := TStringList.Create;
  try
    if WseNode is TWseBlock then
    begin
      Block := TWseBlock(WseNode);

      Info.Add('Block Type: ' + Block.BlockType);
      Info.Add('Line: ' + IntToStr(Block.Line));
      Info.Add('Status: ' + SeverityToStr(TConversionSeverity(Ord(Block.ConversionStatus))));
      Info.Add('Remarked: ' + BoolToStr(Block.IsRemarked, True));
      Info.Add('');
      Info.Add('Properties:');

      for Prop in Block.Properties do
      begin
        if Prop.Language <> '' then
          Info.Add(Format('  %s [%s] = %s', [Prop.Key, Prop.Language, Prop.Value]))
        else
          Info.Add(Format('  %s = %s', [Prop.Key, Prop.Value]));
      end;

      if Block.NsisOutput <> '' then
      begin
        Info.Add('');
        Info.Add('NSIS Output:');
        Info.Add(Block.NsisOutput);
      end;
    end;

    // Could show in a separate panel or tooltip
  finally
    Info.Free;
  end;
end;

procedure TfrmMain.lvIssuesDblClick(Sender: TObject);
var
  Item: TListItem;
  LineNo: Integer;
begin
  Item := lvIssues.Selected;
  if Item = nil then
    Exit;

  // Navigate to line in preview
  LineNo := StrToIntDef(Item.Caption, 0);
  if LineNo > 0 then
  begin
    // Select corresponding tree node
    // (Would need to implement line-to-node mapping)
  end;
end;

procedure TfrmMain.LoadASTToTree(ADocument: TWseDocument);
var
  RootNode: TTreeNode;
begin
  tvAST.Items.BeginUpdate;
  try
    tvAST.Items.Clear;

    RootNode := tvAST.Items.Add(nil, 'Document: ' + ExtractFileName(ADocument.FilePath));
    RootNode.Data := ADocument;

    AddNodeToTree(RootNode, ADocument);

    RootNode.Expand(False);
    if RootNode.Count > 0 then
      RootNode.Item[0].Expand(False);
  finally
    tvAST.Items.EndUpdate;
  end;
end;

procedure TfrmMain.AddNodeToTree(AParentNode: TTreeNode; ANode: TWseNode);
var
  i: Integer;
  TreeNode: TTreeNode;
  Block: TWseBlock;
  NodeText: string;
begin
  for i := 0 to ANode.ChildCount - 1 do
  begin
    if ANode.Children[i] is TWseBlock then
    begin
      Block := TWseBlock(ANode.Children[i]);

      NodeText := Block.BlockType;
      if Block.IsRemarked then
        NodeText := '[REMARKED] ' + NodeText;

      case Block.ConversionStatus of
        csConverted: ; // No marker
        csPartial:   NodeText := NodeText + ' [PARTIAL]';
        csSkipped:   NodeText := NodeText + ' [SKIPPED]';
        csFailed:    NodeText := NodeText + ' [FAILED]';
      end;

      TreeNode := tvAST.Items.AddChild(AParentNode, NodeText);
      TreeNode.Data := Block;

      // Set icon based on status
      case Block.ConversionStatus of
        csConverted: TreeNode.ImageIndex := 0;
        csPartial:   TreeNode.ImageIndex := 1;
        csSkipped:   TreeNode.ImageIndex := 2;
        csFailed:    TreeNode.ImageIndex := 3;
      end;

      AddNodeToTree(TreeNode, Block);
    end;
  end;
end;

procedure TfrmMain.LoadIssuesToList;
var
  Issue: TConversionIssue;
  Item: TListItem;
begin
  lvIssues.Items.BeginUpdate;
  try
    lvIssues.Items.Clear;

    for Issue in FEngine.Issues do
    begin
      Item := lvIssues.Items.Add;
      Item.Caption := IntToStr(Issue.SourceLine);
      Item.SubItems.Add(SeverityToStr(Issue.Severity));
      Item.SubItems.Add(CategoryToStr(Issue.Category));
      Item.SubItems.Add(Issue.Message);
      Item.Data := Issue;
    end;
  finally
    lvIssues.Items.EndUpdate;
  end;
end;

procedure TfrmMain.UpdateUI;
begin
  actConvert.Enabled := FCurrentFile <> '';
  actSave.Enabled := FIsConverted;
  btnConvert.Enabled := actConvert.Enabled;
  btnSave.Enabled := actSave.Enabled;

  if FCurrentFile <> '' then
    Caption := 'WiseToNSIS Converter - ' + ExtractFileName(FCurrentFile)
  else
    Caption := 'WiseToNSIS Converter';
end;

procedure TfrmMain.UpdateStatus(const AMessage: string);
begin
  lblStatus.Caption := AMessage;
end;

procedure TfrmMain.ClearAll;
begin
  tvAST.Items.Clear;
  mmoPreview.Clear;
  lvIssues.Items.Clear;
  FIsConverted := False;
  pbProgress.Position := 0;
end;

end.
