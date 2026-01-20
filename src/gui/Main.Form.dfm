object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'WiseToNSIS Converter'
  ClientHeight = 600
  ClientWidth = 1000
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object splLeft: TSplitter
    Left = 250
    Top = 41
    Height = 510
    ExplicitTop = 49
  end
  object splRight: TSplitter
    Left = 653
    Top = 41
    Height = 510
    Align = alRight
    ExplicitLeft = 700
    ExplicitTop = 49
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1000
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnOpen: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Action = actOpen
      TabOrder = 0
    end
    object btnConvert: TButton
      Left = 89
      Top = 8
      Width = 75
      Height = 25
      Action = actConvert
      TabOrder = 1
    end
    object btnSave: TButton
      Left = 170
      Top = 8
      Width = 75
      Height = 25
      Action = actSave
      TabOrder = 2
    end
    object pbProgress: TProgressBar
      Left = 260
      Top = 12
      Width = 200
      Height = 17
      TabOrder = 3
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 41
    Width = 1000
    Height = 510
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 250
      Height = 510
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object lblASTTitle: TLabel
        Left = 8
        Top = 4
        Width = 88
        Height = 13
        Caption = 'Source AST Tree:'
      end
      object tvAST: TTreeView
        Left = 0
        Top = 20
        Width = 250
        Height = 490
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Indent = 19
        TabOrder = 0
        OnChange = tvASTChange
      end
    end
    object pnlCenter: TPanel
      Left = 253
      Top = 0
      Width = 400
      Height = 510
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object lblPreviewTitle: TLabel
        Left = 8
        Top = 4
        Width = 74
        Height = 13
        Caption = 'NSIS Preview:'
      end
      object mmoPreview: TMemo
        Left = 0
        Top = 20
        Width = 400
        Height = 490
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object pnlRight: TPanel
      Left = 656
      Top = 0
      Width = 344
      Height = 510
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object lblIssuesTitle: TLabel
        Left = 8
        Top = 4
        Width = 36
        Height = 13
        Caption = 'Issues:'
      end
      object lvIssues: TListView
        Left = 0
        Top = 20
        Width = 344
        Height = 490
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <>
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvIssuesDblClick
      end
    end
  end
  object pnlStatus: TPanel
    Left = 0
    Top = 551
    Width = 1000
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object lblStatus: TLabel
      Left = 8
      Top = 8
      Width = 39
      Height = 13
      Caption = 'Ready.'
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'wse'
    Filter = 'Wise Install Scripts (*.wse)|*.wse|All Files (*.*)|*.*'
    Title = 'Open Wise Install Script'
    Left = 472
    Top = 8
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'nsi'
    Filter = 'NSIS Scripts (*.nsi)|*.nsi|All Files (*.*)|*.*'
    Title = 'Save NSIS Script'
    Left = 520
    Top = 8
  end
  object ActionList: TActionList
    Left = 568
    Top = 8
    object actOpen: TAction
      Caption = '&Open...'
      ShortCut = 16463
      OnExecute = actOpenExecute
    end
    object actConvert: TAction
      Caption = '&Convert'
      Enabled = False
      ShortCut = 116
      OnExecute = actConvertExecute
    end
    object actSave: TAction
      Caption = '&Save...'
      Enabled = False
      ShortCut = 16467
      OnExecute = actSaveExecute
    end
  end
  object MainMenu: TMainMenu
    Left = 616
    Top = 8
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Action = actOpen
      end
      object Save1: TMenuItem
        Action = actSave
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Caption = '&About...'
        OnClick = About1Click
      end
    end
  end
end
