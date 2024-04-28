object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Demo how to use UnitExifTool'
  ClientHeight = 556
  ClientWidth = 783
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 523
    Top = 41
    Height = 515
    Align = alRight
    ExplicitLeft = 0
    ExplicitTop = 223
    ExplicitHeight = 100
  end
  object PnlExif: TPanel
    Left = 526
    Top = 41
    Width = 257
    Height = 515
    Align = alRight
    TabOrder = 0
    ExplicitLeft = 522
    ExplicitHeight = 514
    object ExifLog: TMemo
      Left = 1
      Top = 425
      Width = 255
      Height = 89
      Align = alBottom
      Lines.Strings = (
        'ExifLog')
      TabOrder = 0
      ExplicitTop = 424
    end
    object LvExif: TListView
      Left = 1
      Top = 1
      Width = 255
      Height = 424
      Align = alClient
      Columns = <
        item
          AutoSize = True
          Caption = 'Item'
        end
        item
          AutoSize = True
          Caption = 'Value'
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      ParentShowHint = False
      ShowWorkAreas = True
      ShowHint = True
      SortType = stText
      TabOrder = 1
      ViewStyle = vsReport
      ExplicitHeight = 423
    end
  end
  object PnlFunctions: TPanel
    Left = 0
    Top = 0
    Width = 783
    Height = 41
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 779
    object BtnLensTags: TButton
      Left = 14
      Top = 10
      Width = 75
      Height = 25
      Caption = 'BtnLensTags'
      TabOrder = 0
      OnClick = BtnLensTagsClick
    end
    object BtnGpsTags: TButton
      Left = 95
      Top = 11
      Width = 75
      Height = 25
      Caption = 'BtnGpsTags'
      TabOrder = 1
      OnClick = BtnGpsTagsClick
    end
    object BtnExifDateForward: TButton
      Left = 176
      Top = 11
      Width = 129
      Height = 25
      Caption = 'BtnExifDateForward'
      TabOrder = 2
      OnClick = BtnExifDateForwardClick
    end
    object BtnExifDateBackward: TButton
      Left = 322
      Top = 11
      Width = 129
      Height = 25
      Caption = 'BtnExifDateBackward'
      TabOrder = 3
      OnClick = BtnExifDateBackwardClick
    end
    object BtnSetFileDatesFromExif: TButton
      Left = 470
      Top = 10
      Width = 155
      Height = 25
      Caption = 'BtnSetFileDatesFromExif'
      TabOrder = 4
      OnClick = BtnSetFileDatesFromExifClick
    end
  end
  object PnlFiles: TPanel
    Left = 0
    Top = 41
    Width = 523
    Height = 515
    Align = alClient
    Caption = 'PnlFiles'
    TabOrder = 2
    ExplicitWidth = 519
    ExplicitHeight = 514
    object Image1: TImage
      Left = 1
      Top = 289
      Width = 521
      Height = 225
      Align = alClient
      Proportional = True
      Stretch = True
      ExplicitLeft = 41
      ExplicitTop = 330
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
    object FileListBox1: TFileListBox
      Left = 1
      Top = 159
      Width = 521
      Height = 130
      Align = alTop
      Mask = '*.jpg'
      ShowGlyphs = True
      TabOrder = 0
      OnClick = FileListBox1Click
      ExplicitWidth = 517
    end
    object DriveComboBox1: TDriveComboBox
      Left = 1
      Top = 1
      Width = 521
      Height = 21
      Align = alTop
      DirList = DirectoryListBox1
      TabOrder = 1
      ExplicitWidth = 517
    end
    object DirectoryListBox1: TDirectoryListBox
      Left = 1
      Top = 22
      Width = 521
      Height = 137
      Align = alTop
      FileList = FileListBox1
      TabOrder = 2
      ExplicitWidth = 517
    end
  end
end
