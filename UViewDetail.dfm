object FormViewDetail: TFormViewDetail
  Left = 196
  Top = 153
  Width = 547
  Height = 458
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 250
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
    0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 30
    Width = 531
    Height = 390
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Dispositivo'
      object Chart4: TChart
        Left = 0
        Top = 0
        Width = 523
        Height = 362
        BackWall.Brush.Color = clWhite
        BackWall.Color = cl3DLight
        Gradient.Direction = gdBottomTop
        Gradient.EndColor = clActiveCaption
        Gradient.StartColor = cl3DLight
        Gradient.Visible = True
        MarginBottom = 1
        MarginLeft = 1
        MarginRight = 5
        MarginTop = 1
        Title.Alignment = taLeftJustify
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlack
        Title.Font.Height = -13
        Title.Font.Name = 'Arial'
        Title.Font.Style = []
        Title.Text.Strings = (
          'TChart')
        BackColor = cl3DLight
        BottomAxis.LabelStyle = talText
        DepthAxis.Automatic = False
        DepthAxis.AutomaticMaximum = False
        DepthAxis.AutomaticMinimum = False
        DepthAxis.Labels = False
        DepthAxis.LabelStyle = talNone
        DepthAxis.Maximum = 0.500000000000000000
        DepthAxis.Minimum = -0.500000000000000000
        DepthAxis.TickOnLabelsOnly = False
        LeftAxis.LabelStyle = talValue
        Legend.Alignment = laBottom
        Legend.Visible = False
        TopAxis.Automatic = False
        TopAxis.AutomaticMaximum = False
        TopAxis.AutomaticMinimum = False
        TopAxis.Labels = False
        View3D = False
        View3DOptions.Elevation = 315
        View3DOptions.Orthogonal = False
        View3DOptions.Perspective = 0
        View3DOptions.Rotation = 360
        Align = alClient
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Ficheros'
      ImageIndex = 1
      object TreeView1: TTreeView
        Left = 0
        Top = 0
        Width = 523
        Height = 362
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Resumen'
      ImageIndex = 2
      object RichEdit1: TRichEdit
        Left = 0
        Top = 0
        Width = 523
        Height = 362
        Align = alClient
        Color = clCream
        Font.Charset = ANSI_CHARSET
        Font.Color = clMaroon
        Font.Height = -16
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 531
    Height = 30
    ButtonHeight = 23
    ButtonWidth = 30
    Caption = 'ToolBar1'
    Images = MainForm.ImageList1
    TabOrder = 1
    object ToolButton2: TToolButton
      Left = 0
      Top = 2
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object DBNavigator1: TDBNavigator
      Left = 8
      Top = 2
      Width = 128
      Height = 23
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
      Align = alTop
      Hints.Strings = (
        'First record'
        'Prior record'
        'Next record'
        'Last record')
      TabOrder = 0
      OnClick = DBNavigator1Click
    end
    object ToolButton3: TToolButton
      Left = 136
      Top = 2
      Action = ActionTraverseDat
    end
    object ToolButton4: TToolButton
      Left = 166
      Top = 2
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ToolButton1: TToolButton
      Left = 174
      Top = 2
      Action = ActionSaveAsText
    end
  end
  object DataSource1: TDataSource
    Left = 24
    Top = 112
  end
  object ActionList1: TActionList
    Images = MainForm.ImageList1
    Left = 24
    Top = 72
    object ActionSaveAsText: TAction
      Caption = 'Guardar'
      Hint = 'Guardar en fichero de texto.'
      ImageIndex = 1
      ShortCut = 16455
      OnExecute = ActionSaveAsTextExecute
      OnUpdate = ActionSaveAsTextUpdate
    end
    object ActionClose: TAction
      Caption = 'Cerrar'
      Hint = 'Cerrar'
      ShortCut = 27
      OnExecute = ActionCloseExecute
    end
    object ActionTraverseDat: TAction
      AutoCheck = True
      Caption = 'Recorrer datos'
      Hint = 'Recorrer los datos'
      ImageIndex = 42
      ShortCut = 16466
      OnExecute = ActionTraverseDatExecute
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Ficheros de texto|*.txt|Todos los ficheros|*.*'
    Left = 64
    Top = 72
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 60
    Top = 110
  end
end
