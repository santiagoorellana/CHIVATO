object FormViewStatistics: TFormViewStatistics
  Left = 330
  Top = 154
  Width = 654
  Height = 452
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 638
    Height = 414
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    OnResize = PageControl1Resize
    object TabSheet2: TTabSheet
      Caption = 'Horarios'
      ImageIndex = 1
      object Chart1: TChart
        Left = 0
        Top = 192
        Width = 630
        Height = 194
        BackWall.Brush.Color = clWhite
        BackWall.Color = cl3DLight
        Gradient.Direction = gdBottomTop
        Gradient.EndColor = clActiveCaption
        Gradient.StartColor = cl3DLight
        Gradient.Visible = True
        MarginBottom = 1
        MarginLeft = 1
        MarginRight = 1
        MarginTop = 2
        Title.Alignment = taLeftJustify
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clBlack
        Title.Font.Height = -13
        Title.Font.Name = 'Arial'
        Title.Font.Style = []
        Title.Text.Strings = (
          'TChart')
        BackColor = cl3DLight
        DepthAxis.Inverted = True
        LeftAxis.LabelStyle = talValue
        TopAxis.Labels = False
        View3D = False
        Align = alBottom
        TabOrder = 0
      end
      object Chart2: TChart
        Left = 257
        Top = 0
        Width = 373
        Height = 192
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
        LeftAxis.LabelStyle = talValue
        Legend.Visible = False
        TopAxis.Labels = False
        View3D = False
        Align = alClient
        TabOrder = 1
      end
      object Chart3: TChart
        Left = 0
        Top = 0
        Width = 257
        Height = 192
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
        Legend.Visible = False
        TopAxis.Automatic = False
        TopAxis.AutomaticMaximum = False
        TopAxis.AutomaticMinimum = False
        TopAxis.Labels = False
        View3D = False
        Align = alLeft
        TabOrder = 2
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Dispositivos'
      ImageIndex = 3
      object Chart6: TChart
        Left = 0
        Top = 0
        Width = 630
        Height = 386
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
        Legend.Visible = False
        TopAxis.Automatic = False
        TopAxis.AutomaticMaximum = False
        TopAxis.AutomaticMinimum = False
        TopAxis.Labels = False
        View3D = False
        Align = alClient
        TabOrder = 0
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Resumen'
      object RichEdit1: TRichEdit
        Left = 0
        Top = 30
        Width = 630
        Height = 356
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clMaroon
        Font.Height = -16
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object ToolBar1: TToolBar
        Left = 0
        Top = 0
        Width = 630
        Height = 30
        AutoSize = True
        BorderWidth = 1
        Caption = 'ToolBar1'
        Images = MainForm.ImageList1
        TabOrder = 1
        object ToolButton2: TToolButton
          Left = 0
          Top = 2
          Action = ActionSaveAsTXT
        end
        object ToolButton4: TToolButton
          Left = 23
          Top = 2
          Width = 8
          Caption = 'ToolButton4'
          ImageIndex = 19
          Style = tbsSeparator
        end
        object ToolButton1: TToolButton
          Left = 31
          Top = 2
          Action = ActionSaveAsRTF
        end
        object ToolButton5: TToolButton
          Left = 54
          Top = 2
          Width = 8
          Caption = 'ToolButton5'
          ImageIndex = 19
          Style = tbsSeparator
        end
        object ToolButton8: TToolButton
          Left = 62
          Top = 2
          Action = ActionSelectAll
        end
        object ToolButton9: TToolButton
          Left = 85
          Top = 2
          Width = 8
          Caption = 'ToolButton9'
          ImageIndex = 20
          Style = tbsSeparator
        end
        object ToolButton3: TToolButton
          Left = 93
          Top = 2
          Action = ActionCopy
        end
        object ToolButton7: TToolButton
          Left = 116
          Top = 2
          Width = 8
          Caption = 'ToolButton7'
          ImageIndex = 20
          Style = tbsSeparator
        end
        object ToolButton6: TToolButton
          Left = 124
          Top = 2
          Action = ActionPrint
        end
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Ficheros de texto|*.txt|Todos los ficheros|*.*'
    Left = 72
    Top = 88
  end
  object ActionList1: TActionList
    Images = MainForm.ImageList1
    Left = 32
    Top = 88
    object ActionClose: TAction
      Caption = 'Cerrar'
      Hint = 'Cerrar'
      ImageIndex = 4
      ShortCut = 27
      OnExecute = ActionCloseExecute
    end
    object ActionSaveAsTXT: TAction
      Caption = 'Guardar como TXT'
      Hint = 'Guardar como TXT.'
      ImageIndex = 17
      ShortCut = 16467
      OnExecute = ActionSaveAsTXTExecute
    end
    object ActionSaveAsRTF: TAction
      Caption = 'Guardar como RTF'
      Hint = 'Guardar como RTF.'
      ImageIndex = 16
      ShortCut = 16454
      OnExecute = ActionSaveAsRTFExecute
    end
    object ActionCopy: TAction
      Caption = 'Copiar selecci'#243'n'
      Hint = 'Copiar texto seleccionado.'
      ImageIndex = 18
      ShortCut = 16451
      OnExecute = ActionCopyExecute
    end
    object ActionPrint: TAction
      Caption = 'Imprimir'
      Hint = 'Imprimir texto.'
      ImageIndex = 19
      ShortCut = 16464
      OnExecute = ActionPrintExecute
    end
    object ActionSelectAll: TAction
      Caption = 'Seleccionar todo'
      Hint = 'Seleccionar todo.'
      ImageIndex = 20
      ShortCut = 16449
      OnExecute = ActionSelectAllExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Images = MainForm.ImageList1
    Left = 104
    Top = 88
  end
end
