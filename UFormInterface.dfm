object FormInterface: TFormInterface
  Left = 173
  Top = 124
  Width = 798
  Height = 564
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 500
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
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 782
    Height = 26
    AutoSize = True
    Caption = 'ToolBar1'
    EdgeBorders = [ebTop, ebBottom]
    EdgeOuter = esNone
    Images = MainForm.ImageList1
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object ToolButton2: TToolButton
      Left = 0
      Top = 2
      Action = ActionClose
    end
    object ToolButton23: TToolButton
      Left = 23
      Top = 2
      Width = 8
      Caption = 'ToolButton23'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton5: TToolButton
      Left = 31
      Top = 2
      Action = ActionViewPresentDevices
    end
    object ToolButton6: TToolButton
      Left = 54
      Top = 2
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton15: TToolButton
      Left = 62
      Top = 2
      Action = ActionViewSelect
    end
    object ToolButton16: TToolButton
      Left = 85
      Top = 2
      Action = ActionColorFromCopy
    end
    object ToolButton17: TToolButton
      Left = 108
      Top = 2
      Width = 8
      Caption = 'ToolButton17'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object ToolButton18: TToolButton
      Left = 116
      Top = 2
      Action = ActionSearchText
    end
    object ToolButton19: TToolButton
      Left = 139
      Top = 2
      Action = ActionSearchNexText
    end
    object ToolButton20: TToolButton
      Left = 162
      Top = 2
      Width = 8
      Caption = 'ToolButton20'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton14: TToolButton
      Left = 170
      Top = 2
      Action = ActionViewDetails
    end
    object ToolButton12: TToolButton
      Left = 193
      Top = 2
      Action = ActionShowStatistics
    end
    object ToolButton22: TToolButton
      Left = 216
      Top = 2
      Width = 8
      Caption = 'ToolButton22'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton8: TToolButton
      Left = 224
      Top = 2
      Action = ActionExportToTXT
    end
    object ToolButton9: TToolButton
      Left = 247
      Top = 2
      Action = ActionExportToCSV
    end
    object ToolButton7: TToolButton
      Left = 270
      Top = 2
      Action = ActionExportToINI
    end
    object ToolButton10: TToolButton
      Left = 293
      Top = 2
      Action = ActionExportToHTML
    end
    object ToolButton13: TToolButton
      Left = 316
      Top = 2
      Action = ActionExportToXML
    end
    object ToolButton11: TToolButton
      Left = 339
      Top = 2
      Width = 8
      Caption = 'ToolButton11'
      ImageIndex = 9
      Style = tbsSeparator
    end
    object ToolButton4: TToolButton
      Left = 347
      Top = 2
      Action = ActionConvert
    end
    object ToolButton21: TToolButton
      Left = 370
      Top = 2
      Width = 8
      Caption = 'ToolButton21'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object ToolButton25: TToolButton
      Left = 378
      Top = 2
      Action = ActionUtilisation
    end
    object ToolButton26: TToolButton
      Left = 401
      Top = 2
      Action = ActionOrigin
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 481
    Width = 782
    Height = 25
    Panels = <
      item
        Width = 130
      end
      item
        Width = 130
      end
      item
        Width = 300
      end>
  end
  object ToolBar2: TToolBar
    Left = 0
    Top = 26
    Width = 782
    Height = 92
    AutoSize = True
    ButtonHeight = 42
    Caption = 'ToolBar2'
    EdgeBorders = [ebBottom]
    EdgeOuter = esNone
    TabOrder = 2
    object GroupBox2: TGroupBox
      Left = 0
      Top = 2
      Width = 350
      Height = 42
      Hint = 'Para filtrar por fecha.'
      Caption = 'Filtrar por fecha'
      TabOrder = 0
      DesignSize = (
        350
        42)
      object Label1: TLabel
        Left = 243
        Top = 8
        Width = 10
        Height = 29
        Anchors = [akTop, akRight]
        Caption = '-'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -24
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object DTPFrom: TDateTimePicker
        Left = 152
        Top = 15
        Width = 89
        Height = 21
        Anchors = [akTop, akRight]
        Date = 42594.954277418980000000
        Time = 42594.954277418980000000
        TabOrder = 0
        OnChange = DTPFromChange
      end
      object DTPTo: TDateTimePicker
        Left = 256
        Top = 15
        Width = 89
        Height = 21
        Anchors = [akTop, akRight]
        Date = 42594.954277418980000000
        Time = 42594.954277418980000000
        TabOrder = 1
        OnChange = DTPToChange
      end
      object ComboBoxDateRank: TComboBox
        Left = 24
        Top = 15
        Width = 123
        Height = 21
        Hint = 'Rango de fecha del filtro.'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 2
        OnChange = ComboBoxDateRankChange
        Items.Strings = (
          'Hoy'
          'Esta semana'
          'Este m'#233's'
          'Este a'#241'o'
          'Este rango')
      end
      object CheckBoxFilterDate: TCheckBox
        Left = 8
        Top = 17
        Width = 17
        Height = 17
        Hint = 'Activaci'#243'n'
        TabOrder = 3
        OnClick = CheckBoxFilterDateClick
      end
    end
    object ToolButton3: TToolButton
      Left = 0
      Top = 2
      Width = 8
      Caption = 'ToolButton3'
      Wrap = True
      Style = tbsSeparator
    end
    object GroupBox3: TGroupBox
      Left = 0
      Top = 49
      Width = 221
      Height = 42
      Hint = 'Para filtrar por capacidad en GB.'
      Caption = 'Filtrar por capacidad en GB'
      Constraints.MinWidth = 160
      TabOrder = 2
      DesignSize = (
        221
        42)
      object CheckBoxFilterCapacity: TCheckBox
        Left = 8
        Top = 17
        Width = 17
        Height = 17
        Hint = 'Activaci'#243'n'
        TabOrder = 0
        OnClick = CheckBoxFilterCapacityClick
      end
      object ComboBoxOp1: TComboBox
        Left = 24
        Top = 15
        Width = 121
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ImeName = 'Tipo de comparaci'#243'n'
        ItemHeight = 13
        TabOrder = 1
        OnChange = ComboBoxOp1Change
        Items.Strings = (
          'Mayor que'
          'Menor que'
          'Mayor o igual a'
          'Menor o igual a'
          'Igual a'
          'Diferente de')
      end
      object SpinEditGB1: TSpinEdit
        Left = 148
        Top = 14
        Width = 70
        Height = 22
        Hint = 'Capacidad en Giga Bytes'
        Anchors = [akTop, akRight]
        MaxValue = 1000000000
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = SpinEditGB1Change
      end
    end
    object ToolButton27: TToolButton
      Left = 221
      Top = 49
      Width = 8
      Caption = 'ToolButton27'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object GroupBox1: TGroupBox
      Left = 229
      Top = 49
      Width = 219
      Height = 42
      Hint = 'Para filtrar por cantidad de GB copiados.'
      Caption = 'Filtrar por GB copiados'
      Constraints.MinWidth = 160
      TabOrder = 1
      DesignSize = (
        219
        42)
      object ComboBoxOp2: TComboBox
        Left = 24
        Top = 15
        Width = 118
        Height = 21
        Hint = 'Tipo de comparaci'#243'n'
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
        OnChange = ComboBoxOp2Change
        Items.Strings = (
          'Mayor que'
          'Menor que'
          'Mayor o igual a'
          'Menor o igual a'
          'Igual a'
          'Diferente de')
      end
      object SpinEditGB2: TSpinEdit
        Left = 145
        Top = 14
        Width = 70
        Height = 22
        Hint = 'Cantidad en Giga Bytes'
        Anchors = [akTop, akRight]
        MaxValue = 1000000000
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = SpinEditGB2Change
      end
      object CheckBoxFilterCopy: TCheckBox
        Left = 8
        Top = 17
        Width = 17
        Height = 17
        Hint = 'Activaci'#243'n'
        TabOrder = 2
        OnClick = CheckBoxFilterCopyClick
      end
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 118
    Width = 782
    Height = 363
    Hint = 'Conecciones de dispositivos.'
    Align = alClient
    DataSource = DataSource1
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
    ParentShowHint = False
    ReadOnly = True
    ShowHint = False
    TabOrder = 3
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnCellClick = DBGrid1CellClick
    OnDrawColumnCell = DBGrid1DrawColumnCell
    OnDblClick = DBGrid1DblClick
  end
  object DataSource1: TDataSource
    OnDataChange = DataSource1DataChange
    Left = 80
    Top = 200
  end
  object MainMenu1: TMainMenu
    Images = MainForm.ImageList1
    Left = 124
    Top = 200
    object Archivo1: TMenuItem
      Caption = 'Administraci'#243'n'
      object Cambiarpassword1: TMenuItem
        Action = ActionChangePassword
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Dispositivosactualmenteconectados1: TMenuItem
        Action = ActionViewPresentDevices
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Finalizarmonitoreo1: TMenuItem
        Action = ActionFinally
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Cerrar1: TMenuItem
        Action = ActionClose
      end
    end
    object View1: TMenuItem
      Caption = 'Visualizaci'#243'n'
      object ActionFilterDate1: TMenuItem
        Action = ActionFilterDate
      end
      object ActionFilterCapacity1: TMenuItem
        Action = ActionFilterCapacity
      end
      object ActionFilterCopyed1: TMenuItem
        Action = ActionFilterCopyed
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Seleccindesimilitud1: TMenuItem
        Action = ActionViewSelect
      end
      object ColorporGBcopiados1: TMenuItem
        Action = ActionColorFromCopy
      end
    end
    object Buscar1: TMenuItem
      Caption = 'B'#250'squeda'
      object Buscartexto1: TMenuItem
        Action = ActionSearchText
      end
      object Buscarsiguientecoincidencia1: TMenuItem
        Action = ActionSearchNexText
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Iralinicio1: TMenuItem
        Action = ActionBegin
      end
      object Iralfinal1: TMenuItem
        Action = ActionEnd
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Recorrerdatos1: TMenuItem
        Action = ActionTraverse
      end
    end
    object Anlisis1: TMenuItem
      Caption = 'An'#225'lisis'
      object Verdetallesdeundispositivo1: TMenuItem
        Action = ActionViewDetails
      end
      object Verlasestadsticas1: TMenuItem
        Action = ActionShowStatistics
      end
    end
    object Exportar1: TMenuItem
      Caption = 'Exportaci'#243'n'
      object GuardartablacomoTXT1: TMenuItem
        Action = ActionExportToTXT
      end
      object GuardartablacomoCSV1: TMenuItem
        Action = ActionExportToCSV
      end
      object ExportarcomoINI1: TMenuItem
        Action = ActionExportToINI
      end
      object GuardartablacomoHTML1: TMenuItem
        Action = ActionExportToHTML
      end
      object GuardartablacomoXML1: TMenuItem
        Action = ActionExportToXML
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Guardarautomticamente1: TMenuItem
        Action = ActionAutoExport
      end
    end
    object Mantenimiento1: TMenuItem
      Caption = 'Herramientas'
      object Vaciarlabasededatos1: TMenuItem
        Action = ActionClearDB
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Convertidor1: TMenuItem
        Action = ActionConvert
      end
    end
    object Ayuda1: TMenuItem
      Caption = 'Ayuda'
      object Origen1: TMenuItem
        Action = ActionUtilisation
      end
      object Procedencia1: TMenuItem
        Action = ActionOrigin
      end
    end
  end
  object ActionList1: TActionList
    Images = MainForm.ImageList1
    Left = 164
    Top = 201
    object ActionChangePassword: TAction
      Category = 'Administracion'
      Caption = 'Cambiar password'
      Hint = 'Cambiar password de acceso.'
      ImageIndex = 5
      ShortCut = 24656
      OnExecute = ActionChangePasswordExecute
      OnUpdate = ActionUpdate
    end
    object ActionExportToTXT: TAction
      Category = 'Exportar'
      Caption = 'Guardar tabla como TXT'
      Hint = 'Guardar tabla como TXT.'
      ImageIndex = 7
      ShortCut = 32852
      OnExecute = ActionExportToTXTExecute
      OnUpdate = ActionUpdate
    end
    object ActionExportToCSV: TAction
      Category = 'Exportar'
      Caption = 'Guardar tabla como CSV'
      Hint = 'Guardar tabla como CSV.'
      ImageIndex = 6
      ShortCut = 32835
      OnExecute = ActionExportToCSVExecute
      OnUpdate = ActionUpdate
    end
    object ActionExportToHTML: TAction
      Category = 'Exportar'
      Caption = 'Guardar tabla como HTML'
      Hint = 'Guardar tabla como HTML.'
      ImageIndex = 8
      ShortCut = 32840
      OnExecute = ActionExportToHTMLExecute
      OnUpdate = ActionUpdate
    end
    object ActionOrigin: TAction
      Category = 'Ayuda'
      Caption = 'Origen'
      Hint = 'Origen de la aplicaci'#243'n.'
      ImageIndex = 3
      ShortCut = 113
      OnExecute = ActionOriginExecute
      OnUpdate = ActionUpdate
    end
    object ActionUtilisation: TAction
      Category = 'Ayuda'
      Caption = 'Utilizaci'#243'n'
      Hint = 'Utilizaci'#243'n de la aplicaci'#243'n.'
      ImageIndex = 2
      ShortCut = 112
      OnExecute = ActionUtilisationExecute
      OnUpdate = ActionUpdate
    end
    object ActionClose: TAction
      Category = 'Administracion'
      Caption = 'Cerrar ventana'
      Hint = 'Cerrar ventana.'
      ImageIndex = 4
      ShortCut = 16411
      OnExecute = ActionCloseExecute
      OnUpdate = ActionUpdate
    end
    object ActionShowStatistics: TAction
      Category = 'Analisis'
      Caption = 'Ver las estad'#237'sticas'
      Hint = 'Ver las estad'#237'sticas del resultado'
      ImageIndex = 9
      ShortCut = 16453
      OnExecute = ActionShowStatisticsExecute
      OnUpdate = ActionUpdate
    end
    object ActionClearDB: TAction
      Category = 'Herramientas'
      Caption = 'Vaciar la base de datos'
      Hint = 'Vaciar la base de datos.'
      ImageIndex = 15
      OnExecute = ActionClearDBExecute
      OnUpdate = ActionUpdate
    end
    object ActionFilterDate: TAction
      Category = 'Ver'
      Caption = 'Filtrar por fecha'
      Hint = 'Filtrar por fecha'
      ShortCut = 16500
      OnExecute = ActionFilterDateExecute
      OnUpdate = ActionFilterDateUpdate
    end
    object ActionFilterCapacity: TAction
      Category = 'Ver'
      Caption = 'Filtrar por capacidad'
      Hint = 'Filtrar por capacidad.'
      ShortCut = 16501
      OnExecute = ActionFilterCapacityExecute
      OnUpdate = ActionFilterCapacityUpdate
    end
    object ActionFilterCopyed: TAction
      Category = 'Ver'
      Caption = 'Filtrar por Gb copiados'
      Hint = 'Filtrar por Gb copiados.'
      ShortCut = 16502
      OnExecute = ActionFilterCopyedExecute
      OnUpdate = ActionFilterCopyedUpdate
    end
    object ActionViewDetails: TAction
      Category = 'Analisis'
      Caption = 'Ver detalles de un dispositivo'
      Hint = 'Ver detalles de un dispositivo'
      ImageIndex = 12
      ShortCut = 16452
      OnExecute = ActionViewDetailsExecute
      OnUpdate = ActionUpdate
    end
    object ActionFinally: TAction
      Category = 'Monitoreo'
      Caption = 'Finalizar monitoreo'
      Hint = 'Finalizar monitoreo hasta pr'#243'ximo reinicio.'
      ImageIndex = 23
      ShortCut = 24611
      OnExecute = ActionFinallyExecute
      OnUpdate = ActionUpdate
    end
    object ActionViewSelect: TAction
      Category = 'Ver'
      Caption = 'Selecci'#243'n de similitud'
      Hint = 'Ver la selecci'#243'n de similitud.'
      ImageIndex = 22
      OnExecute = ActionViewSelectExecute
      OnUpdate = ActionViewSelectUpdate
    end
    object ActionColorFromCopy: TAction
      Category = 'Ver'
      Caption = 'Color por GB copiados'
      ImageIndex = 21
      OnExecute = ActionColorFromCopyExecute
      OnUpdate = ActionColorFromCopyUpdate
    end
    object ActionSearchText: TAction
      Category = 'Busqueda'
      Caption = 'Buscar texto'
      Hint = 'Buscar texto.'
      ImageIndex = 26
      ShortCut = 16450
      OnExecute = ActionSearchTextExecute
      OnUpdate = ActionUpdate
    end
    object ActionSearchNexText: TAction
      Category = 'Busqueda'
      Caption = 'Buscar siguiente coincidencia'
      Hint = 'Buscar siguiente coincidencia del texto.'
      ImageIndex = 25
      ShortCut = 114
      OnExecute = ActionSearchNexTextExecute
      OnUpdate = ActionUpdate
    end
    object ActionViewLog: TAction
      Category = 'Herramientas'
      Caption = 'Ver mensajes del log'
      Hint = 'Ver mensajes del log.'
      ImageIndex = 31
      OnUpdate = ActionUpdate
    end
    object ActionClearLog: TAction
      Category = 'Herramientas'
      Caption = 'Borrar mensajes del log'
      Hint = 'Borrar mensajes del log.'
      ImageIndex = 32
      OnUpdate = ActionUpdate
    end
    object ActionAutoExport: TAction
      Category = 'Exportar'
      Caption = 'Exportaci'#243'n autom'#225'tica'
      Hint = 'Exportar autom'#225'ticamente hacia un dispositivo.'
      ImageIndex = 33
      ShortCut = 32837
      OnUpdate = ActionUpdate
    end
    object ActionExportToXML: TAction
      Category = 'Exportar'
      Caption = 'Guardar tabla como XML'
      Hint = 'Guardar tabla como XML.'
      ImageIndex = 39
      ShortCut = 32856
      OnExecute = ActionExportToXMLExecute
      OnUpdate = ActionUpdate
    end
    object ActionViewPresentDevices: TAction
      Category = 'Monitoreo'
      Caption = 'Ver dispositivos actualmente conectados'
      Hint = 'Ver dispositivos actualmente conectados.'
      ImageIndex = 37
      ShortCut = 24662
      OnExecute = ActionViewPresentDevicesExecute
      OnUpdate = ActionUpdate
    end
    object ActionConvert: TAction
      Category = 'Herramientas'
      Caption = 'Convertidor'
      Hint = 'Convertidor'
      ImageIndex = 38
      OnExecute = ActionConvertExecute
      OnUpdate = ActionUpdate
    end
    object ActionBegin: TAction
      Category = 'Busqueda'
      Caption = 'Ir al inicio'
      Hint = 'Ir al inicio de los datos.'
      ImageIndex = 41
      ShortCut = 16420
      OnExecute = ActionBeginExecute
      OnUpdate = ActionUpdate
    end
    object ActionEnd: TAction
      Category = 'Busqueda'
      Caption = 'Ir al final'
      Hint = 'Ir al final de los datos.'
      ImageIndex = 40
      ShortCut = 16419
      OnExecute = ActionEndExecute
      OnUpdate = ActionUpdate
    end
    object ActionTraverse: TAction
      Category = 'Busqueda'
      Caption = 'Recorrer datos'
      Hint = 'Recorrer los datos uno a uno.'
      ImageIndex = 42
      ShortCut = 16466
      OnExecute = ActionTraverseExecute
      OnUpdate = ActionUpdate
    end
    object ActionExportToINI: TAction
      Category = 'Exportar'
      Caption = 'Guardar tabla como INI'
      Hint = 'Guardar tabla como INI'
      ImageIndex = 43
      ShortCut = 32841
      OnExecute = ActionExportToINIExecute
      OnUpdate = ActionUpdate
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'Fichero XML|*.xml|Fichero Binario|*.bin'
    FilterIndex = 0
    Left = 132
    Top = 240
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'Fichero XML|*.xml|Fichero Binario|*.bin'
    FilterIndex = 0
    Left = 180
    Top = 240
  end
  object SaveDialog2: TSaveDialog
    FilterIndex = 0
    Left = 132
    Top = 280
  end
end
