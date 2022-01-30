object FormExcludeDevice: TFormExcludeDevice
  Left = 540
  Top = 129
  Width = 300
  Height = 243
  BorderStyle = bsSizeToolWin
  Caption = 'Dispositivos excluidos'
  Color = clBtnFace
  Constraints.MinHeight = 243
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    284
    205)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 139
    Top = 3
    Width = 138
    Height = 194
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'N'#250'meros de serie '
    TabOrder = 0
    DesignSize = (
      138
      194)
    object ListBox1: TListBox
      Left = 8
      Top = 16
      Width = 122
      Height = 170
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 20
      ParentFont = False
      PopupMenu = PopupMenu1
      TabOrder = 0
    end
  end
  object BitBtn2: TBitBtn
    Left = 6
    Top = 7
    Width = 126
    Height = 33
    Action = ActionAddToList
    Caption = 'Agregar a la lista'
    TabOrder = 1
    Glyph.Data = {
      36040000424D3604000000000000360000002800000010000000100000000100
      2000000000000004000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000000000000000000000000000000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000000FF000000FF000000FF0000000000000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000000FF000000FF000000FF000000FF00000000
      000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000000FF000000FF000000FF000000FF000000FF
      00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000000FF000000FF000000FF000000FF000000FF
      000000FF00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF00000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000000FF000000FF000000FF000000FF000000FF
      000000FF000000FF00000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000000FF000000FF000000FF000000FF000000FF
      000000FF00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000000FF000000FF000000FF000000FF000000FF
      00000000000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000000FF000000FF000000FF000000FF00000000
      000000000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000000FF000000FF000000FF0000000000000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00000000000000000000000000000000000000000000000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
    Margin = 8
  end
  object BitBtn3: TBitBtn
    Left = 6
    Top = 84
    Width = 126
    Height = 33
    Action = ActionClearList
    Caption = 'Vaciar lista'
    TabOrder = 2
    Glyph.Data = {
      36040000424D3604000000000000360000002800000010000000100000000100
      2000000000000004000000000000000000000000000000000000FF00FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF000000FF000000
      FF00FFFFFF00FFFFFF0080808000FFFFFF0080808000FFFFFF0080808000FFFF
      FF0080808000FFFFFF0080808000FFFFFF00000000000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000FF000000FF00FF00FF000000
      FF000000FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000FF000000FF000000FF000000FF00FF00FF000000
      0000FFFFFF000000FF000000FF000000FF0080808000FFFFFF0080808000FFFF
      FF00808080000000FF000000FF000000FF000000FF00FF00FF00FF00FF000000
      0000FFFFFF00000000000000FF000000FF000000FF000000FF00000000000000
      00000000FF000000FF000000FF00FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF0080808000FFFFFF00808080000000FF000000FF000000
      FF000000FF00FFFFFF0080808000FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF000000FF000000FF000000FF00FFFFFF000000
      FF000000FF000000FF000000FF00FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF000000FF000000FF000000FF00FFFFFF0080808000FFFF
      FF00808080000000FF000000FF000000FF0000000000FF00FF00FF00FF000000
      00000000FF000000FF000000FF000000FF000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00FF00FF000000
      FF000000FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
    Margin = 8
  end
  object BitBtn4: TBitBtn
    Left = 6
    Top = 124
    Width = 126
    Height = 33
    Action = ActionSaveList
    Caption = 'Guardar lista'
    TabOrder = 3
    Glyph.Data = {
      36040000424D3604000000000000360000002800000010000000100000000100
      2000000000000004000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF000000
      0000008080000080800000000000000000000000000000000000000000000000
      0000C0C0C000C0C0C000000000000080800000000000FF00FF00FF00FF000000
      0000008080000080800000000000000000000000000000000000000000000000
      0000C0C0C000C0C0C000000000000080800000000000FF00FF00FF00FF000000
      0000008080000080800000000000000000000000000000000000000000000000
      0000C0C0C000C0C0C000000000000080800000000000FF00FF00FF00FF000000
      0000008080000080800000000000000000000000000000000000000000000000
      00000000000000000000000000000080800000000000FF00FF00FF00FF000000
      0000008080000080800000808000008080000080800000808000008080000080
      80000080800000808000008080000080800000000000FF00FF00FF00FF000000
      0000008080000080800000000000000000000000000000000000000000000000
      00000000000000000000008080000080800000000000FF00FF00FF00FF000000
      00000080800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000000000000080800000000000FF00FF00FF00FF000000
      00000080800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000000000000080800000000000FF00FF00FF00FF000000
      00000080800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000000000000080800000000000FF00FF00FF00FF000000
      00000080800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000000000000080800000000000FF00FF00FF00FF000000
      00000080800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000000000000000000000000000FF00FF00FF00FF000000
      00000080800000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00000000000C0C0C00000000000FF00FF00FF00FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
    Margin = 8
  end
  object BitBtn5: TBitBtn
    Left = 6
    Top = 45
    Width = 126
    Height = 33
    Action = ActionDelSelect
    Caption = 'Eliminar selecci'#243'n'
    TabOrder = 4
    Glyph.Data = {
      36040000424D3604000000000000360000002800000010000000100000000100
      2000000000000004000000000000000000000000000000000000FF00FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF0080808000FFFFFF0080808000FFFFFF0080808000FFFF
      FF0080808000FFFFFF0080808000FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF0080808000FFFFFF0080808000FFFFFF0080808000FFFF
      FF0080808000FFFFFF0080808000FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF0080808000FFFFFF0080808000FFFFFF0080808000FFFF
      FF0080808000FFFFFF0080808000FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF0000000000FF00FF00FF00FF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FF00FF00FF00FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
    Margin = 8
  end
  object BitBtn1: TBitBtn
    Left = 6
    Top = 163
    Width = 126
    Height = 33
    Action = ActionClose
    Caption = 'Cerrar'
    TabOrder = 5
    Glyph.Data = {
      36040000424D3604000000000000360000002800000010000000100000000100
      2000000000000004000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF00FF00FF00FF000000
      0000C0C0C0000000800000008000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000800000008000C0C0C00000000000FF00FF00FF00FF000000
      0000C0C0C000000080000000FF0000008000C0C0C000C0C0C000C0C0C000C0C0
      C000000080000000FF0000008000C0C0C00000000000FF00FF00FF00FF000000
      0000C0C0C000C0C0C000000080000000FF0000008000C0C0C000C0C0C0000000
      80000000FF0000008000C0C0C000C0C0C00000000000FF00FF00FF00FF000000
      0000C0C0C000C0C0C000C0C0C000000080000000FF0000008000000080000000
      FF0000008000C0C0C000C0C0C000C0C0C00000000000FF00FF00FF00FF000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000000080000000FF000000FF000000
      8000C0C0C000C0C0C000C0C0C000C0C0C00000000000FF00FF00FF00FF000000
      0000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C0000000FF000000FF000000
      8000C0C0C000C0C0C000C0C0C000C0C0C00000000000FF00FF00FF00FF000000
      0000C0C0C000C0C0C000C0C0C000C0C0C0000000FF0000008000C0C0C0000000
      FF0000008000C0C0C000C0C0C000C0C0C00000000000FF00FF00FF00FF000000
      0000C0C0C000C0C0C000C0C0C0000000FF0000008000C0C0C000C0C0C000C0C0
      C0000000FF0000008000C0C0C000C0C0C00000000000FF00FF00FF00FF000000
      0000C0C0C000C0C0C0000000FF0000008000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C0000000FF0000008000C0C0C00000000000FF00FF00FF00FF000000
      0000C0C0C00000008000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C00000008000C0C0C00000000000FF00FF00FF00FF008000
      0000800000008000000080000000800000008000000080000000800000008000
      00008000000080000000800000008000000080000000FF00FF00FF00FF008000
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000FF00000080000000FF00FF00FF00FF008000
      0000800000008000000080000000800000008000000080000000800000008000
      00008000000080000000800000008000000080000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
    Margin = 8
  end
  object ActionList1: TActionList
    Images = MainForm.ImageList1
    Left = 184
    Top = 32
    object ActionAddToList: TAction
      Category = 'Botones'
      Caption = 'Agregar a la lista'
      Hint = 'Agregar a la lista.'
      ImageIndex = 30
      ShortCut = 16449
      OnExecute = ActionAddToListExecute
    end
    object ActionClearList: TAction
      Category = 'Botones'
      Caption = 'Vaciar lista'
      Hint = 'Vaciar lista'
      ImageIndex = 29
      ShortCut = 16450
      OnExecute = ActionClearListExecute
    end
    object ActionSaveList: TAction
      Category = 'Botones'
      Caption = 'Guardar lista'
      Hint = 'Guardar lista'
      ImageIndex = 1
      ShortCut = 16455
      OnExecute = ActionSaveListExecute
    end
    object ActionDelSelect: TAction
      Category = 'Botones'
      Caption = 'Eliminar selecci'#243'n'
      ImageIndex = 28
      ShortCut = 16453
      OnExecute = ActionDelSelectExecute
    end
    object ActionClose: TAction
      Category = 'Botones'
      Caption = 'Cerrar'
      Hint = 'Cerrar'
      ImageIndex = 4
      ShortCut = 27
      OnExecute = ActionCloseExecute
    end
    object ActionLoadFile: TAction
      Category = 'Menu'
      Caption = 'Cargar desde un fichero de texto'
      Hint = 'Cargar desde un fichero de texto.'
      ImageIndex = 13
      ShortCut = 16460
      OnExecute = ActionLoadFileExecute
    end
    object ActionSaveFile: TAction
      Category = 'Menu'
      Caption = 'Guardar en un fichero de texto'
      Hint = 'Guardar en un fichero de texto'
      ImageIndex = 17
      ShortCut = 16467
      OnExecute = ActionSaveFileExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Images = MainForm.ImageList1
    Left = 187
    Top = 75
    object Guardarficherodetexto1: TMenuItem
      Action = ActionSaveFile
    end
    object Cargarficherodetexto1: TMenuItem
      Action = ActionLoadFile
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Agregaralalista1: TMenuItem
      Action = ActionAddToList
    end
    object Eliminarseleccin1: TMenuItem
      Action = ActionDelSelect
    end
    object Vaciarlista1: TMenuItem
      Action = ActionClearList
    end
    object Guardarlista1: TMenuItem
      Action = ActionSaveList
    end
  end
end