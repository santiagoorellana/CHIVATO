object FormViewPresentDevices: TFormViewPresentDevices
  Left = 239
  Top = 165
  Width = 400
  Height = 284
  BorderStyle = bsSizeToolWin
  Caption = 'Dispositivos actualmente conectados'
  Color = clBtnFace
  Constraints.MinHeight = 243
  Constraints.MinWidth = 400
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
    384
    246)
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 2
    Top = 3
    Width = 379
    Height = 241
    Anchors = [akLeft, akTop, akRight, akBottom]
    Indent = 19
    PopupMenu = PopupMenu1
    TabOrder = 0
  end
  object ActionList1: TActionList
    Images = MainForm.ImageList1
    Left = 56
    Top = 56
    object ActionClose: TAction
      Caption = 'Cerrar'
      Hint = 'Cerrar'
      ImageIndex = 27
      ShortCut = 27
      OnExecute = ActionCloseExecute
    end
    object ActionReset: TAction
      Caption = 'Actualizar'
      Hint = 'Actualizar datos'
      ImageIndex = 36
      ShortCut = 116
      OnExecute = ActionResetExecute
    end
    object ActionSaveAsTXT: TAction
      Caption = 'Guardar como texto'
      Hint = 'Guardar como texto.'
      ImageIndex = 17
      ShortCut = 16455
      OnExecute = ActionSaveAsTXTExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Images = MainForm.ImageList1
    Left = 56
    Top = 16
    object Guardarcomotexto1: TMenuItem
      Action = ActionSaveAsTXT
    end
    object Actualizar1: TMenuItem
      Action = ActionReset
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Cerrar1: TMenuItem
      Action = ActionClose
    end
  end
end
