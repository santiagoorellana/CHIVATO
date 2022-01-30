object FormOrigin: TFormOrigin
  Left = 217
  Top = 145
  BorderStyle = bsToolWindow
  Caption = 'CHIVATO (Origen)'
  ClientHeight = 169
  ClientWidth = 417
  Color = clBtnFace
  Constraints.MinHeight = 207
  Constraints.MinWidth = 425
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
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    417
    169)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelTitle: TLabel
    Left = 8
    Top = 8
    Width = 387
    Height = 73
    Caption = '0000000 000'
    Font.Charset = ANSI_CHARSET
    Font.Color = clNavy
    Font.Height = -64
    Font.Name = 'Bauhaus 93'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelPhone: TLabel
    Left = 11
    Top = 91
    Width = 169
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Tel'#233'fono: 00000000'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelCountryYear: TLabel
    Left = 11
    Top = 116
    Width = 157
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Provincia, pa'#237's, a'#241'o'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelRights: TLabel
    Left = 10
    Top = 143
    Width = 225
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Todos los derechos reservados'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object BitBtn1: TBitBtn
    Left = 384
    Top = 136
    Width = 28
    Height = 28
    Action = ActionFormOriginClose
    Anchors = [akRight, akBottom]
    TabOrder = 0
    Glyph.Data = {
      E6000000424DE6000000000000007600000028000000100000000E0000000100
      0400000000007000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFF22FFFFFFFFFFFFF2222FFFFFFFFFFF222222FFFFFFFFF2222222FFF
      FFFFFF222F2222FFFFFFF222FFF2222FFFFFF22FFFFF2222FFFF22FFFFFFF222
      FFFF2FFFFFFFFF222FFFFFFFFFFFFFF22FFFFFFFFFFFFFFF22FFFFFFFFFFFFFF
      F22FFFFFFFFFFFFFFF22}
  end
  object ActionList1: TActionList
    Images = MainForm.ImageList1
    Left = 280
    Top = 96
    object ActionFormOriginClose: TAction
      Hint = 'Cerrar'
      ImageIndex = 0
      ShortCut = 27
      OnExecute = ActionFormOriginCloseExecute
    end
  end
end
