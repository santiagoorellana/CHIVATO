object FormConvert: TFormConvert
  Left = 192
  Top = 124
  Width = 250
  Height = 170
  BorderStyle = bsSizeToolWin
  Caption = 'Convertidor'
  Color = clBtnFace
  Constraints.MaxHeight = 170
  Constraints.MinHeight = 170
  Constraints.MinWidth = 250
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
    234
    132)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 165
    Top = 12
    Width = 26
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Bytes'
  end
  object Label2: TLabel
    Left = 165
    Top = 36
    Width = 46
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Kilo Bytes'
  end
  object Label3: TLabel
    Left = 165
    Top = 60
    Width = 56
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Mega Bytes'
  end
  object Label4: TLabel
    Left = 165
    Top = 84
    Width = 51
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Giga Bytes'
  end
  object Label5: TLabel
    Left = 165
    Top = 108
    Width = 51
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Tera Bytes'
  end
  object EditBytes: TEdit
    Left = 8
    Top = 9
    Width = 151
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = EditBytesChange
    OnKeyPress = EditKeyPress
  end
  object EditKBytes: TEdit
    Left = 8
    Top = 33
    Width = 151
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = EditKBytesChange
    OnKeyPress = EditKeyPress
  end
  object EditMBytes: TEdit
    Left = 8
    Top = 57
    Width = 151
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = EditMBytesChange
    OnKeyPress = EditKeyPress
  end
  object EditGBytes: TEdit
    Left = 8
    Top = 81
    Width = 151
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = EditGBytesChange
    OnKeyPress = EditKeyPress
  end
  object EditTBytes: TEdit
    Left = 8
    Top = 105
    Width = 151
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    OnChange = EditTBytesChange
    OnKeyPress = EditKeyPress
  end
  object ActionList1: TActionList
    Left = 72
    Top = 16
    object ActionClose: TAction
      Caption = 'Cerrar'
      ShortCut = 27
      OnExecute = ActionCloseExecute
    end
  end
end
