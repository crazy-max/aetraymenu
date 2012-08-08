object MainForm: TMainForm
  Left = 225
  Top = 106
  Caption = 'AeTrayMenu'
  ClientHeight = 64
  ClientWidth = 206
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LeftClickPopup: TBcBarPopupMenu
    Images = ImageList
    OwnerDraw = True
    OnPopup = LeftRightClickPopupPopup
    Bar.Visible = False
    Bar.BarPicture.Visible = False
    Bar.BarBackPicture.Visible = False
    Bar.BarCaption.Font.Charset = DEFAULT_CHARSET
    Bar.BarCaption.Font.Color = clWhite
    Bar.BarCaption.Font.Height = -19
    Bar.BarCaption.Font.Name = 'Tahoma'
    Bar.BarCaption.Font.Style = [fsBold, fsItalic]
    Bar.BarCaption.Visible = False
    Separators.Font.Charset = DEFAULT_CHARSET
    Separators.Font.Color = clWindowText
    Separators.Font.Height = -11
    Separators.Font.Name = 'MS Sans Serif'
    Separators.Font.Style = []
    MenuFont.Charset = DEFAULT_CHARSET
    MenuFont.Color = clWindowText
    MenuFont.Height = -11
    MenuFont.Name = 'MS Sans Serif'
    MenuFont.Style = []
    Left = 40
    Top = 8
  end
  object RightClickPopup: TBcBarPopupMenu
    Images = ImageList
    OwnerDraw = True
    OnPopup = LeftRightClickPopupPopup
    Bar.Visible = False
    Bar.BarPicture.Visible = False
    Bar.BarBackPicture.Visible = False
    Bar.BarCaption.Font.Charset = DEFAULT_CHARSET
    Bar.BarCaption.Font.Color = clWhite
    Bar.BarCaption.Font.Height = -19
    Bar.BarCaption.Font.Name = 'Tahoma'
    Bar.BarCaption.Font.Style = [fsBold, fsItalic]
    Bar.BarCaption.Visible = False
    Separators.Font.Charset = DEFAULT_CHARSET
    Separators.Font.Color = clWindowText
    Separators.Font.Height = -11
    Separators.Font.Name = 'MS Sans Serif'
    Separators.Font.Style = []
    MenuFont.Charset = DEFAULT_CHARSET
    MenuFont.Color = clWindowText
    MenuFont.Height = -11
    MenuFont.Name = 'MS Sans Serif'
    MenuFont.Style = []
    Left = 72
    Top = 8
  end
  object TrayIcon: TJvTrayIcon
    IconIndex = -1
    Icons = ImageList
    DropDownMenu = LeftClickPopup
    PopupMenu = RightClickPopup
    Visibility = []
    OnDblClick = TrayIconDblClick
    Left = 104
    Top = 8
  end
  object CheckServicesTimer: TTimer
    Enabled = False
    Interval = 0
    OnTimer = CheckServicesTimerTimer
    Left = 136
    Top = 8
  end
  object ImageList: TImageList
    Left = 168
    Top = 8
  end
end
