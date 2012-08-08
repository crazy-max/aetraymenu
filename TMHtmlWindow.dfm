object HtmlWindow: THtmlWindow
  Left = 226
  Top = 113
  Caption = 'HtmlWindow'
  ClientHeight = 266
  ClientWidth = 492
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 233
    Width = 492
    Height = 2
    Align = alBottom
    Shape = bsTopLine
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 492
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    object AboutHeader: TLabel
      Left = 0
      Top = 0
      Width = 492
      Height = 30
      Align = alTop
      AutoSize = False
      Caption = ' Caption here'
      Color = clWhite
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -19
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = True
      Layout = tlCenter
    end
    object JvGradient2: TJvGradient
      Left = 0
      Top = 30
      Width = 492
      Height = 6
      Align = alBottom
      StartColor = 8404992
      EndColor = clSkyBlue
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 235
    Width = 492
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      492
      31)
    object CloseBtn: TButton
      Left = 414
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 0
      OnClick = CloseBtnClick
    end
  end
  object WebBrowser: TWebBrowser
    Left = 0
    Top = 36
    Width = 492
    Height = 197
    Align = alClient
    TabOrder = 2
    OnTitleChange = WebBrowserTitleChange
    OnBeforeNavigate2 = WebBrowserBeforeNavigate2
    OnDocumentComplete = WebBrowserDocumentComplete
    ControlData = {
      4C000000DA3200005C1400000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
