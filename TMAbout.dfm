object AboutDiag: TAboutDiag
  Left = 226
  Top = 110
  ActiveControl = CloseBtn
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 219
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object VersionLabel: TLabel
    Left = 240
    Top = 2
    Width = 206
    Height = 28
    Alignment = taRightJustify
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object BottomLine: TBevel
    Left = 0
    Top = 182
    Width = 452
    Height = 2
  end
  object OtherAboutLabel: TLabel
    Left = 9
    Top = 193
    Width = 140
    Height = 13
    Cursor = crHandPoint
    Caption = 'About Aestan Tray Menu'
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsUnderline]
    ParentFont = False
    Visible = False
    OnClick = OtherAboutLabelClick
  end
  object AboutHeader: TLabel
    Left = 0
    Top = 0
    Width = 452
    Height = 30
    AutoSize = False
    Color = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object JvGradient2: TJvGradient
    Left = 0
    Top = 30
    Width = 452
    Height = 6
    Align = alNone
    StartColor = 8404992
    EndColor = clSkyBlue
  end
  object CloseBtn: TButton
    Left = 372
    Top = 189
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object AboutText: TJvRichEdit
    Left = 0
    Top = 40
    Width = 452
    Height = 142
    BorderStyle = bsNone
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    HideScrollBars = False
    ParentFont = False
    PlainText = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
    OnURLClick = AboutTextsURLClick
  end
  object AboutAeTrayMenu: TJvStrHolder
    Capacity = 60
    Macros = <>
    Left = 390
    Top = 10
    InternalVer = 1
    StrData = (
      ''
      
        '546869732074726179206d656e7520776173206d616465207769746820416573' +
        '74616e2054726179204d656e752e'
      
        '4165547261794d656e7520686173206265656e2072656c656173656420696e74' +
        '6f20746865205075626c696320446f6d61696e2e'
      
        '284e6f74653a206120646966666572656e74206c6963656e7365206d61792061' +
        '70706c7920746f2074686520736f6674776172652074686973206d656e752069' +
        '732070617274206f66292e'
      ''
      
        '576562736974653a20687474703a2f2f7777772e6f6e6e6f64622e636f6d2f61' +
        '65747261796d656e75'
      
        '4f6e204269744275636b65743a2068747470733a2f2f6269746275636b65742e' +
        '6f72672f6f6e6e6f64622f747261796d656e752f'
      ''
      
        '4165547261794d656e7520686173206265656e207772697474656e207573696e' +
        '672044656c7068692032303037'
      ''
      '3d3d3d3d3d3d3d3d3d3d3d3d3d'
      '5448495244504152545920434f4445'
      '3d3d3d3d3d3d3d3d3d3d3d3d3d'
      
        '506f7274696f6e73206f662074686520736f7572636520636f6465206f662074' +
        '6869732070726f6772616d2068617665206265656e2074616b656e2066726f6d' +
        '206f7220'
      
        '696e7370697265642062792074686520736f75726365206f662074686520666f' +
        '6c6c6f77696e672070726f64756374732e20506c6561736520646f206e6f7420' +
        '72656d6f766520'
      '746865736520636f70797269676874206e6f74696365732e'
      ''
      '2a2a2a20496e6e6f205365747570'
      
        '4d6f7374206f662074686520636f646520666f722072656164696e6720616e64' +
        '2070617273696e672074686520636f6e66696775726174696f6e2066696c6520' +
        '686173206265656e20'
      
        '74616b656e2066726f6d2074686973206d617276656c6c6f75732c2066726565' +
        '20696e7374616c6c657220666f722057696e646f77732e'
      
        '496e6e6f20536574757020697320436f70797269676874202863292031393937' +
        '2d32303033204a6f7264616e2052757373656c6c2e20506f7274696f6e732062' +
        '79204d617274696a6e20'
      '4c61616e2e'
      '687474703a2f2f7777772e696e6e6f73657475702e6f7267'
      ''
      '2a2a2a20545372764374726c'
      
        '54686520636f646520746f20636f6e74726f6c2057696e646f77732073657276' +
        '6963657320686173206265656e20696e737069726564206279207468697320'
      '636f6d706f6e656e742e'
      
        '545372764374726c20697320436f7079726967687420286329204753432f544d' +
        '45444941'
      '687474703a2f2f7777772e6773632e6875'
      '687474703a2f2f7777772e746d656469612e6465'
      ''
      '3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d'
      '5448495244504152545920434f4d504f4e454e5453'
      '3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d3d'
      
        '4165547261794d656e7520757365732074686520666f6c6c6f77696e67207468' +
        '697264706172747920636f6d706f6e656e74732028696e20616c706861626574' +
        '6963616c20'
      '6f72646572293a'
      ''
      '2a2a2a204261724d656e7520436f6d706f6e656e7473'
      
        '436f707972696768742028632920323030302d3230303120426c756563617665' +
        '20436f6d706f6e656e7473'
      '687474703a2f2f7777772e626c7565636176652e6e65742f'
      ''
      '2a2a2a204a65646920436f6465204c69627261727920284a434c29'
      '2a2a2a204a6564692056434c20284a56434c29'
      '687474703a2f2f7777772e64656c7068692d6a6564692e6f7267'
      ''
      '3d3d3d3d3d3d3d3d'
      '5448414e4b5320544f'
      '3d3d3d3d3d3d3d3d'
      
        '54686520617574686f72206f66204165547261794d656e75206f776573206120' +
        '73696e6365726520227468616e6b20796f752220746f2074686520666f6c6c6f' +
        '77696e6720'
      '70656f706c653a'
      ''
      
        '2a2a2a2054686520636f6d706f6e656e7420616e6420736f6674776172652061' +
        '7574686f7273206d656e74696f6e65642061626f7665'
      ''
      '2a2a2a20566963746f7220422e20476f6e7a616c657a'
      '4f726967696e616c2069646561'
      ''
      '2a2a2a2053746576656e204d617274696e')
  end
end
