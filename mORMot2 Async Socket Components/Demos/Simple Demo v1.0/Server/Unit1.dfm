object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'mORMot2 - Server - Simple Demo v1.0'
  ClientHeight = 435
  ClientWidth = 763
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000000040000000000000000000000000000000000004C6F
    9BFF396E9EFF276FAEFF2970AFFF2870B0FF2A73B1FF325ABBFF2939B7FF2A40
    B6FF2F68B9FF2973B1FF286FAFFF2770AFFF2D6DA8FF597CA2FF406598FFA7BE
    CEFF2A72ADFF2770B0FF296FAEFF2C74B3FF173073FF221573FF352995FF2C1D
    97FF191579FF25609FFF2974B3FF2970AFFF236EAFFF5088B4FFD9E0E3FF3E7B
    AEFF226BAEFF2A70AEFF2975B5FF265FA0FF1A2163FF5B698BFFF2F6EEFFB0BC
    C7FF222D64FF1D3476FF2A74B4FF2770B0FF2B71AEFF1B66A9FF7BA5C2FF276E
    AFFF2C6DA6FF2A659BFF2568A4FF407AB2FF7AB8DDFF8BD4FBFF90C7E2FF88CA
    EBFF86CDF2FF5C8EBCFF2868A4FF2B679FFF2B69A0FF256EAEFF3E7CB0FF286D
    ABFF29679EFF2E679DFF24578CFF44799FFF7EBADBFF426074FF1E222BFF2A33
    3EFF6391ABFF67A0BFFF245586FF2B6297FF2C669EFF2569A6FF3F7DB1FF3876
    ABFF266BAAFF2B6DA8FF4681BBFF3C7EB9FF5E90AFFF342C2BFF242025FF1710
    10FF39434CFF3D7EB3FF427DB6FF3373ACFF2A6EA8FF2168AAFF6F9BBBFF8EAF
    C9FF1E6AAAFF2C74B0FF2D76B7FF3E7FB6FFBAE1EBFFABC4CFFF99B1BAFF8BAB
    B8FFD5EEEFFF80ACCDFF2F77B8FF2F78B5FF2570AFFF2B70AAFFD1DEE3FFECF1
    F2FF3D7BAEFF256EAFFF1D65A6FF69A5CBFFD0EFF4FF465E82FFD5F6FBFF7490
    ABFFB5D2E0FFCFECF4FF286CAAFF2971AFFF1D67AAFF76A0C0FFFFFFFFFFFFFF
    FFFF9EBCCFFF1C67A9FF226AABFF629FC8FFDFFFFFFFBBD9E6FFD8F7FAFFC3E2
    EBFFE1FAFAFFBAE0EDFF276FADFF266FAFFF2C71ABFFE1EAEDFFFFFFFFFFFFFF
    FFFFF4F6F8FF4682B4FF216CACFF3176AFFFB4E0EEFFE9FFFFFF80B2D0FF7FB5
    D5FFC5E7F1FF5087B1FF256EADFF1E69AAFF8CAFC9FFFFFFFFFFFCFDFEFFFCFD
    FEFFFFFFFFFF98B2C3FF1D6AABFF24649FFF2E455FFF46759BFF246EB0FF2069
    A8FF23405EFF235686FF2570B5FF3A77A9FFE9EEEFFFFFFFFFFFFEFEFEFFFFFF
    FFFFB3BECAFF66A2CCFF65A9D7FF2B6EA9FF185083FF184B7EFF2D74B4FF296F
    ADFF1C619EFF226AA7FF3A7FB8FF70B6E0FF7292B3FFFFFEFCFFFFFFFFFFFFFF
    FFFFCED5DBFF4676A7FF5E8CB7FFD6DEE0FFA2C3D7FF538DBBFF286FAAFF3073
    ADFF6D9DBBFFCDDCE4FF85A2BDFF3C77ACFF8BA2BDFFFFFFFFFFFEFEFEFFFEFE
    FFFFFFFFFFFFDEE1E5FFD4D9E2FFFFFFFFFFFFFFFFFFE7EBECFF7FA3B6FF6B98
    B0FFF7F8F4FFFFFFFFFFEBEDF1FFCED2DCFFFFFEFDFFFFFFFFFFFFFFFFFFFFFF
    FFFFFEFEFEFFFFFFFFFFFFFFFFFFFEFEFEFFFCFDFEFFFFFFFFFFF8FAFAFFE8EE
    EFFFFFFFFFFFFCFDFEFFFFFFFFFFFFFFFFFFFEFEFEFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFEFEFEFFFDFDFEFFFFFFFFFFFFFFFFFFFEFFFFFFFEFFFFFFFFFF
    FFFFFEFFFFFFFFFFFFFFFEFEFFFFFDFDFEFFFFFFFFFFFFFFFFFFFFFFFFFF0000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object Image1: TImage
    Left = 546
    Top = 71
    Width = 209
    Height = 201
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 416
    Width = 763
    Height = 19
    Panels = <
      item
        Text = 'Status: Offline!'
        Width = 150
      end
      item
        Text = 'Clients Connected: 0'
        Width = 150
      end>
  end
  object ListView1: TListView
    Left = 0
    Top = 65
    Width = 540
    Height = 213
    Columns = <
      item
        AutoSize = True
        Caption = 'NickName'
      end
      item
        AutoSize = True
        Caption = 'IP Address'
      end
      item
        Caption = 'Connected At'
        Width = 150
      end
      item
        Caption = 'Handle'
        Width = 80
      end>
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 1
    ViewStyle = vsReport
  end
  object LogMemo: TMemo
    Left = 0
    Top = 278
    Width = 763
    Height = 138
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    WordWrap = False
  end
  object gb1: TGroupBox
    Left = 0
    Top = 0
    Width = 763
    Height = 65
    Align = alTop
    TabOrder = 3
    object btnActivate: TButton
      Left = 16
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Activate'
      TabOrder = 0
      OnClick = btnActivateClick
    end
    object sedtPort: TSpinEdit
      Left = 105
      Top = 25
      Width = 121
      Height = 24
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 3434
      OnChange = sedtPortChange
    end
    object btnClearLog: TButton
      Left = 232
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Clear Log'
      TabOrder = 2
      OnClick = btnClearLogClick
    end
  end
  object mMServer1: TmMServer
    LogLevel = llNone
    UseTLS = True
    OnHandleCommand = mMServer1HandleCommand
    OnConnect = mMServer1Connect
    OnDisconnect = mMServer1Disconnect
    Left = 368
    Top = 152
  end
  object PopupMenu1: TPopupMenu
    Left = 248
    Top = 152
    object S1: TMenuItem
      Caption = 'Send To All'
      OnClick = S1Click
    end
    object S2: TMenuItem
      Caption = 'Send To Selected'
      OnClick = S2Click
    end
    object D1: TMenuItem
      Caption = 'Disconnect Selected'
      OnClick = D1Click
    end
    object G1: TMenuItem
      Caption = 'Get Client Information'
      OnClick = G1Click
    end
    object G2: TMenuItem
      Caption = 'Get ScreenShot'
      OnClick = G2Click
    end
  end
end
