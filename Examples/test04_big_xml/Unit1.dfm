object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 510
  ClientWidth = 891
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 208
    Top = 13
    Width = 41
    Height = 13
    Caption = 'Size(Mb)'
  end
  object lblProcessed: TLabel
    Left = 352
    Top = 13
    Width = 59
    Height = 13
    Caption = 'lblProcessed'
    Visible = False
  end
  object lblXMLObjectResult: TLabel
    Left = 208
    Top = 101
    Width = 91
    Height = 13
    Caption = 'lblXMLObjectResult'
    Visible = False
  end
  object lblXMLParserResult: TLabel
    Left = 584
    Top = 101
    Width = 90
    Height = 13
    Caption = 'lblXMLParserResult'
    Visible = False
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 30
    Height = 13
    Caption = 'Tests:'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 169
    Height = 25
    Caption = 'Generate XML File (test.xml)'
    TabOrder = 0
    OnClick = Button1Click
  end
  object edtWishSizeMb: TEdit
    Left = 256
    Top = 10
    Width = 73
    Height = 21
    TabOrder = 1
    Text = '500'
  end
  object Button2: TButton
    Left = 8
    Top = 96
    Width = 169
    Height = 25
    Caption = 'TsanXMLObject: Load test.xml'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 384
    Top = 96
    Width = 169
    Height = 25
    Caption = 'TsanXMLParser: Parse test.xml'
    TabOrder = 3
    OnClick = Button3Click
  end
end
