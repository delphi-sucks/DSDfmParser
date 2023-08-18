object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 676
  ClientWidth = 817
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 632
    Width = 817
    Height = 44
    Align = alBottom
    Caption = 'Panel1'
    TabOrder = 0
    object Button1: TButton
      Left = 736
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 817
    Height = 591
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 216
    ExplicitTop = 280
    ExplicitWidth = 289
    ExplicitHeight = 193
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object CheckBox1: TCheckBox
        Left = 12
        Top = 10
        Width = 97
        Height = 17
        Caption = 'CheckBox1'
        TabOrder = 0
      end
      object CheckBox2: TCheckBox
        Left = 12
        Top = 33
        Width = 97
        Height = 17
        Caption = 'CheckBox1'
        TabOrder = 1
      end
      object CheckBox3: TCheckBox
        Left = 12
        Top = 56
        Width = 97
        Height = 17
        Caption = 'CheckBox1'
        TabOrder = 2
      end
      object RadioGroup1: TRadioGroup
        Left = 12
        Top = 79
        Width = 185
        Height = 105
        Caption = 'RadioGroup1'
        Items.Strings = (
          'Item 1'
          'Item 2'
          'Item 3')
        TabOrder = 3
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 809
        Height = 561
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
        ExplicitLeft = 3
        ExplicitTop = 16
        ExplicitWidth = 281
        ExplicitHeight = 163
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 817
    Height = 41
    Align = alTop
    Caption = 'Panel2'
    TabOrder = 2
    ExplicitLeft = 320
    ExplicitTop = 336
    ExplicitWidth = 185
    object Label1: TLabel
      Left = 16
      Top = 13
      Width = 34
      Height = 15
      Caption = 'Label1'
    end
    object Edit1: TEdit
      Left = 56
      Top = 10
      Width = 755
      Height = 23
      TabOrder = 0
      Text = 'Edit1'
    end
  end
end
