unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, semin64.xml, semin64.xml.ansi;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FXml: TsanXMLObject;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  PrologElement: PsanXMLObjElement;
  RootElement: PsanXMLObjElement;
  AcceptElement: PsanXMLObjElement;
  OrderDetailsElement: PsanXMLObjElement;
  AttrName: string;
  Value: string;
  I: integer;
begin

  FXml.LoadFromFile('..\..\701_01_CP1251.xml');

  Memo1.Lines.Add('XML Header');

  PrologElement:= FXml.PrologueElement;

  for I := 1 to FXml.AttributeCount(PrologElement) do begin
    AttrName:= FXml.GetAttribute(PrologElement, I-1)^.pName;
    Value:= FXml.GetAttribute(PrologElement, I-1)^.pValue;
    Memo1.Lines.Add(' ' + AttrName + ': ' + Value);
  end;


  Memo1.Lines.Add('');
  Memo1.Lines.Add('XML Data');

  RootElement:= FXml.RootElement; // documents

  AcceptElement:= FXml.GetElement(RootElement, 'accept');
  Value:= FXml.GetAttributeValue(AcceptElement, 'action_id');
  Memo1.Lines.Add(' attribute action_id: ' + Value);
  Memo1.Lines.Add('');

  Value:= FXml.GetElement(AcceptElement, 'subject_id')^.pValue;
  Memo1.Lines.Add(' subject_id: ' + Value);

  Value:= FXml.GetElement(AcceptElement, 'counterparty_id')^.pValue;
  Memo1.Lines.Add(' counterparty_id: ' + Value);

  Value:= FXml.GetElement(AcceptElement, 'operation_date')^.pValue;
  Memo1.Lines.Add(' operation_date: ' + Value);

  Memo1.Lines.Add('');
  Memo1.Lines.Add(' order_details:');

  OrderDetailsElement:= FXml.GetElement(AcceptElement, 'order_details');

  for I := 1 to FXml.ChildCount(OrderDetailsElement) do begin
    Value:= FXml.GetElement(OrderDetailsElement, I-1)^.pValue;
    Memo1.Lines.Add('  ' + Value);
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FXml:= TsanXMLObject.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FXml.Free;
end;

end.
