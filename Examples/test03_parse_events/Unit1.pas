unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, semin64.xml;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FXmlParser: TsanXMLParser;
    procedure OnParseEvent(Sender: TObject; EventType: TsanXMLEventType; XMLStack: TsanXMLStack);
    function EventTypeToString(EventType: TsanXMLEventType): string;
    procedure ShowAttributes(XMLStack: TsanXMLStack);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Memo1.Lines.BeginUpdate;
  try
    FXmlParser.ParseFromFile('..\..\701_01.xml');
  finally
    Memo1.Lines.EndUpdate;
  end;
end;

function TForm1.EventTypeToString(EventType: TsanXMLEventType): string;
begin

  case EventType of
    etStart: Result:= 'etStart';
    etFinish: Result:= 'etFinish';
    etHeaderOpen: Result:= 'etHeaderOpen';
    etHeaderClose: Result:= 'etHeaderClose';
    etElementOpen: Result:= 'etElementOpen';
    etElementClose: Result:= 'etElementClose';
    else Result:= 'Unknown';
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FXmlParser:= TsanXMLParser.Create;
  FXmlParser.OnParse:= OnParseEvent;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FXmlParser.Free;
end;

procedure TForm1.OnParseEvent(Sender: TObject; EventType: TsanXMLEventType;
  XMLStack: TsanXMLStack);
begin

  Memo1.Lines.Add('------------------------------------------------------');

  Memo1.Lines.Add('EventType: ' + EventTypeToString(EventType));

  if (EventType <> etStart) and (EventType <> etFinish) then begin

    Memo1.Lines.Add('Element: ' + XMLStack.GetElementPath);

    if (EventType = etHeaderClose) then begin
      ShowAttributes(XMLStack);
    end;

    if (EventType = etElementClose) then begin
      ShowAttributes(XMLStack);
      Memo1.Lines.Add('Value: ' + XMLStack.GetElementValue);
    end;

  end;

end;

procedure TForm1.ShowAttributes(XMLStack: TsanXMLStack);
var
  I: integer;
begin

  for I := 1 to XMLStack.GetAttributeCount do begin
    Memo1.Lines.Add('Attribute: ' + XMLStack.GetAttributeName(I-1) + '=' +
                                    XMLStack.GetAttributeValue(I-1));
  end;

end;

end.
