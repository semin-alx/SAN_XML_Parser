unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, semin64.xml;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    edtWishSizeMb: TEdit;
    lblProcessed: TLabel;
    Button2: TButton;
    lblXMLObjectResult: TLabel;
    Button3: TButton;
    lblXMLParserResult: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FProcessed: integer;
    FXmlParser: TsanXMLParser;
    FXmlData: TsanXMLObject;
    procedure OnParseEvent(Sender: TObject; EventType: TsanXMLEventType; XMLStack: TsanXMLStack);

    procedure GenerateXMLFile;
    function GetDetailElement(Num: integer): UTF8String;
    procedure ShowProcessed(Value: integer);
    procedure AppendToXmlFile(FileStream: TFileStream; Value: UTF8String);

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AppendToXmlFile(FileStream: TFileStream; Value: UTF8String);
begin
  FileStream.Write(Value[1], Length(Value));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  GenerateXMLFile;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Tick1, Tick2: DWORD;
begin

  lblXMLObjectResult.Visible:= True;
  lblXMLObjectResult.Caption:= 'Loading...';
  lblXMLObjectResult.Update;

  Tick1:= GetTickCount;
  FXmlData.LoadFromFile('../../test.xml');
  Tick2:= GetTickCount;

  lblXMLObjectResult.Caption:= Format('File has loaded %d ms', [Tick2 - Tick1]);

  ShowMessage('ok');
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Tick1, Tick2: DWORD;
begin

  lblXMLParserResult.Visible:= True;
  lblXMLParserResult.Caption:= 'Parsing...';
  lblXMLParserResult.Update;

  Tick1:= GetTickCount;
  FXmlParser.ParseFromFile('../../test.xml');
  Tick2:= GetTickCount;

  lblXMLParserResult.Caption:= Format('File has parsed %d ms', [Tick2 - Tick1]);

  ShowMessage('ok');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FXmlParser:= TsanXMLParser.Create;
  FXmlParser.OnParse:= OnParseEvent;
  FXmlData:= TsanXMLObject.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FXmlData.Free;
  FXmlParser.Free;
end;

procedure TForm1.GenerateXMLFile;
var
  nDetCount: integer;
  nWishSize: Int64;
  FileStream: TFileStream;
  Preamble: TBytes;
  Encoding: TEncoding;
  I: integer;
  ProcessedPr: integer;
begin

  nWishSize:= StrToInt64(edtWishSizeMb.Text) * 1024 * 1024;

  nDetCount:= nWishSize div Length(GetDetailElement(0));

  FileStream:= TFileStream.Create('../../test.xml', fmCreate);

  try

    Encoding:= TEncoding.UTF8;
    Preamble:= Encoding.GetPreamble;
    FileStream.Write(Preamble[0], Length(Preamble));

    AppendToXmlFile(FileStream, '<?xml version="1.0" encoding="UTF-8"?>'#13#10);
    AppendToXmlFile(FileStream, '<documents>'#13#10);
    AppendToXmlFile(FileStream, '  <details>'#13#10);

    ProcessedPr:= 0;
    FProcessed:= -1;
    lblProcessed.Visible:= True;
    ShowProcessed(ProcessedPr);

    for I:= 1 to nDetCount do begin

      AppendToXmlFile(FileStream, GetDetailElement(I));

      ProcessedPr:= I * 100 div nDetCount;
      ShowProcessed(ProcessedPr);

    end;

    AppendToXmlFile(FileStream, '  </details>'#13#10);
    AppendToXmlFile(FileStream, '</documents>');

    lblProcessed.Caption:= lblProcessed.Caption + '  Please wait few seconds...';
    lblProcessed.Update;

  finally
    FileStream.Free;
  end;

  lblProcessed.Caption:= 'Done!';
  ShowMessage('OK');

end;

function TForm1.GetDetailElement(Num: integer): UTF8String;
begin
  Result:= UTF8String(Format('    <detail>6967741261369175FFBRHCS03LF%0.10d</detail>'#13#10, [Num]));
end;

procedure TForm1.OnParseEvent(Sender: TObject; EventType: TsanXMLEventType;
  XMLStack: TsanXMLStack);
begin

end;

procedure TForm1.ShowProcessed(Value: integer);
begin

  if Value <> FProcessed then begin
    lblProcessed.Caption:= IntToStr(Value) + ' %';
    lblProcessed.Update;
    FProcessed:= Value;
  end;

end;

end.
