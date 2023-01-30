unit semin64.xml;

{ =============================================================================

    SAN XML PARSER v.1.0
    Main module

    Description:

    This module contains classes for parsing xml files.
    Support encodings: UTF-8, UTF-16BE, UTF16-LE.
    For ansi encodings add semin64.xml.ansi module to your project.

    TsanXMLParser - XML event-driven parser.
    TsanXMLObject - It parses XML file and loads a result into the memory.

  =============================================================================
	
	Copyright (c) 2021 Alexey Semin
	semin.aleksey1@yandex.ru

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.	

  ============================================================================= }

// 30.01.2023 fix  TsanXMLReader.DefineXMLEncoding
//   The encoding can be specified in any case

interface

uses Windows, System.SysUtils, System.Classes, System.Character,
     System.StrUtils, System.RegularExpressions, System.Generics.Collections,
     semin64.memory;

const
  SAN_XML_READ_BUF_SIZE = 512;
  SAN_XML_RING_BUF_SIZE = 20;

type

  PsanXMLStackAttribute = ^TsanXMLStackAttribute;
  TsanXMLStackAttribute = record
    pNext: PsanXMLStackAttribute;
    pName: PChar;
    pValue: PChar;
  end;

  PsanXMLStackElement = ^TsanXMLStackElement;
  TsanXMLStackElement = record
    pNext: PsanXMLStackElement;
    pName: PChar;
    pPath: PChar;
    pValue: PChar;
    pAttribute: PsanXMLStackAttribute;
    AttrCount: integer;
    UserData: Pointer;
  end;

  TsanXMLStack = class(TObject)
  private
    FMemoryManager: TsanStackMemoryManager;
    FActiveElementPtr: PsanXMLStackElement;
    FStackSize: integer;
    procedure ErrorInvalidXMLStructure;
    procedure ErrorIndexOutOfBound;
    function AllocString(Value: string): PChar;
    function GetStackElementPtr(StackIndex: integer): PsanXMLStackElement;
    function GetAttributePtr(AttrIndex: integer; StackIndex: integer): PsanXMLStackAttribute;
  public

    constructor Create;
    destructor Destroy; override;

    procedure _OpenElement(ElementName: string);
    procedure _CloseElement;
    procedure _AddAttribute(AttrName: string; Value: string);
    procedure _SetValue(Value: string);

    function StackSize: integer;
    function GetElementName(StackIndex: integer = 0): string;
    function GetElementPath(StackIndex: integer = 0): string;
    function GetElementValue(StackIndex: integer = 0): string;
    function GetAttributeCount(StackIndex: integer = 0): integer;
    function GetAttributeName(AttrIndex: integer; StackIndex: integer = 0): string;
    function GetAttributeValue(AttrIndex: integer; StackIndex: integer = 0): string; overload;
    function GetAttributeValue(AttrName: string; StackIndex: integer = 0): string; overload;
    function GetAttributeIndex(AttrName: string; StackIndex: integer = 0): integer;
    function GetElementUserData(StackIndex: integer = 0): Pointer;
    procedure SetElementUserData(UserData: Pointer; StackIndex: integer = 0);

  end;

  TsanXMLAnsiEncoderClass = class of TsanXMLAnsiEncoder;

  TsanXMLAnsiEncoder = class(TObject)
  public
    class procedure RegisterEncoder(EncName: string; EncoderClass: TsanXMLAnsiEncoderClass);
    class function FindEncoder(EncName: string): TsanXMLAnsiEncoderClass;
  public
    function AnsiToWide(C: AnsiChar): Char; virtual; abstract;
  end;

  TsanXMLEncoding = (cUnknown, cUTF8, cUTF16BE, cUTF16LE, cANSI);

  TsanXMLReader = class(TObject)
  private
    FSource: TStream;
    FEncoding: TsanXMLEncoding;
    FAnsiEncoder: TsanXMLAnsiEncoder;
    FBuffer: array[0..SAN_XML_READ_BUF_SIZE-1] of Byte;
    FBufferSize: Cardinal;
    FBufferCursor: Cardinal;
    FEof: Boolean;
    procedure ErrorUnexpectedEndOfData;
    procedure ErrorDataIsEmpty;
    procedure ErrorUnknownPreambula;
    procedure ErrorIvalidFormat;
    procedure ErrorUTF8Encoding;
    function LoadBuffer: Boolean;
    procedure DefineXMLEncoding;
    function CharacterEncodingFromXMLDecl: string;
    function GetEncodingByPreambula: TsanXMLEncoding;
    function GetEncodingByLTCode: TsanXMLEncoding;
    function CreateAnsiEncoder(CharacterEncoding: string): TsanXMLAnsiEncoder;
    procedure SkipBytes(Count: integer);
    function GetChar_UTF8: Char;
    function GetChar_UTF16BE: Char;
    function GetChar_UTF16LE: Char;
    function GetChar_ANSI: Char;
    function GetByte: Byte;
  public
    constructor Create;
    destructor Destroy; override;
    function GetChar: Char;
    function IsEof: Boolean;
    procedure Init;
    property Source: TStream read FSource write FSource;
  end;

  TsanXMLParsePosition = record
    LineNo: integer;
    CharNo: integer;
    CharNoInLine: integer;
  end;

  TsanXMLEventType = (etStart, etFinish, etHeaderOpen, etHeaderClose, etElementOpen, etElementClose);

  TsanXMLParserEvent = procedure(Sender: TObject;
                                 EventType: TsanXMLEventType;
                                 XMLStack: TsanXMLStack) of object;

  TsanXMLRingBuffer = class(TObject)
  private
    FValues: array of Char;
    FBufferSize: integer;
    FRingPos: integer;
    FCursor: integer;
    function GetValue(Index: integer): Char;
    function GetRealIndex(Index: integer): integer;
  public
    constructor Create(BufferSize: integer);
    procedure Put(V: Char);
    procedure Clear;
    property Values[Index: integer]: Char read GetValue; default;
    property Size: integer read FBufferSize;
  end;

  TsanXMLParser = class(TObject)
  private
    FOnParse: TsanXMLParserEvent;
    FXMLStack: TsanXMLStack;
    FXMLReader: TsanXMLReader;
    FStringBulder: TStringBuilder;
    FParsePositionInfo: TsanXMLParsePosition;
    FFirstPullOut: Boolean;
    FReadBuffer: TsanXMLRingBuffer;

    procedure ErrorUnexpectedEndOfData;
    procedure ErrorIvalidFormat;
    procedure ErrorXMLHeader;
    procedure ErrorExpectChar(C: Char);
    procedure ErrorUnknownSpecialChar;
    procedure ErrorParseIntValue(StrInt: string);
    procedure ErrorCharCodeTooBig(StrInt: string);

    procedure RaiseParseError(Mes: string);
    procedure PullOutChar;
    procedure InitParser;
    procedure DoXMLParse;
    procedure UpdatePositionInfo;
    function DoParseItem: Boolean;
    procedure DoParseHeader;
    procedure DoParseCloseElement;
    procedure DoParseOpenElement;
    procedure DoParseDeclaration;
    function DoParseAttribute: Boolean;
    function DoParseIdentifier: string;
    function DoParseAttrValue: string;
    function DoParseElementValue: string;
    function DoParseSpecialChar: Char;
    function DoParseCodeOfChar: Char;
    procedure DoParseCDATA(StringBulder: TStringBuilder);
    procedure DoEvent(EventType: TsanXMLEventType);
    procedure SkipSpaces;
    function IsSpace(C: Char): Boolean;
    function IsIdentifierChar(C: Char): Boolean;
    function ReadChar: Char;
    function IsEof: Boolean;
    function IsCDATA: Boolean;
    function IsCDATAEof: Boolean;
    function IsLetter(C: Char): Boolean;
    function IsLetterOrDigit(C: Char): Boolean;
    procedure ReadCodeOfChar(var StrCode: string; var IsHex: Boolean);
    function StrDecCodeToChar(StrCode: string): Char;
    function StrHexCodeToChar(StrCode: string): Char;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseFromStream(Stream: TStream);
    procedure ParseFromFile(FileName: string);
    property OnParse: TsanXMLParserEvent read FOnParse write FOnParse;
    property ParsePositionInfo: TsanXMLParsePosition read FParsePositionInfo;
  end;

  PsanXMLObjElement = ^TsanXMLObjElement;
  TsanXMLObjElement = record
    pName: PChar;
    pValue: PChar;
  end;

  PsanXMLObjAttribute = ^TsanXMLObjAttribute;
  TsanXMLObjAttribute = record
    pName: PChar;
    pValue: PChar;
  end;

  PsanXMLObjElementEx = ^TsanXMLObjElementEx;
  TsanXMLObjElementEx = record
    pName: PChar;  // TsanXMLObjElement.pName
    pValue: PChar; // TsanXMLObjElement.pValue
    pNext: PsanXMLObjElementEx;
    pFirstChild: PsanXMLObjElementEx;
    pArrayChilds: ^PsanXMLObjElementEx;
    ChildCount: integer;
    pArrayAttributes: ^PsanXMLObjAttribute;
    AttrCount: integer;
  end;

  TsanXMLObject = class(TObject)
  private
    FParser: TsanXMLParser;
    FRootElement: PsanXMLObjElementEx;
    FPrologue: PsanXMLObjElementEx;
    FMemoryManager: TsanStackMemoryManager;
    procedure ErrorIndexOutOfBound;
    procedure DoParserEvent(Sender: TObject; EventType: TsanXMLEventType; XMLStack: TsanXMLStack);
    procedure DoInit;
    procedure DoPrologue(XMLStack: TsanXMLStack);
    procedure DoOpenElement(XMLStack: TsanXMLStack);
    procedure DoCloseElement(XMLStack: TsanXMLStack);
    procedure LoadAttributes(pElement: PsanXMLObjElementEx; XMLStack: TsanXMLStack);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(FileName: string);
    function RootElement: PsanXMLObjElement;
    function PrologueElement: PsanXMLObjElement;
    function ChildCount(pElement: PsanXMLObjElement): integer;
    function GetElement(pParent: PsanXMLObjElement; Index: integer): PsanXMLObjElement; overload;
    function GetElement(pParent: PsanXMLObjElement; ElementName: string): PsanXMLObjElement; overload;
    function AttributeCount(pElement: PsanXMLObjElement): integer;
    function GetAttribute(pElement: PsanXMLObjElement; AttrIndex: integer): PsanXMLObjAttribute; overload;
    function GetAttribute(pElement: PsanXMLObjElement; AttrName: string): PsanXMLObjAttribute;   overload;
    function GetAttributeValue(pElement: PsanXMLObjElement; AttrName: string): string;
    function MemoryUsed: Cardinal;
  end;

implementation

resourcestring
  RSXmlInvalidXMLStructure = 'Invalid XML Structure';
  RSXmlIndexOutOfBound = 'Index out of bound';
  RSXmlUnexpectedEndOfData = 'Unexpected end of data';
  RSXmlDataIsEmpty = 'Data is empty';
  RSXmlUnknownPreambula = 'Unknown format, unrecognized encoding preambula';
  RSXmlIvalidFormat = 'Invalid XML format';
  RSXmlErrorUTF8Encoding = 'Error UTF8 encoding';
  RSXmlExpectChar = 'Expect char: %s';
  RSXmlHeader = 'Invalid XML Header';
  RSUnknownSpecialChar = 'Unknown special character';
  RSErrorParseIntValue = 'Invalid integer value: %s';
  RSCharCodeTooBig = 'Char code (%s) is too big';
  RSEncNameAlreadyRegistered = 'Encoder for %s already registered';
  RSCharacterEncodingUnsupport = 'Character encoding %s is not supported';

type
  TsanXMLAnsiEncoderRegEntity = record
    EncName: string;
    EncoderClass: TsanXMLAnsiEncoderClass;
  end;

var
  _AnsiEncoderRegistry: TList<TsanXMLAnsiEncoderRegEntity>;

{ TsanXMLStack }

function TsanXMLStack.AllocString(Value: string): PChar;
begin
  Result:= FMemoryManager.GetMem(Value);
end;

constructor TsanXMLStack.Create;
begin
  FActiveElementPtr:= nil;
  FStackSize:= 0;
  FMemoryManager:= TsanStackMemoryManager.Create;
end;

destructor TsanXMLStack.Destroy;
begin
  FMemoryManager.Free;
  inherited;
end;

procedure TsanXMLStack.ErrorIndexOutOfBound;
begin
  raise Exception.Create(RSXmlIndexOutOfBound);
end;

procedure TsanXMLStack.ErrorInvalidXMLStructure;
begin
  raise Exception.Create(RSXmlInvalidXMLStructure);
end;

function TsanXMLStack.GetAttributeCount(StackIndex: integer): integer;
begin
  Result:= GetStackElementPtr(StackIndex)^.AttrCount;
end;

function TsanXMLStack.GetAttributeIndex(AttrName: string;
  StackIndex: integer): integer;
var
  pElement: PsanXMLStackElement;
  pAttr: PsanXMLStackAttribute;
  nIndex: integer;
begin

  Result:= -1;
  nIndex:= -1;

  pElement:= GetStackElementPtr(StackIndex);

  pAttr:= pElement^.pAttribute;

  while Assigned(pAttr) do begin

    Inc(nIndex);

    if String(pAttr^.pName) = AttrName then begin
      Result:= nIndex;
      break;
    end else begin
      pAttr:= pAttr^.pNext;
    end;

  end;

end;

function TsanXMLStack.GetAttributeName(AttrIndex, StackIndex: integer): string;
begin
  Result:= String(GetAttributePtr(AttrIndex, StackIndex)^.pName);
end;

function TsanXMLStack.GetAttributePtr(AttrIndex,
  StackIndex: integer): PsanXMLStackAttribute;
var
  pElement: PsanXMLStackElement;
  n: integer;
begin

  pElement:= GetStackElementPtr(StackIndex);

  if AttrIndex >= pElement.AttrCount
    then ErrorIndexOutOfBound;

  n:= AttrIndex;

  Result:= pElement^.pAttribute;

  while n > 0 do begin
    Result:= Result^.pNext;
    Dec(n);
  end;

end;

function TsanXMLStack.GetAttributeValue(AttrIndex, StackIndex: integer): string;
begin
  Result:= String(GetAttributePtr(AttrIndex, StackIndex)^.pValue);
end;

function TsanXMLStack.GetAttributeValue(AttrName: string;
  StackIndex: integer): string;
var
  AttrIndex: integer;
begin

  AttrIndex:= GetAttributeIndex(AttrName, StackIndex);

  if AttrIndex = -1 then begin
    Result:= '';
  end else begin
    Result:= GetAttributeValue(AttrIndex, StackIndex);
  end;

end;

function TsanXMLStack.GetElementName(StackIndex: integer): string;
begin
  Result:= String(FActiveElementPtr^.pName);
end;

function TsanXMLStack.GetElementPath(StackIndex: integer): string;
begin
  Result:= String(FActiveElementPtr^.pPath);
end;

function TsanXMLStack.GetElementUserData(StackIndex: integer): Pointer;
begin
  Result:= GetStackElementPtr(StackIndex)^.UserData;
end;

function TsanXMLStack.GetElementValue(StackIndex: integer): string;
begin
  if Assigned(FActiveElementPtr^.pValue) then begin
    Result:= String(FActiveElementPtr^.pValue);
  end else begin
    Result:= '';
  end;
end;

function TsanXMLStack.GetStackElementPtr(
  StackIndex: integer): PsanXMLStackElement;
var
  n: integer;
begin

  if StackIndex >= FStackSize then ErrorInvalidXMLStructure;

  n:= StackIndex;
  Result:= FActiveElementPtr;

  while n > 0 do begin
    Result:= Result^.pNext;
    Dec(n);
  end;

end;

procedure TsanXMLStack.SetElementUserData(UserData: Pointer;
  StackIndex: integer);
begin
  GetStackElementPtr(StackIndex)^.UserData:= UserData;
end;

function TsanXMLStack.StackSize: integer;
begin
  Result:= FStackSize;
end;

procedure TsanXMLStack._AddAttribute(AttrName, Value: string);
var
  P: PsanXMLStackAttribute;
begin

  P:= FMemoryManager.GetMem(SizeOf(TsanXMLStackAttribute));

  P^.pName:= AllocString(AttrName);
  P^.pValue:= AllocString(Value);

  if Assigned(FActiveElementPtr^.pAttribute) then begin
    P^.pNext:= FActiveElementPtr^.pAttribute;
  end else begin
    P^.pNext:= nil;
  end;

  FActiveElementPtr^.pAttribute:= P;
  Inc(FActiveElementPtr^.AttrCount);

end;

procedure TsanXMLStack._CloseElement;
var
  P: PsanXMLStackElement;
begin

  if FStackSize = 0 then ErrorInvalidXMLStructure;

  P:= FActiveElementPtr;
  FActiveElementPtr:= P^.pNext;
  FMemoryManager.FreeMem(P);
  Dec(FStackSize);

end;

procedure TsanXMLStack._OpenElement(ElementName: string);
var
  P: PsanXMLStackElement;
begin

  P:= FMemoryManager.GetMem(SizeOf(TsanXMLStackElement));

  P^.pNext:= nil;
  P^.pName:= AllocString(ElementName);
  P^.pPath:= nil;
  P^.pValue:= nil;
  P^.pAttribute:= nil;
  P^.AttrCount:= 0;

  if Assigned(FActiveElementPtr) then begin
    P^.pNext:= FActiveElementPtr;
    P^.pPath:= AllocString(String(P^.pNext^.pPath) + '/' + ElementName);
  end else begin
    P^.pPath:= P^.pName;
  end;

  FActiveElementPtr:= P;

  Inc(FStackSize);

end;

procedure TsanXMLStack._SetValue(Value: string);
begin
  FActiveElementPtr^.pValue:= AllocString(Value);
end;

{ TsanXMLReader }

constructor TsanXMLReader.Create;
begin
  FEncoding:= cUnknown;
  FAnsiEncoder:= nil;
  FEof:= True;
end;

function TsanXMLReader.CreateAnsiEncoder(
  CharacterEncoding: string): TsanXMLAnsiEncoder;
var
  EncoderClass: TsanXMLAnsiEncoderClass;
begin

  EncoderClass:= TsanXMLAnsiEncoder.FindEncoder(CharacterEncoding);

  if Not Assigned(EncoderClass) then begin
    raise Exception.Create(Format(RSCharacterEncodingUnsupport, [CharacterEncoding]));
  end;

  Result:= EncoderClass.Create;

end;

destructor TsanXMLReader.Destroy;
begin
  if Assigned(FAnsiEncoder) then FAnsiEncoder.Free;
  inherited;
end;

procedure TsanXMLReader.DefineXMLEncoding;
var
  CharacterEncoding: string;
begin

  // [Preambula]<?xml version="1.0" encoding="UTF-8"?>

  FEncoding:= GetEncodingByPreambula;

  if FEncoding = cUnknown then begin
    // I'll try to get the encodeing by the code of the first char '<'
    FEncoding:= GetEncodingByLTCode;
  end;

  if (FEncoding = cUnknown) and (FBuffer[0] <> $3C) then begin
    ErrorUnknownPreambula;
  end;

  if (FEncoding = cUnknown) and
     (AnsiChar(FBuffer[0]) = '<') and
     (AnsiChar(FBuffer[1]) = '?')
  then begin

    // fix 30/01/2023
    //CharacterEncoding:= CharacterEncodingFromXMLDecl;
    CharacterEncoding:= UpperCase(CharacterEncodingFromXMLDecl);

    if (CharacterEncoding = 'UTF-8') or (CharacterEncoding = '') then begin
      FEncoding:= cUTF8;
    end else begin

      FEncoding:= cANSI;

      if Assigned(FAnsiEncoder) then begin
        FAnsiEncoder.Free;
        FAnsiEncoder:= nil;
      end;

      FAnsiEncoder:= CreateAnsiEncoder(CharacterEncoding);

    end;

  end else begin
    FEncoding:= cUTF8;
  end;

end;

procedure TsanXMLReader.ErrorDataIsEmpty;
begin
  raise Exception.Create(RSXmlDataIsEmpty);
end;

procedure TsanXMLReader.ErrorIvalidFormat;
begin
  raise Exception.Create(RSXmlIvalidFormat);
end;

procedure TsanXMLReader.ErrorUnexpectedEndOfData;
begin
  raise Exception.Create(RSXmlUnexpectedEndOfData);
end;

procedure TsanXMLReader.ErrorUnknownPreambula;
begin
  raise Exception.Create(RSXmlUnknownPreambula);
end;

procedure TsanXMLReader.ErrorUTF8Encoding;
begin
  raise Exception.Create(RSXmlErrorUTF8Encoding);
end;

function TsanXMLReader.GetChar: Char;
begin

  case FEncoding of
    cUTF16BE: Result:= GetChar_UTF16BE;
    cUTF16LE: Result:= GetChar_UTF16LE;
    cANSI:    Result:= GetChar_ANSI;
    else begin
      Result:= GetChar_UTF8;
    end;
  end;

end;

function TsanXMLReader.GetChar_ANSI: Char;
var
  C: AnsiChar;
begin

  C:= AnsiChar(GetByte);

  if Assigned(FAnsiEncoder) then begin
    Result:= FAnsiEncoder.AnsiToWide(C);
  end else begin
    //MultiByteToWideChar(CP_ACP, 0, @C, 1, @Result, 1);
    Result:= '?';
  end;

end;

function TsanXMLReader.GetChar_UTF16BE: Char;
var
  b1, b2: integer;
begin
  b1:= GetByte;
  b2:= GetByte;
  Result:= Char((b1 shl 8) or b2);
end;

function TsanXMLReader.GetChar_UTF16LE: Char;
var
  b1, b2: integer;
begin
  b1:= GetByte;
  b2:= GetByte;
  Result:= Char((b2 shl 8) or b1);
end;

function TsanXMLReader.GetChar_UTF8: Char;
var
  b1, b2, b3, b4: integer;
  Count: integer;
begin

  Count:= 0;
  Result:= Char(0);

  // 0xxxxxxx
  // 110xxxxx 10xxxxxx
  // 1110xxxx 10xxxxxx 10xxxxxx
  // 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx

  b1:= GetByte;

  if (b1 and $80) = $00 then Count:= 1 else
  if (b1 and $E0) = $C0 then Count:= 2 else
  if (b1 and $F0) = $E0 then Count:= 3 else
  if (b1 and $F8) = $F0 then Count:= 4 else
    ErrorUTF8Encoding;

  case Count of

    1:
    begin
      Result:= Char(b1);
    end;

    2:
    begin

      b2:= GetByte;
      if (b2 and $C0) <> $80 then ErrorUTF8Encoding;

      b1:= (b1 and $1F) shl 6;
      b2:= (b2 and $3F);

      Result:= Char(b1 or b2);

    end;

    3:
    begin

      b2:= GetByte;
      if (b2 and $C0) <> $80 then ErrorUTF8Encoding;

      b3:= GetByte;
      if (b3 and $C0) <> $80 then ErrorUTF8Encoding;

      b1:= (b1 and $0F) shl 12;
      b2:= (b2 and $3F) shl 6;
      b3:= (b3 and $3F);

      Result:= Char(b1 or b2 or b3);

    end;

    4:
    begin

      b2:= GetByte;
      if (b2 and $C0) <> $80 then ErrorUTF8Encoding;

      b3:= GetByte;
      if (b3 and $C0) <> $80 then ErrorUTF8Encoding;

      b4:= GetByte;
      if (b4 and $C0) <> $80 then ErrorUTF8Encoding;

      b1:= (b1 and $07) shl 18;
      b2:= (b2 and $3F) shl 12;
      b3:= (b3 and $3F) shl 6;
      b4:= (b4 and $3F);

      Result:= Char(b1 or b2 or b3 or b4);

    end;

  end;

end;

function TsanXMLReader.GetEncodingByLTCode: TsanXMLEncoding;
var
  b1, b2: Byte;
begin

  b1:= FBuffer[0];
  b2:= FBuffer[1];

  //   UTF8, ANSI	3C
  //   UTF16BE    00 3C
  //   UTF16LE    3C 00

  if (b1 = $00) and (b2 = $3C) then begin
    Result:= cUTF16BE;
  end else
  if (b1 = $3C) and (b2 = $00) then begin
    Result:= cUTF16LE;
  end else begin
    Result:= cUnknown;
  end;

end;

function TsanXMLReader.GetEncodingByPreambula: TsanXMLEncoding;
var
  b1, b2, b3: Byte;
begin

  if FBufferSize <= 3 then ErrorIvalidFormat;

  b1:= FBuffer[0];
  b2:= FBuffer[1];
  b3:= FBuffer[2];

  //   UTF8	   EF BB BF
  //   UTF16BE FE FF
  //   UTF16LE FF FE

  if (b1 = $EF) and (b2 = $BB) and (b3 = $BF) then begin
    Result:= cUTF8;
    SkipBytes(3);
  end else
  if (b1 = $FE) and (b2 = $FF) then begin
    Result:= cUTF16BE;
    SkipBytes(2)
  end else
  if (b1 = $FF) and (b2 = $FE) then begin
    Result:= cUTF16LE;
    SkipBytes(2)
  end else begin
    Result:= cUnknown;
  end;

end;

procedure TsanXMLReader.Init;
begin

  FEncoding:= cUnknown;
  FBufferSize:= 0;
  FBufferCursor:= 0;
  FSource.Position:= 0;

  FEof:= LoadBuffer;
  if FEof then ErrorDataIsEmpty;

  DefineXMLEncoding;

end;

function TsanXMLReader.IsEof: Boolean;
begin
  Result:= FEof;
end;

function TsanXMLReader.LoadBuffer: Boolean;
begin
  FBufferSize:= FSource.Read(FBuffer, SAN_XML_READ_BUF_SIZE);
  FBufferCursor:= 0;
  Result:= FBufferSize = 0;
end;

function TsanXMLReader.CharacterEncodingFromXMLDecl: string;
const
  XML_DECLARATION_MAX_LENGTH = 200; // Взял из головы, но должно хватить
var
  SaveStreamPosition: integer;
  Buf: AnsiString;
  M: TMatch;
begin

  SaveStreamPosition:= FSource.Position;

  FSource.Position:= 0;

  SetLength(Buf, XML_DECLARATION_MAX_LENGTH);
  FSource.Read((@Buf[1])^, XML_DECLARATION_MAX_LENGTH);

  FSource.Position:= SaveStreamPosition;

  M:= TRegEx.Match(String(Buf),
                   '^.*encoding\s*\=\s*[\",\'']([a-z,0-9.\-]*).*\?>',
                   [roIgnoreCase]);


  if M.Success then begin
    Result:= M.Result('$1');
  end else begin
    Result:= '';
  end;

end;

procedure TsanXMLReader.SkipBytes(Count: integer);
var
  I: integer;
begin
  for I := 1 to Count do GetByte;
end;

function TsanXMLReader.GetByte: Byte;
begin

  if FEof then ErrorUnexpectedEndOfData;

  Result:= FBuffer[FBufferCursor];

  Inc(FBufferCursor);

  if FBufferCursor >= FBufferSize then begin
    if FBufferSize = SAN_XML_READ_BUF_SIZE
      then FEof:= LoadBuffer
      else FEof:= True;
  end;

end;

{ TsanXMLParser }

constructor TsanXMLParser.Create;
begin
  FXMLStack:= TsanXMLStack.Create;
  FXMLReader:= TsanXMLReader.Create;
  FStringBulder:= TStringBuilder.Create;
  FReadBuffer:= TsanXMLRingBuffer.Create(SAN_XML_RING_BUF_SIZE);
end;

destructor TsanXMLParser.Destroy;
begin
  FReadBuffer.Free;
  FStringBulder.Free;
  FXMLReader.Free;
  FXMLStack.Free;
  inherited;
end;

procedure TsanXMLParser.DoEvent(EventType: TsanXMLEventType);
begin
  if Assigned(FOnParse) then begin
    FOnParse(Self, EventType, FXMLStack);
  end;
end;

function TsanXMLParser.DoParseAttribute: Boolean;
var
  AttrName: string;
  AttrValue: string;
begin

  AttrName:= DoParseIdentifier;

  SkipSpaces;

  if ReadChar <> '=' then ErrorExpectChar('=');

  PullOutChar;
  SkipSpaces;

  AttrValue:= DoParseAttrValue;

  FXMLStack._AddAttribute(AttrName, AttrValue);

  PullOutChar;
  SkipSpaces;

  Result:= Not IsLetter(ReadChar);

end;

function TsanXMLParser.DoParseAttrValue: string;
var
  QuotChar: Char;
begin

  if (ReadChar <> '''') and (ReadChar <> '"') then begin
    ErrorExpectChar('"');
  end;

  QuotChar:= ReadChar;

  FStringBulder.Clear;

  PullOutChar;

  while ReadChar <> QuotChar do begin

    if ReadChar = '&' then begin
      FStringBulder.Append(DoParseSpecialChar);
    end else begin
      FStringBulder.Append(ReadChar);
      PullOutChar;
    end;

  end;

  Result:= FStringBulder.ToString;

end;

procedure TsanXMLParser.DoParseCDATA(StringBulder: TStringBuilder);
var
  I: integer;
begin

  // <![CDATA[...]]>
  for I := 1 to 9 do PullOutChar; // Skip <![CDATA[

  while Not IsCDATAEof do begin
    StringBulder.Append(ReadChar);
    PullOutChar;
  end;

  for I := 1 to 3 do PullOutChar; // Skip ]]>

end;

procedure TsanXMLParser.DoParseCloseElement;
begin

  // </NAME>
  DoEvent(etElementClose);
  FXMLStack._CloseElement;

  while ReadChar <> '>' do PullOutChar;
  PullOutChar; // skip '>'
  SkipSpaces;

end;

function TsanXMLParser.DoParseCodeOfChar: Char;
var
  IsHex: Boolean;
  StrCode: string;
begin

  // &#1048;   -> 1048  IsHex = False
  // &#x6C34;  -> 6C34  IsHex = True
  ReadCodeOfChar(StrCode, IsHex);

  if IsHex then begin
    Result:= StrHexCodeToChar(StrCode);
  end else begin
    Result:= StrDecCodeToChar(StrCode);
  end;

end;

procedure TsanXMLParser.DoParseDeclaration;
var
  Nesting: integer;
begin

  // example: <!DOCTYPE greeting [
  //           <!ELEMENT greeting (#PCDATA)>
  //          ]>
  // We just skip this string

  Nesting:= 0;

  while (Nesting > 0) or (ReadChar <> '>') do begin
    if ReadChar = '<' then Inc(Nesting);
    if ReadChar = '>' then Dec(Nesting);
    PullOutChar;
  end;

  PullOutChar; // skip '>'

end;

function TsanXMLParser.DoParseElementValue: string;
begin

  Result:= '';

  FStringBulder.Clear;

  while True do begin

    if ReadChar = '&' then FStringBulder.Append(DoParseSpecialChar) else
    if IsCDATA then DoParseCDATA(FStringBulder) else
    if ReadChar = '<' then break else
    begin
      FStringBulder.Append(ReadChar);
      PullOutChar;
    end;

  end;

  Result:= FStringBulder.ToString;

end;

procedure TsanXMLParser.DoParseHeader;
begin

  FXMLStack._OpenElement('XML_Prologue');

  // <?xml version="1.0" encoding="UTF-8"?>
  // At the moment we have read the chars: <?

  // check <?xml_

  PullOutChar;

  if ReadChar <> 'x' then ErrorXMLHeader;

  PullOutChar;
  if ReadChar <> 'm' then ErrorXMLHeader;

  PullOutChar;
  if ReadChar <> 'l' then ErrorXMLHeader;

  PullOutChar;
  if Not IsSpace(ReadChar) then ErrorXMLHeader;

  SkipSpaces;

  if Not IsLetter(ReadChar) then ErrorXMLHeader;

  while Not DoParseAttribute do;

  if ReadChar <> '?' then ErrorExpectChar('?');

  PullOutChar;
  if ReadChar <> '>' then ErrorExpectChar('>');

  PullOutChar; // skip '>'

  DoEvent(etHeaderOpen);
  DoEvent(etHeaderClose);
  FXMLStack._CloseElement;

end;

function TsanXMLParser.DoParseIdentifier: string;
begin

  FStringBulder.Clear;

  if Not IsIdentifierChar(ReadChar) then begin
    ErrorIvalidFormat;
  end;

  repeat
    FStringBulder.Append(ReadChar);
    PullOutChar;
  until Not IsIdentifierChar(ReadChar);

  Result:= FStringBulder.ToString;

end;

function TsanXMLParser.DoParseItem: Boolean;
begin

  // Skip all spaces and other garbage
  SkipSpaces;

  // The first char of any element of XML must be <
  if ReadChar <> '<' then ErrorExpectChar('<');

  // Read second char
  PullOutChar;

  // Variants are possible:
  // ? - XML Header          <?xml ....
  // / - close element   </name>
  // ! - Declarations and comments, example: <!DOCTYPE greeting SYSTEM "hello.dtd">
  // An optional space and first char of element name - open element

  case ReadChar of
    '?': DoParseHeader;
    '/': DoParseCloseElement;
    '!': DoParseDeclaration;
    else begin
      DoParseOpenElement;
    end;
  end;

  Result:= IsEof;

end;

procedure TsanXMLParser.DoParseOpenElement;
begin

  // Example: <aaaa .... >  или <aaaa ..../>

  FXMLStack._OpenElement(DoParseIdentifier);

  SkipSpaces;

  // If a read char is a letter then it is a begining of attributes
  if IsLetter(ReadChar) then begin
    while Not DoParseAttribute do;
  end;

  DoEvent(etElementOpen);

  // There are two possible variants: > and />
  case ReadChar of

    '/':
    begin
      PullOutChar;
      if ReadChar <> '>' then ErrorExpectChar('>');
      DoEvent(etElementClose);
      FXMLStack._CloseElement;
      PullOutChar; // skip >
    end;

    '>':
    begin

      PullOutChar;
      SkipSpaces;

      if Not((FReadBuffer[0] = '<') and (IsLetter(FReadBuffer[1])))
      then begin
        FXMLStack._SetValue(DoParseElementValue);
      end;

    end;

    else begin
      ErrorIvalidFormat;
    end;

  end;

end;

function TsanXMLParser.DoParseSpecialChar: Char;
begin

  Result:= Char(0);

  // &apos; &quot; &lt; &gt; &amp;

  if (FReadBuffer[0] = '&') and
     (FReadBuffer[1] = 'a') and
     (FReadBuffer[2] = 'p') and
     (FReadBuffer[3] = 'o') and
     (FReadBuffer[4] = 's') and
     (FReadBuffer[5] = ';') then Result:= '''' else

  if (FReadBuffer[0] = '&') and
     (FReadBuffer[1] = 'q') and
     (FReadBuffer[2] = 'u') and
     (FReadBuffer[3] = 'o') and
     (FReadBuffer[4] = 't') and
     (FReadBuffer[5] = ';') then Result:= '"' else

  if (FReadBuffer[0] = '&') and
     (FReadBuffer[1] = 'l') and
     (FReadBuffer[2] = 't') and
     (FReadBuffer[3] = ';') then Result:= '<' else

  if (FReadBuffer[0] = '&') and
     (FReadBuffer[1] = 'g') and
     (FReadBuffer[2] = 't') and
     (FReadBuffer[3] = ';') then Result:= '>' else

  if (FReadBuffer[0] = '&') and
     (FReadBuffer[1] = 'a') and
     (FReadBuffer[2] = 'm') and
     (FReadBuffer[3] = 'p') and
     (FReadBuffer[4] = ';') then Result:= '&' else

  if (FReadBuffer[0] = '&') and
     (FReadBuffer[1] = '#') then
  begin
    Result:= DoParseCodeOfChar;
  end else
  begin
    ErrorUnknownSpecialChar;
  end;

  while ReadChar <> ';' do PullOutChar;

  PullOutChar;

end;

procedure TsanXMLParser.DoXMLParse;
begin

  DoEvent(etStart);

  // Read the first char and start parsing
  PullOutChar;

  // Start the parsing loop
  while Not DoParseItem do;

  // The parsing has done
  // The stack must be empty (All elements are closed)
  if FXMLStack.StackSize > 0 then ErrorUnexpectedEndOfData;

  DoEvent(etFinish);

end;

procedure TsanXMLParser.ErrorCharCodeTooBig(StrInt: string);
begin
  RaiseParseError(Format(RSCharCodeTooBig, [StrInt]));
end;

procedure TsanXMLParser.ErrorExpectChar(C: Char);
begin
  RaiseParseError(RSXmlIvalidFormat + #13 + Format(RSXmlExpectChar, [C]));
end;

procedure TsanXMLParser.ErrorIvalidFormat;
begin
  RaiseParseError(RSXmlIvalidFormat);
end;

procedure TsanXMLParser.ErrorParseIntValue(StrInt: string);
begin
  RaiseParseError(Format(RSErrorParseIntValue, [StrInt]));
end;

procedure TsanXMLParser.ErrorUnexpectedEndOfData;
begin
  RaiseParseError(RSXmlUnexpectedEndOfData);
end;

procedure TsanXMLParser.ErrorUnknownSpecialChar;
begin
  RaiseParseError(RSUnknownSpecialChar);
end;

procedure TsanXMLParser.ErrorXMLHeader;
begin
  RaiseParseError(RSXmlHeader);
end;

procedure TsanXMLParser.InitParser;
begin
  FXMLReader.Init;
  FParsePositionInfo.LineNo:= 0;
  FParsePositionInfo.CharNo:= 0;
  FParsePositionInfo.CharNoInLine:= 0;
  FFirstPullOut:= True;
end;

function TsanXMLParser.IsCDATA: Boolean;
begin

  Result:= (FReadBuffer[0] = '<') and
           (FReadBuffer[1] = '!') and
           (FReadBuffer[2] = '[') and
           (FReadBuffer[3] = 'C') and
           (FReadBuffer[4] = 'D') and
           (FReadBuffer[5] = 'A') and
           (FReadBuffer[6] = 'T') and
           (FReadBuffer[7] = 'A') and
           (FReadBuffer[8] = '[');

end;

function TsanXMLParser.IsCDATAEof: Boolean;
begin
  Result:= (FReadBuffer[0] = ']') and
           (FReadBuffer[1] = ']') and
           (FReadBuffer[2] = '>');
end;

function TsanXMLParser.IsEof: Boolean;
begin
  Result:= FReadBuffer[0] = Char(0);
end;

function TsanXMLParser.IsIdentifierChar(C: Char): Boolean;
begin

  // An identifier can contain letters, digits, -, _, :
  Result:= IsLetterOrDigit(C) or
    (C = '-') or (C = '_') or (C = ':');

end;

function TsanXMLParser.IsLetter(C: Char): Boolean;
begin

{$IFDEF VER230}
  Result:= TCharacter.IsLetter(C); // Delphi XE2
{$ELSE}
  Result:= C.IsLetter;             // Delphi XE3 and higher
{$ENDIF}

end;

function TsanXMLParser.IsLetterOrDigit(C: Char): Boolean;
begin

{$IFDEF VER230}
  Result:= TCharacter.IsLetterOrDigit(C); // Delphi XE2
{$ELSE}
  Result:= C.IsLetterOrDigit;             // Delphi XE3 and higher
{$ENDIF}

end;

function TsanXMLParser.IsSpace(C: Char): Boolean;
begin
  Result:= (C = ' ') or (C = #10) or (C = #13) or (C = #9);
end;

procedure TsanXMLParser.ParseFromFile(FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream:= TFileStream.Create(FileName, fmOpenRead);
  try
    ParseFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TsanXMLParser.ParseFromStream(Stream: TStream);
begin
  FXMLReader.Source:= Stream;
  InitParser;
  DoXMLParse;
end;

procedure TsanXMLParser.PullOutChar;
var
  I: integer;
begin

  if FFirstPullOut then begin

    FFirstPullOut:= False;

    for I := 1 to FReadBuffer.Size do begin

      if FXMLReader.IsEof then begin
        FReadBuffer.Put(Char(0));
      end else begin
        FReadBuffer.Put(FXMLReader.GetChar);
      end;

    end;

  end else begin

    if FReadBuffer[0] = Char(0) then begin
      raise Exception.Create(RSXmlUnexpectedEndOfData);
    end;

    if FXMLReader.IsEof then begin
      FReadBuffer.Put(Char(0));
    end else begin
      FReadBuffer.Put(FXMLReader.GetChar);
    end;

  end;

  UpdatePositionInfo;

end;

procedure TsanXMLParser.RaiseParseError(Mes: string);
var
  S: string;
begin

  S:= Format('%s'#13'Line: %d Pos: %d',
    [Mes, FParsePositionInfo.LineNo, FParsePositionInfo.CharNoInLine]);

  raise Exception.Create(S);

end;

function TsanXMLParser.ReadChar: Char;
begin
  Result:= FReadBuffer[0];
end;

procedure TsanXMLParser.ReadCodeOfChar(var StrCode: string; var IsHex: Boolean);
var
  I: integer;
  nStart: integer;
  nEnd: integer;
begin

  // &#1048; &#x6C34;
  IsHex:= (FReadBuffer[2] = 'x') or (FReadBuffer[2] = 'X');

  if isHex then begin
    nStart:= 4;
  end else begin
    nStart:= 3;
  end;

  nEnd:= -1;

  for I := nStart to FReadBuffer.Size do begin
    if FReadBuffer[I-1] = ';' then begin
      nEnd:= I-1;
      break;
    end;
  end;

  if nEnd < nStart then ErrorIvalidFormat;

  SetLength(StrCode, nEnd - nStart + 1);

  for I := 1 to Length(StrCode) do StrCode[I]:= FReadBuffer[nStart + I - 2];

end;

procedure TsanXMLParser.SkipSpaces;
begin

  while IsSpace(ReadChar) do begin
    PullOutChar;
  end;

end;

function TsanXMLParser.StrDecCodeToChar(StrCode: string): Char;
var
  V: integer;
begin

  if Not TryStrToInt(StrCode, V) then begin
    ErrorParseIntValue(StrCode);
  end;

  if V > 65535 then ErrorCharCodeTooBig(StrCode);

  Result:= Char(V);

end;

function TsanXMLParser.StrHexCodeToChar(StrCode: string): Char;
var
  I: integer;
  V: word;
  R: word;
  HexStrLen: integer;
  CharCode: integer;
begin

  R:= 0;

  HexStrLen:= Length(StrCode);

  if HexStrLen > 4 then ErrorCharCodeTooBig(StrCode);

  for I:= HexStrLen downto 1 do begin

    CharCode:= Integer(StrCode[I]);
    V:= 0;

    case CharCode of
      48..57:  V:= CharCode - 48;
      65..70:  V:= CharCode - 65 + $A;
      97..102: V:= CharCode - 97 + $A;
      else begin
        ErrorParseIntValue(StrCode);
      end;
    end;

    R:= R or (V shl ((HexStrLen - I) shl 2));

  end;

  Result:= Char(R);

end;

procedure TsanXMLParser.UpdatePositionInfo;
begin

  if FParsePositionInfo.LineNo = 0 then begin
    FParsePositionInfo.LineNo:= 1;
    FParsePositionInfo.CharNo:= 1;
    FParsePositionInfo.CharNoInLine:= 1;
  end else begin
    Inc(FParsePositionInfo.CharNo);
    if ReadChar = #13 then begin
      Inc(FParsePositionInfo.LineNo);
      FParsePositionInfo.CharNoInLine:= 0;
    end else begin
      if ReadChar <> #10 then Inc(FParsePositionInfo.CharNoInLine);
    end;
  end;

end;

{ TsanXMLObject }

function TsanXMLObject.AttributeCount(pElement: PsanXMLObjElement): integer;
begin
  Result:= PsanXMLObjElementEx(pElement)^.AttrCount;
end;

function TsanXMLObject.ChildCount(pElement: PsanXMLObjElement): integer;
begin
  Result:= PsanXMLObjElementEx(pElement)^.ChildCount;
end;

constructor TsanXMLObject.Create;
begin
  FRootElement:= nil;
  FPrologue:= nil;
  FMemoryManager:= TsanStackMemoryManager.Create;
  FParser:= TsanXMLParser.Create;
  FParser.OnParse:= DoParserEvent;
end;

destructor TsanXMLObject.Destroy;
begin
  FMemoryManager.Clear;
  FParser.Free;
  FMemoryManager.Free;
  inherited;
end;

procedure TsanXMLObject.DoCloseElement(XMLStack: TsanXMLStack);
var
  pElement: PsanXMLObjElementEx;
  pChildElement: PsanXMLObjElementEx;
  pArrayCell: ^PsanXMLObjElementEx;
  N: integer;
begin

  pElement:= PsanXMLObjElementEx(XMLStack.GetElementUserData);

  pElement^.pValue:= FMemoryManager.GetMem(XMLStack.GetElementValue);

  if pElement^.ChildCount > 0 then begin

    pElement^.pArrayChilds:= FMemoryManager.GetMem(pElement^.ChildCount * SizeOf(Pointer));

    pChildElement:= pElement^.pFirstChild;

    N:= pElement^.ChildCount;

    while Assigned(pChildElement) do begin

      pArrayCell:= Pointer(Int64(pElement^.pArrayChilds) + (N-1)*SizeOf(Pointer));
      pArrayCell^:= pChildElement;

      pChildElement:= pChildElement^.pNext;
      Dec(N);

    end;

  end;

end;

procedure TsanXMLObject.DoInit;
begin
  FRootElement:= nil;
  FMemoryManager.Clear;
end;

procedure TsanXMLObject.DoOpenElement(XMLStack: TsanXMLStack);
var
  pElement: PsanXMLObjElementEx;
  pParentElement: PsanXMLObjElementEx;
begin

  pElement:= FMemoryManager.GetMem(SizeOf(TsanXMLObjElementEx));

  XMLStack.SetElementUserData(pElement);

  pElement^.pName:= FMemoryManager.GetMem(XMLStack.GetElementName);
  pElement^.pValue:= nil;

  pElement^.pNext:= nil;
  pElement^.pFirstChild:= nil;
  pElement^.pArrayChilds:= nil;
  pElement^.ChildCount:= 0;

  LoadAttributes(pElement, XMLStack);

  if Not Assigned(FRootElement) then FRootElement:= pElement;

  if XMLStack.StackSize > 1 then begin
    pParentElement:= PsanXMLObjElementEx(XMLStack.GetElementUserData(1));
    pElement^.pNext:= pParentElement^.pFirstChild;
    pParentElement^.pFirstChild:= pElement;
    Inc(pParentElement^.ChildCount);
  end;

end;

procedure TsanXMLObject.DoParserEvent(Sender: TObject;
  EventType: TsanXMLEventType; XMLStack: TsanXMLStack);
begin

  case EventType of
    etStart:        DoInit;
    etHeaderOpen:   DoPrologue(XMLStack);
    etElementOpen:  DoOpenElement(XMLStack);
    etElementClose: DoCloseElement(XMLStack);
  end;

end;

procedure TsanXMLObject.DoPrologue(XMLStack: TsanXMLStack);
begin

  FPrologue:= FMemoryManager.GetMem(SizeOf(TsanXMLObjElementEx));

  FPrologue^.pName:= nil;
  FPrologue^.pValue:= nil;
  FPrologue^.pNext:= nil;
  FPrologue^.pFirstChild:= nil;
  FPrologue^.pArrayChilds:= nil;
  FPrologue^.ChildCount:= 0;

  LoadAttributes(FPrologue, XMLStack);

end;

procedure TsanXMLObject.ErrorIndexOutOfBound;
begin
  raise Exception.Create(RSXmlIndexOutOfBound);
end;

function TsanXMLObject.GetElement(pParent: PsanXMLObjElement;
  ElementName: string): PsanXMLObjElement;
var
  I: integer;
  P: PsanXMLObjElement;
begin

  Result:= nil;

  for I := 1 to PsanXMLObjElementEx(pParent)^.ChildCount do begin
    P:= GetElement(pParent, I-1);
    if CompareStr(P^.pName, ElementName) = 0 then begin
      Result:= P;
      break;
    end;
  end;

end;

function TsanXMLObject.GetAttribute(pElement: PsanXMLObjElement;
  AttrIndex: integer): PsanXMLObjAttribute;
var
  nAdr: Int64;
  pAdr: ^PsanXMLObjAttribute;
begin

  if AttrIndex >= PsanXMLObjElementEx(pElement)^.AttrCount then ErrorIndexOutOfBound;

  nAdr:= Int64(PsanXMLObjElementEx(pElement)^.pArrayAttributes) + AttrIndex * SizeOf(Pointer);
  pAdr:= Pointer(nAdr);
  Result:= pAdr^;

end;

function TsanXMLObject.GetAttribute(pElement: PsanXMLObjElement;
  AttrName: string): PsanXMLObjAttribute;
var
  I: integer;
  P: PsanXMLObjAttribute;
begin

  Result:= nil;

  for I := 1 to PsanXMLObjElementEx(pElement)^.AttrCount do begin
    P:= GetAttribute(pElement, I-1);
    if CompareStr(P^.pName, AttrName) = 0 then begin
      Result:= P;
      break;
    end;
  end;

end;

function TsanXMLObject.GetAttributeValue(pElement: PsanXMLObjElement;
  AttrName: string): string;
var
  P: PsanXMLObjAttribute;
begin

  P:= GetAttribute(pElement, AttrName);

  if Assigned(P) then begin
    Result:= P^.pValue;
  end else begin
    Result:= '';
  end;

end;

function TsanXMLObject.GetElement(pParent: PsanXMLObjElement;
  Index: integer): PsanXMLObjElement;
var
  nAdr: Int64;
  pAdr: ^PsanXMLObjElement;
begin
  if Index >= PsanXMLObjElementEx(pParent)^.ChildCount then ErrorIndexOutOfBound;
  nAdr:= Int64(PsanXMLObjElementEx(pParent)^.pArrayChilds) + Index * SizeOf(Pointer);
  pAdr:= Pointer(nAdr);
  Result:= pAdr^;
end;

procedure TsanXMLObject.LoadAttributes(pElement: PsanXMLObjElementEx;
  XMLStack: TsanXMLStack);
var
  I: integer;
  pAttr: PsanXMLObjAttribute;
  pArrayCell: ^PsanXMLObjAttribute;
begin

  pElement^.AttrCount:= XMLStack.GetAttributeCount;
  pElement^.pArrayAttributes:= nil;

  if pElement^.AttrCount > 0 then begin
    pElement^.pArrayAttributes:= FMemoryManager.GetMem(SizeOf(Pointer)*pElement^.AttrCount);
  end;

  for I := 1 to pElement^.AttrCount do begin
    pAttr:= FMemoryManager.GetMem(SizeOf(TsanXMLObjAttribute));
    pAttr^.pName:= FMemoryManager.GetMem(XMLStack.GetAttributeName(I-1));
    pAttr^.pValue:= FMemoryManager.GetMem(XMLStack.GetAttributeValue(I-1));
    pArrayCell:= Pointer(Uint64(pElement^.pArrayAttributes) + (I-1)*SizeOf(Pointer));
    pArrayCell^:= pAttr;
  end;

end;

procedure TsanXMLObject.LoadFromFile(FileName: string);
begin
  FParser.ParseFromFile(FileName);
end;

procedure TsanXMLObject.LoadFromStream(Stream: TStream);
begin
  FParser.ParseFromStream(Stream);
end;

function TsanXMLObject.MemoryUsed: Cardinal;
begin
  Result:= FMemoryManager.TotalMemory;
end;

function TsanXMLObject.PrologueElement: PsanXMLObjElement;
begin
  Result:= PsanXMLObjElement(FPrologue);
end;

function TsanXMLObject.RootElement: PsanXMLObjElement;
begin
  Result:= PsanXMLObjElement(FRootElement);
end;

{ TsanXMLRingBuffer }

procedure TsanXMLRingBuffer.Clear;
var
  I: integer;
begin

  FRingPos:= 0;
  FCursor:= 0;

  for I := 1 to FBufferSize do FValues[I-1]:= Char(0);

end;

constructor TsanXMLRingBuffer.Create(BufferSize: integer);
begin
  FBufferSize:= BufferSize;
  SetLength(FValues, BufferSize);
  Clear;
end;

function TsanXMLRingBuffer.GetRealIndex(Index: integer): integer;
begin
  Result:= (FRingPos + Index) mod FBufferSize;
end;

function TsanXMLRingBuffer.GetValue(Index: integer): Char;
begin
  Result:= FValues[GetRealIndex(Index)];
end;

procedure TsanXMLRingBuffer.Put(V: Char);
begin

  if FCursor < FBufferSize then begin
    FValues[FCursor]:= V;
    Inc(FCursor);
  end else begin
    FRingPos:= (FRingPos + 1) mod FBufferSize;
    FValues[GetRealIndex(FBufferSize-1)]:= V;
  end;

end;

{ TsanXMLAnsiEncoder }

class function TsanXMLAnsiEncoder.FindEncoder(
  EncName: string): TsanXMLAnsiEncoderClass;
var
  I: integer;
begin

  Result:= nil;
  EncName:= UpperCase(EncName);

  for I := 1 to _AnsiEncoderRegistry.Count do begin

    if _AnsiEncoderRegistry[I-1].EncName = EncName then begin
      Result:= _AnsiEncoderRegistry[I-1].EncoderClass;
      break;
    end;

  end;

end;

class procedure TsanXMLAnsiEncoder.RegisterEncoder(EncName: string;
  EncoderClass: TsanXMLAnsiEncoderClass);
var
  RegEntity: TsanXMLAnsiEncoderRegEntity;
begin

  if Assigned(TsanXMLAnsiEncoder.FindEncoder(EncName)) then begin
    raise Exception.Create(Format(RSEncNameAlreadyRegistered, [EncName]));
  end;

  RegEntity.EncName:= EncName;
  RegEntity.EncoderClass:= EncoderClass;
  _AnsiEncoderRegistry.Add(RegEntity);

end;

initialization
  _AnsiEncoderRegistry:= TList<TsanXMLAnsiEncoderRegEntity>.Create;

finalization
  _AnsiEncoderRegistry.Free;

end.
