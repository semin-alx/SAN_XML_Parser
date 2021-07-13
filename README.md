SAN XML PARSER v.1.0
For Delphi XE2, XE3, XE4, XE5, XE6, XE7, XE8, 10, 10.1, 10.2

The library is designed to read large xml files 

There are two methods to read a xml data:

1. Parsing and loading the result to the memory
2. The event-driven parser, sequential reading of xml elements and 
   call of corresponding events

Support encodings:
UTF-8, UTF-16BE, UTF16-LE

For support ansi encoding just add semin64.xml.ansi.pas into your project
KOI8-R, KOI8-U, WINDOWS-1250, WINDOWS-1251, WINDOWS-1252, WINDOWS-1253, WINDOWS-1254, WINDOWS-1255,
WINDOWS-1256, WINDOWS-1257, WINDOWS-1258, ISO-8859-1, ISO-8859-2, ISO-8859-3, ISO-8859-4, ISO-8859-5, 
ISO-8859-6, ISO-8859-7, ISO-8859-8, ISO-8859-9

Examples:
test01_read_xml - Parsing a xml file and put the result to the memory (TsanXMLObject)
test02_read_xml_ansi - read a xml file (encoding: WINDOWS-1251)
test03_parse_events - Event-driven parser (TsanXMLParser)

Модули:
semin64.xml.pas - main module
semin64.xml.ansi.pas - support ansi encoding
semin64.memory.pas  - Stack memory manager, it is used inside semin64.xml.pas

My contact:
semin.aleksey1@yandex.ru

