unit semin64.xml.ansi;

{ =============================================================================

    SAN XML PARSER v.1.0
    Ansi encoding support

    You need to add this module to your application.
    Sample: uses semin64.xml.ansi

    Supported encodings:
    KOI8-R, KOI8-U, WINDOWS-1250, WINDOWS-1251,
    WINDOWS-1252, WINDOWS-1253, WINDOWS-1254, WINDOWS-1255,
    WINDOWS-1256, WINDOWS-1257, WINDOWS-1258, ISO-8859-1,
    ISO-8859-2, ISO-8859-3, ISO-8859-4, ISO-8859-5, ISO-8859-6,
    ISO-8859-7, ISO-8859-8, ISO-8859-9
	
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

interface

uses semin64.xml;

type

  TsanXML_KOI8R_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_KOI8U_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_CP1250_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_CP1251_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_CP1252_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_CP1253_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_CP1254_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_CP1255_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_CP1256_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_CP1257_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_CP1258_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_ISO_8859_1_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_ISO_8859_2_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_ISO_8859_3_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_ISO_8859_4_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_ISO_8859_5_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_ISO_8859_6_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_ISO_8859_7_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_ISO_8859_8_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

  TsanXML_ISO_8859_9_Encoder = class(TsanXMLAnsiEncoder)
  public
    function AnsiToWide(C: AnsiChar): Char; override;
  end;

implementation

const

ICONV_KOI8_R_UCS2: array[0..127] of Word =
($2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524, $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
 $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248, $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
 $2550, $2551, $2552, $0451, $2553, $2554, $2555, $2556, $2557, $2558, $2559, $255A, $255B, $255C, $255D, $255E,
 $255F, $2560, $2561, $0401, $2562, $2563, $2564, $2565, $2566, $2567, $2568, $2569, $256A, $256B, $256C, $00A9,
 $044E, $0430, $0431, $0446, $0434, $0435, $0444, $0433, $0445, $0438, $0439, $043A, $043B, $043C, $043D, $043E,
 $043F, $044F, $0440, $0441, $0442, $0443, $0436, $0432, $044C, $044B, $0437, $0448, $044D, $0449, $0447, $044A,
 $042E, $0410, $0411, $0426, $0414, $0415, $0424, $0413, $0425, $0418, $0419, $041A, $041B, $041C, $041D, $041E,
 $041F, $042F, $0420, $0421, $0422, $0423, $0416, $0412, $042C, $042B, $0417, $0428, $042D, $0429, $0427, $042A);

ICONV_KOI8_U_UCS2: array[0..127] of Word =
($2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524, $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
 $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248, $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
 $2550, $2551, $2552, $0451, $0454, $2554, $0456, $0457, $2557, $2558, $2559, $255A, $255B, $0491, $255D, $255E,
 $255F, $2560, $2561, $0401, $0404, $2563, $0406, $0407, $2566, $2567, $2568, $2569, $256A, $0490, $256C, $00A9,
 $044E, $0430, $0431, $0446, $0434, $0435, $0444, $0433, $0445, $0438, $0439, $043A, $043B, $043C, $043D, $043E,
 $043F, $044F, $0440, $0441, $0442, $0443, $0436, $0432, $044C, $044B, $0437, $0448, $044D, $0449, $0447, $044A,
 $042E, $0410, $0411, $0426, $0414, $0415, $0424, $0413, $0425, $0418, $0419, $041A, $041B, $041C, $041D, $041E,
 $041F, $042F, $0420, $0421, $0422, $0423, $0416, $0412, $042C, $042B, $0417, $0428, $042D, $0429, $0427, $042A);

ICONV_CP1250_UCS2: array[0..127] of Word =
($20AC, $0000, $201A, $0000, $201E, $2026, $2020, $2021, $0000, $2030, $0160, $2039, $015A, $0164, $017D, $0179,
 $0000, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0000, $2122, $0161, $203A, $015B, $0165, $017E, $017A,
 $00A0, $02C7, $02D8, $0141, $00A4, $0104, $00A6, $00A7, $00A8, $00A9, $015E, $00AB, $00AC, $00AD, $00AE, $017B,
 $00B0, $00B1, $02DB, $0142, $00B4, $00B5, $00B6, $00B7, $00B8, $0105, $015F, $00BB, $013D, $02DD, $013E, $017C,
 $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7, $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
 $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7, $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
 $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7, $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
 $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7, $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9);

ICONV_CP1251_UCS2: array[0..127] of Word =
($0402, $0403, $201A, $0453, $201E, $2026, $2020, $2021, $20AC, $2030, $0409, $2039, $040A, $040C, $040B, $040F,
 $0452, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0000, $2122, $0459, $203A, $045A, $045C, $045B, $045F,
 $00A0, $040E, $045E, $0408, $00A4, $0490, $00A6, $00A7, $0401, $00A9, $0404, $00AB, $00AC, $00AD, $00AE, $0407,
 $00B0, $00B1, $0406, $0456, $0491, $00B5, $00B6, $00B7, $0451, $2116, $0454, $00BB, $0458, $0405, $0455, $0457,
 $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417, $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
 $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427, $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
 $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437, $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
 $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447, $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F);

ICONV_CP1252_UCS2: array[0..127] of Word =
($20AC, $0000, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $0160, $2039, $0152, $0000, $017D, $0000,
 $0000, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $0161, $203A, $0153, $0000, $017E, $0178,
 $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
 $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
 $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
 $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
 $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
 $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF);

ICONV_CP1253_UCS2: array[0..127] of Word =
($20AC, $0000, $201A, $0192, $201E, $2026, $2020, $2021, $0000, $2030, $0000, $2039, $0000, $0000, $0000, $0000,
 $0000, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0000, $2122, $0000, $203A, $0000, $0000, $0000, $0000,
 $00A0, $0385, $0386, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $0000, $00AB, $00AC, $00AD, $00AE, $2015,
 $00B0, $00B1, $00B2, $00B3, $0384, $00B5, $00B6, $00B7, $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
 $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397, $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
 $03A0, $03A1, $0000, $03A3, $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
 $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7, $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
 $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7, $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, $0000);

ICONV_CP1254_UCS2: array[0..127] of Word =
($20AC, $0000, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $0160, $2039, $0152, $0000, $0000, $0000,
 $0000, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $0161, $203A, $0153, $0000, $0000, $0178,
 $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
 $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
 $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
 $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
 $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
 $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF);

ICONV_CP1255_UCS2: array[0..127] of Word =
($20AC, $0000, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $0000, $2039, $0000, $0000, $0000, $0000,
 $0000, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $0000, $203A, $0000, $0000, $0000, $0000,
 $00A0, $00A1, $00A2, $00A3, $20AA, $00A5, $00A6, $00A7, $00A8, $00A9, $00D7, $00AB, $00AC, $00AD, $00AE, $00AF,
 $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00F7, $00BB, $00BC, $00BD, $00BE, $00BF,
 $05B0, $05B1, $05B2, $05B3, $05B4, $05B5, $05B6, $05B7, $05B8, $05B9, $0000, $05BB, $05BC, $05BD, $05BE, $05BF,
 $05C0, $05C1, $05C2, $05C3, $05F0, $05F1, $0000, $05F2, $05F4, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
 $0000, $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $0000, $05D8, $05D9, $05DA, $05DB, $05DC, $0000, $05DE,
 $0000, $05E0, $05E1, $0000, $05E3, $05E4, $0000, $05E6, $05E7, $05E8, $05E9, $0000, $0000, $05EA, $200F, $0000);

ICONV_CP1256_UCS2: array[0..127] of Word =
($20AC, $067E, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $0679, $2039, $0152, $0686, $0698, $0688,
 $06AF, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $06A9, $2122, $0691, $203A, $0153, $200C, $200D, $06BA,
 $00A0, $060C, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $06BE, $00AB, $00AC, $00AD, $00AE, $00AF,
 $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $061B, $00BB, $00BC, $00BD, $00BE, $061F,
 $06C1, $0621, $0622, $0623, $0624, $0625, $0626, $0627, $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
 $0630, $0631, $0632, $0633, $0634, $0635, $0636, $00D7, $0637, $0638, $0639, $063A, $0640, $0641, $0642, $0643,
 $00E0, $0644, $00E2, $0645, $0646, $0647, $0648, $00E7, $00E8, $00E9, $00EA, $00EB, $0649, $064A, $00EE, $00EF,
 $064B, $064C, $064D, $064E, $00F4, $064F, $0650, $00F7, $0651, $00F9, $0652, $00FB, $00FC, $200E, $200F, $06D2);

ICONV_CP1257_UCS2: array[0..127] of Word =
($20AC, $0000, $201A, $0000, $201E, $2026, $2020, $2021, $0000, $2030, $0000, $2039, $0000, $00A8, $02C7, $00B8,
 $0000, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $0000, $2122, $0000, $203A, $0000, $00AF, $02DB, $0000,
 $00A0, $0000, $00A2, $00A3, $00A4, $0000, $00A6, $00A7, $00D8, $00A9, $0156, $00AB, $00AC, $00AD, $00AE, $00C6,
 $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00F8, $00B9, $0157, $00BB, $00BC, $00BD, $00BE, $00E6,
 $0104, $012E, $0100, $0106, $00C4, $00C5, $0118, $0112, $010C, $00C9, $0179, $0116, $0122, $0136, $012A, $013B,
 $0160, $0143, $0145, $00D3, $014C, $00D5, $00D6, $00D7, $0172, $0141, $015A, $016A, $00DC, $017B, $017D, $00DF,
 $0105, $012F, $0101, $0107, $00E4, $00E5, $0119, $0113, $010D, $00E9, $017A, $0117, $0123, $0137, $012B, $013C,
 $0161, $0144, $0146, $00F3, $014D, $00F5, $00F6, $00F7, $0173, $0142, $015B, $016B, $00FC, $017C, $017E, $02D9);

ICONV_CP1258_UCS2: array[0..127] of Word =
($20AC, $0000, $201A, $0192, $201E, $2026, $2020, $2021, $02C6, $2030, $0000, $2039, $0152, $0000, $0000, $0000,
 $0000, $2018, $2019, $201C, $201D, $2022, $2013, $2014, $02DC, $2122, $0000, $203A, $0153, $0000, $0000, $0178,
 $00A0, $00A1, $00A2, $00A3, $00A4, $0000, $00A5, $00A7, $0000, $00A8, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
 $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
 $00C0, $00C1, $0000, $00C2, $0102, $0000, $00C5, $00C6, $00C7, $00C9, $0000, $00CA, $0300, $00CD, $00CE, $0000,
 $00CF, $00D1, $0309, $0000, $00D3, $00D4, $01A0, $00D6, $0000, $00D8, $0000, $00DA, $0000, $00DC, $1EEE, $00DF,
 $00E0, $00E1, $0000, $00E2, $0103, $0000, $00E5, $00E6, $00E7, $00E9, $0000, $00EA, $0301, $00ED, $00EE, $0000,
 $00EF, $00F1, $0323, $0000, $00F3, $00F4, $01A1, $00F6, $0000, $00F8, $0000, $00FA, $0000, $00FC, $01B0, $00FF);

ICONV_ISO_8859_1_UCS2: array[0..127] of Word =
($0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
 $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
 $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
 $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
 $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
 $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
 $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
 $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF);

ICONV_ISO_8859_2_UCS2: array[0..127] of Word =
($0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
 $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
 $00A0, $0104, $02D8, $0141, $00A4, $013D, $015A, $00A7, $00A8, $0160, $015E, $0164, $0179, $00AD, $017D, $017B,
 $00B0, $0105, $02DB, $0142, $00B4, $013E, $015B, $02C7, $00B8, $0161, $015F, $0165, $017A, $02DD, $017E, $017C,
 $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7, $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
 $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7, $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
 $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7, $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
 $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7, $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9);

ICONV_ISO_8859_3_UCS2: array[0..127] of Word =
($0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
 $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
 $00A0, $0126, $02D8, $00A3, $00A4, $0000, $0124, $00A7, $00A8, $0130, $015E, $011E, $0134, $00AD, $0000, $017B,
 $00B0, $0127, $00B2, $00B3, $00B4, $00B5, $0125, $00B7, $00B8, $0131, $015F, $011F, $0135, $00BD, $0000, $017C,
 $00C0, $00C1, $00C2, $0000, $00C4, $010A, $0108, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
 $0000, $00D1, $00D2, $00D3, $00D4, $0120, $00D6, $00D7, $011C, $00D9, $00DA, $00DB, $00DC, $016C, $015C, $00DF,
 $00E0, $00E1, $00E2, $0000, $00E4, $010B, $0109, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
 $0000, $00F1, $00F2, $00F3, $00F4, $0121, $00F6, $00F7, $011D, $00F9, $00FA, $00FB, $00FC, $016D, $015D, $02D9);

ICONV_ISO_8859_4_UCS2: array[0..127] of Word =
($0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
 $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
 $00A0, $0104, $0138, $0156, $00A4, $0128, $013B, $00A7, $00A8, $0160, $0112, $0122, $0166, $00AD, $017D, $00AF,
 $00B0, $0105, $02DB, $0157, $00B4, $0129, $013C, $02C7, $00B8, $0161, $0113, $0123, $0167, $014A, $017E, $014B,
 $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E, $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $012A,
 $0110, $0145, $014C, $0136, $00D4, $00D5, $00D6, $00D7, $00D8, $0172, $00DA, $00DB, $00DC, $0168, $016A, $00DF,
 $0101, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $012F, $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $012B,
 $0111, $0146, $014D, $0137, $00F4, $00F5, $00F6, $00F7, $00F8, $0173, $00FA, $00FB, $00FC, $0169, $016B, $02D9);

ICONV_ISO_8859_5_UCS2: array[0..127] of Word =
($0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
 $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
 $00A0, $0401, $0402, $0403, $0404, $0405, $0406, $0407, $0408, $0409, $040A, $040B, $040C, $00AD, $040E, $040F,
 $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417, $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
 $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427, $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
 $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437, $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
 $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447, $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
 $2116, $0451, $0452, $0453, $0454, $0455, $0456, $0457, $0458, $0459, $045A, $045B, $045C, $00A7, $045E, $045F);

ICONV_ISO_8859_6_UCS2: array[0..127] of Word =
($0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
 $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
 $00A0, $0000, $0000, $0000, $00A4, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $060C, $00AD, $0000, $0000,
 $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $061B, $0000, $0000, $0000, $061F,
 $0000, $0621, $0622, $0623, $0624, $0625, $0626, $0627, $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
 $0630, $0631, $0632, $0633, $0634, $0635, $0636, $0637, $0638, $0639, $063A, $0000, $0000, $0000, $0000, $0000,
 $0640, $0641, $0642, $0643, $0644, $0645, $0646, $0647, $0648, $0649, $064A, $064B, $064C, $064D, $064E, $064F,
 $0650, $0651, $0652, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000);

ICONV_ISO_8859_7_UCS2: array[0..127] of Word =
($0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
 $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
 $00A0, $2018, $2019, $00A3, $20AC, $20AF, $00A6, $00A7, $00A8, $00A9, $037A, $00AB, $00AC, $00AD, $0000, $2015,
 $00B0, $00B1, $00B2, $00B3, $0384, $0385, $0386, $00B7, $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
 $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397, $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
 $03A0, $03A1, $0000, $03A3, $03A4, $03A5, $03A6, $03A7, $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
 $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7, $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
 $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7, $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, $0000);

ICONV_ISO_8859_8_UCS2: array[0..127] of Word =
($0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
 $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
 $00A0, $0000, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00D7, $00AB, $00AC, $00AD, $00AE, $00AF,
 $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00F7, $00BB, $00BC, $00BD, $00BE, $0000,
 $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
 $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000, $2017,
 $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7, $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
 $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7, $05E8, $05E9, $05EA, $0000, $0000, $200E, $200F, $0000);

ICONV_ISO_8859_9_UCS2: array[0..127] of Word =
($0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087, $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
 $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097, $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
 $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7, $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
 $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7, $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
 $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7, $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
 $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7, $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
 $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7, $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
 $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7, $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF);

{ TsanXML_KOI8R_Encoder }

function TsanXML_KOI8R_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_KOI8_R_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_KOI8U_Encoder }

function TsanXML_KOI8U_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_KOI8_U_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_CP1250_Encoder }

function TsanXML_CP1250_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_CP1250_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_CP1251_Encoder }

function TsanXML_CP1251_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_CP1251_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_CP1252_Encoder }

function TsanXML_CP1252_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_CP1252_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_CP1253_Encoder }

function TsanXML_CP1253_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_CP1253_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_CP1254_Encoder }

function TsanXML_CP1254_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_CP1254_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_CP1255_Encoder }

function TsanXML_CP1255_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_CP1255_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_CP1256_Encoder }

function TsanXML_CP1256_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_CP1256_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_CP1257_Encoder }

function TsanXML_CP1257_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_CP1257_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_CP1258_Encoder }

function TsanXML_CP1258_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_CP1258_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_ISO_8859_1_Encoder }

function TsanXML_ISO_8859_1_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_ISO_8859_1_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_ISO_8859_2_Encoder }

function TsanXML_ISO_8859_2_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_ISO_8859_2_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_ISO_8859_3_Encoder }

function TsanXML_ISO_8859_3_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_ISO_8859_3_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_ISO_8859_4_Encoder }

function TsanXML_ISO_8859_4_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_ISO_8859_4_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_ISO_8859_5_Encoder }

function TsanXML_ISO_8859_5_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_ISO_8859_5_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_ISO_8859_6_Encoder }

function TsanXML_ISO_8859_6_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_ISO_8859_6_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_ISO_8859_7_Encoder }

function TsanXML_ISO_8859_7_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_ISO_8859_7_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_ISO_8859_8_Encoder }

function TsanXML_ISO_8859_8_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_ISO_8859_8_UCS2[Byte(C) and $7F]);
  end;
end;

{ TsanXML_ISO_8859_9_Encoder }

function TsanXML_ISO_8859_9_Encoder.AnsiToWide(C: AnsiChar): Char;
begin
  if (Byte(C) and $80) = 0 then begin
    Result:= Char(C);
  end else begin
    Result:= Char(ICONV_ISO_8859_9_UCS2[Byte(C) and $7F]);
  end;
end;

initialization
  TsanXMLAnsiEncoder.RegisterEncoder('KOI8-R', TsanXML_KOI8R_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('KOI8-U', TsanXML_KOI8U_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('WINDOWS-1250', TsanXML_CP1250_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('WINDOWS-1251', TsanXML_CP1251_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('WINDOWS-1252', TsanXML_CP1252_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('WINDOWS-1253', TsanXML_CP1253_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('WINDOWS-1254', TsanXML_CP1254_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('WINDOWS-1255', TsanXML_CP1255_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('WINDOWS-1256', TsanXML_CP1256_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('WINDOWS-1257', TsanXML_CP1257_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('WINDOWS-1258', TsanXML_CP1258_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('ISO-8859-1', TsanXML_ISO_8859_1_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('ISO-8859-2', TsanXML_ISO_8859_2_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('ISO-8859-3', TsanXML_ISO_8859_3_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('ISO-8859-4', TsanXML_ISO_8859_4_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('ISO-8859-5', TsanXML_ISO_8859_5_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('ISO-8859-6', TsanXML_ISO_8859_6_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('ISO-8859-7', TsanXML_ISO_8859_7_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('ISO-8859-8', TsanXML_ISO_8859_8_Encoder);
  TsanXMLAnsiEncoder.RegisterEncoder('ISO-8859-9', TsanXML_ISO_8859_9_Encoder);
end.
