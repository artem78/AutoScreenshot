unit uTestCases;

interface

uses
  TestFramework;

type
  TMyTestCase = class(TTestCase)
  private
    function GetFormattedPath(Path: String): String;
  published
    procedure TestFormatPath;
    procedure TestInt2Str;
    procedure TestDecode;
  end;

implementation

uses uUtils, {SysUtils} DateUtils;

function TMyTestCase.GetFormattedPath(Path: String): String;
var
  DateTime: TDateTime;
begin
  DateTime := EncodeDateTime(2020, 4, 19, 12, 34, 56, 789);
  Result := FormatDateTime2(Path, DateTime);
end;

procedure TMyTestCase.TestDecode;
begin
  CheckEqualsString('This is' + #13 + #10 + 'multiline' + #13 + #10 + 'text',
        DecodeControlCharacters('This is\r\nmultiline\r\ntext'));
  CheckEqualsString('lorem' + #9 + 'ipsum' + #13 + #10 + 'dolor sit\amet' + #13 + #10,
        DecodeControlCharacters('lorem\tipsum\r\ndolor sit\\amet\r\n'));
  CheckEqualsString('', DecodeControlCharacters(''));
  CheckEqualsString('blablabla', DecodeControlCharacters('blablabla'));
  //CheckEqualsString('', Encode(''));
  //CheckEqualsString('', Encode(''));
end;

procedure TMyTestCase.TestFormatPath;
begin
  CheckEqualsString('19-04-2020', GetFormattedPath('%D-%M-%Y'));
  CheckEqualsString('2020_04_19', GetFormattedPath('%Y_%M_%D'));
  CheckEqualsString('12.34.56', GetFormattedPath('%H.%N.%S'));
  CheckNotEqualsString('12.34.56', GetFormattedPath('%H.%M.%S'),
      'Month and munute mixed up');
  CheckEquals('screenshot123', GetFormattedPath('screenshot123'),
      'No format variables');
  CheckEqualsString('2020\04-19\screenshot_20200419_123456.png',
      GetFormattedPath('%Y\%M-%D\screenshot_%Y%M%D_%H%N%S.png'),
      'Complex test');
  CheckEqualsString('скриншот 123456.jpeg',
      GetFormattedPath('скриншот %H%N%S.jpeg'),
      'Non ASCII symbols');
  CheckEqualsString('20200419_123456/123456-19042020', GetFormattedPath('%Y%M%D_%H%N%S/%H%N%S-%D%M%Y'))

  // ToDo: Test incorect strings
end;

procedure TMyTestCase.TestInt2Str;
begin
  CheckEqualsString('152', Int2Str(152));
  CheckEqualsString('123456789', Int2Str(123456789));
  CheckEqualsString('-200300', Int2Str(-200300));
  CheckEqualsString('007', Int2Str(7, 3));
  CheckEqualsString('-00121', Int2Str(-121, 5));
  CheckEqualsString('0', Int2Str(0));
  CheckEqualsString('000000', Int2Str(0, 6));
  CheckEqualsString('303', Int2Str(303, 3));
  CheckEqualsString('-505', Int2Str(-505, 3));
end;

initialization
  TestFramework.RegisterTest(TMyTestCase.Suite);
end.
