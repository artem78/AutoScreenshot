unit uTestCases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TMyTestCase }

  TMyTestCase= class(TTestCase)
  private
    function GetFormattedPath(Path: String): String;
  published
    procedure TestFormatPath;
    procedure TestInt2Str;
    procedure TestDecode;
    procedure TestJoinPath;
    procedure TestRemoveExtraPathDelimiters;
  end;

implementation

uses uUtils, {SysUtils} DateUtils;

{ TMyTestCase }

function TMyTestCase.GetFormattedPath(Path: String): String;
var
  DateTime: TDateTime;
begin
  DateTime := EncodeDateTime(2020, 4, 19, 12, 34, 56, 789);
  Result := FormatDateTime2(Path, DateTime);
end;

procedure TMyTestCase.TestDecode;
begin
  AssertEquals('This is' + #13 + #10 + 'multiline' + #13 + #10 + 'text',
        DecodeControlCharacters('This is\r\nmultiline\r\ntext'));
  AssertEquals('lorem' + #9 + 'ipsum' + #13 + #10 + 'dolor sit\amet' + #13 + #10,
        DecodeControlCharacters('lorem\tipsum\r\ndolor sit\\amet\r\n'));
  AssertEquals('', DecodeControlCharacters(''));
  AssertEquals('blablabla', DecodeControlCharacters('blablabla'));
end;

procedure TMyTestCase.TestFormatPath;
{var
  CompName, UserName: String;}
begin
  AssertEquals('19-04-2020', GetFormattedPath('%D-%M-%Y'));
  AssertEquals('2020_04_19', GetFormattedPath('%Y_%M_%D'));
  AssertEquals('12.34.56', GetFormattedPath('%H.%N.%S'));
  if '12.34.56' = GetFormattedPath('%H.%M.%S') then
    Fail('Month and minute mixed up');
  AssertEquals('No format variables',
      'screenshot123', GetFormattedPath('screenshot123'));
  AssertEquals('Complex test',
      '2020\04-19\screenshot_20200419_123456.png',
      GetFormattedPath('%Y\%M-%D\screenshot_%Y%M%D_%H%N%S.png'));
  AssertEquals('Non ASCII symbols',
      'скриншот 123456.jpeg',
      GetFormattedPath('скриншот %H%N%S.jpeg'));
  AssertEquals('20200419_123456/123456-19042020', GetFormattedPath('%Y%M%D_%H%N%S/%H%N%S-%D%M%Y'));

  // ToDo: Test incorect strings

  {// Note: On your machine this values will be different
  CompName := 'PC';
  UserName := 'Artem';
  AssertEquals('PC and user name',
      CompName + '_' + UserName + '_image.tiff',
      GetFormattedPath('%COMP_%USER_image.tiff'));}
end;

procedure TMyTestCase.TestInt2Str;
begin
  AssertEquals('152', Int2Str(152));
  AssertEquals('123456789', Int2Str(123456789));
  AssertEquals('-200300', Int2Str(-200300));
  AssertEquals('007', Int2Str(7, 3));
  AssertEquals('-00121', Int2Str(-121, 5));
  AssertEquals('0', Int2Str(0));
  AssertEquals('000000', Int2Str(0, 6));
  AssertEquals('303', Int2Str(303, 3));
  AssertEquals('-505', Int2Str(-505, 3));
end;

procedure TMyTestCase.TestJoinPath;
begin
  //CheckEqualsString('', JoinPath('', ''));
  {$IFDEF MSWINDOWS}
  AssertEquals('c:\root\subdir\', JoinPath('c:\root', 'subdir\'));
  AssertEquals('Path starts with backslash',
    'd:\folder1\folder2\folder3\folder4\',
    JoinPath('d:\folder1\folder2\', '\folder3\folder4\'));
  AssertEquals('Empty path', 'a:\DisketDir\', JoinPath('a:\DisketDir', ''));
  AssertEquals('Relative path',
      'mydir\picture.jpeg', JoinPath('mydir', 'picture.jpeg'));
  {$ENDIF}
end;

procedure TMyTestCase.TestRemoveExtraPathDelimiters;
begin
  //CheckEqualsString('', RemoveExtraPathDelimiters(''));
  {$IFDEF MSWINDOWS}
  AssertEquals('c:\dir1\dir2\dir3\file.txt', RemoveExtraPathDelimiters('c:\dir1\\dir2\\\dir3\\\\file.txt'));
  AssertEquals('folder\', RemoveExtraPathDelimiters('folder\\'));
  AssertEquals('\my folder\', RemoveExtraPathDelimiters('\\my folder\\'));
  AssertEquals('Nothing to do', 'd:\dir\file.txt', RemoveExtraPathDelimiters('d:\dir\file.txt'));
  {$ENDIF}
end;

initialization

  RegisterTest(TMyTestCase);
end.

