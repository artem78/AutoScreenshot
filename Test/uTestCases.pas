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
    procedure TestVersions;
  end;

implementation

uses uUtils, {SysUtils} DateUtils, uUtilsMore;

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

procedure TMyTestCase.TestVersions;
var
  V1, V2: TProgramVersion;
begin
  with V1 do
  begin
    Major := 5;
    Minor := 12;
    Revision := 101;
    Build := 54912;
  end;

  with V2 do
  begin
    Major := 3;
    Minor := 0;
    Revision := 2;
    Build := 0;
  end;

  // Test equal / not equal
  AssertTrue(V1 = TProgramVersion.Create(5, 12, 101, 54912));
  AssertFalse(V1 = V2);
  AssertTrue(V1 <> V2);
  AssertTrue(V2 = TProgramVersion.Create(3, 0, 2));
  AssertTrue(V1 = TProgramVersion.Create('5.12.101.54912'));
  AssertTrue(V1 <> TProgramVersion.Create('5.12.101'));
  AssertTrue(V2 = TProgramVersion.Create('v3.0.2'));
  AssertTrue(V2 = TProgramVersion.Create('V3.0.2.0'));
  AssertTrue(V2 <> TProgramVersion.Create('v3.02'));

  // Test more / less
  AssertTrue(V1 > V2);
  AssertTrue(V2 < V1);
  AssertTrue(V2 < TProgramVersion.Create(3, 1));
  AssertTrue(V2 > TProgramVersion.Create(3, 0, 1));
  AssertTrue(V2 < TProgramVersion.Create(3, 0, 2, 55));
  AssertTrue(V1 < TProgramVersion.Create(5, 12, 101, 54913));
  AssertTrue(V1 > TProgramVersion.Create(5, 12, 101, 54911));
  AssertFalse(V1 > V1);

  // Test to string conversion
  AssertEquals('12.345.67.8', TProgramVersion.Create(12, 345, 67, 8).ToString(False));
  AssertEquals('12.345.67.0', TProgramVersion.Create(12, 345, 67, 0).ToString(False));
  AssertEquals('12.345.0.0',  TProgramVersion.Create(12, 345, 0, 0) .ToString(False));
  AssertEquals('12.0.0.0',    TProgramVersion.Create(12, 0, 0, 0)   .ToString(False));
  AssertEquals('12.0.0.8',    TProgramVersion.Create(12, 0, 0, 8)   .ToString(False));
  AssertEquals('0.0.0.8',     TProgramVersion.Create(0, 0, 0, 8)    .ToString(False));
  AssertEquals('0.345.0.0',   TProgramVersion.Create(0, 345, 0, 0)  .ToString(False));
  AssertEquals('0.0.0.0',     TProgramVersion.Create(0, 0, 0, 0)    .ToString(False));

  AssertEquals('12.345.67.8', TProgramVersion.Create(12, 345, 67, 8).ToString(True));
  AssertEquals('12.345.67',   TProgramVersion.Create(12, 345, 67, 0).ToString(True));
  AssertEquals('12.345',      TProgramVersion.Create(12, 345, 0, 0) .ToString(True));
  AssertEquals('12',          TProgramVersion.Create(12, 0, 0, 0)   .ToString(True));
  AssertEquals('12.0.0.8',    TProgramVersion.Create(12, 0, 0, 8)   .ToString(True));
  AssertEquals('0.0.0.8',     TProgramVersion.Create(0, 0, 0, 8)    .ToString(True));
  AssertEquals('0.345',       TProgramVersion.Create(0, 345, 0, 0)  .ToString(True));
  //AssertEquals({''} '0',           TProgramVersion.Create(0, 0, 0, 0)    .ToString(True));

  // ToDo: Check wrong version strings
end;

initialization

  RegisterTest(TMyTestCase);
end.

