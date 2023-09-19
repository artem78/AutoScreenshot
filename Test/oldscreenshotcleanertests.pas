unit OldScreenshotCleanerTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TOldScreenshotCleanerTestCase }

  TOldScreenshotCleanerTestCase= class(TTestCase)
  published
    procedure TestIntervalToStr;
    procedure TestStrToInterval;
  private
    procedure TryParseStr1; // Empty string
    procedure TryParseStr2; // Wrong string
    procedure TryParseStr3; // Without unit
    procedure TryParseStr4; // Wrong unit
    procedure TryParseStr5; // Without value
    procedure TryParseStr6; // Wrong unit again
  end;

implementation

uses
  OldScreenshotCleaner;

procedure TOldScreenshotCleanerTestCase.TestIntervalToStr;
var
  I: TInterval;
begin
  I.Val := 5;
  I.Unit_ := iuHours;
  AssertEquals('5h', String(I));

  I.Val := 200;
  I.Unit_ := iuDays;
  AssertEquals('200d', String(I));

  I.Val := 11;
  I.Unit_ := iuWeeks;
  AssertEquals('11w', String(I));

  I.Val := 35;
  I.Unit_ := iuMonths;
  AssertEquals('35m', String(I));
end;

procedure TOldScreenshotCleanerTestCase.TestStrToInterval;
var
  I: TInterval;
begin
  I := TInterval('48h');
  AssertEquals(48, I.Val);
  AssertTrue(I.Unit_ = iuHours);

  I := TInterval('100d');
  AssertEquals(100, I.Val);
  AssertTrue(I.Unit_ = iuDays);

  I := TInterval('3w');
  AssertEquals(3, I.Val);
  AssertTrue(I.Unit_ = iuWeeks);

  I := TInterval('12m');
  AssertEquals(12, I.Val);
  AssertTrue(I.Unit_ = iuMonths);

  I := TInterval('00101d'); // Leading zeroes
  AssertEquals(101, I.Val);
  AssertTrue(I.Unit_ = iuDays);


  // Some wrong values
  AssertException(Exception, @TryParseStr1);
  AssertException(Exception, @TryParseStr2);
  AssertException(Exception, @TryParseStr3);
  AssertException(Exception, @TryParseStr4);
  AssertException({Exception} EConvertError, @TryParseStr5);
  AssertException({Exception} EConvertError, @TryParseStr6);
end;

procedure TOldScreenshotCleanerTestCase.TryParseStr1;
var
  I: TInterval;
begin
  I := TInterval('');
end;

procedure TOldScreenshotCleanerTestCase.TryParseStr2;
var
  I: TInterval;
begin
  I := TInterval('???');
end;

procedure TOldScreenshotCleanerTestCase.TryParseStr3;
var
  I: TInterval;
begin
  I := TInterval('405');
end;

procedure TOldScreenshotCleanerTestCase.TryParseStr4;
var
  I: TInterval;
begin
  I := TInterval('122X');
end;

procedure TOldScreenshotCleanerTestCase.TryParseStr5;
var
  I: TInterval;
begin
  I := TInterval('m');
end;

procedure TOldScreenshotCleanerTestCase.TryParseStr6;
var
  I: TInterval;
begin
  I := TInterval('155km');
end;

initialization

  RegisterTest(TOldScreenshotCleanerTestCase);
end.

