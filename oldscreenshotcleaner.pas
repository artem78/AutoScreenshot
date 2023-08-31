unit OldScreenshotCleaner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TIntervalUnit = (iuHours, iuDays, iuWeeks, iuMonths);

  TInterval = record
    Val: Cardinal;
    Unit_: TIntervalUnit;
  end;

  TOldScreenshotCleanerChangeCallback = procedure of object;

  { TOldScreenshotCleaner }

  TOldScreenshotCleaner = class
  private
    FInterval: TInterval;
    FActive: Boolean;
    FOnChangeCallback: TOldScreenshotCleanerChangeCallback;

    procedure SetInterval(AInterval: TInterval);
    procedure SetActive(AActive: Boolean);
  public

    property Interval: TInterval read FInterval write SetInterval;
    property Active: Boolean read FActive write SetActive;
    property OnChangeCallback: TOldScreenshotCleanerChangeCallback
               read FOnChangeCallback write FOnChangeCallback;
  end;

  function IntervalUnitToString(AValue: TIntervalUnit): String;
  function StringToIntervalUnit(AValue: String): TIntervalUnit;

implementation

uses TypInfo;

function IntervalUnitToString(AValue: TIntervalUnit): String;
begin
  Result := GetEnumName(TypeInfo(TIntervalUnit), Ord(AValue));
  if Result.StartsWith('iu') then
    Result := Result.Remove(0, 2);
end;

function StringToIntervalUnit(AValue: String): TIntervalUnit;
begin
  Result := TIntervalUnit(GetEnumValue(TypeInfo(TIntervalUnit), 'iu' + AValue));
end;

{ TOldScreenshotCleaner }

procedure TOldScreenshotCleaner.SetInterval(AInterval: TInterval);
begin
  //if (FInterval.Unit_ = AInterval.Unit_) and (FInterval.Val = AInterval.Val) then
  //  Exit;

  FInterval := AInterval;

  if Assigned(FOnChangeCallback) then
    FOnChangeCallback;
end;

procedure TOldScreenshotCleaner.SetActive(AActive: Boolean);
begin
  //if FActive = AActive then
  //  Exit;

  FActive := AActive;

  if Assigned(FOnChangeCallback) then
    FOnChangeCallback;
end;

end.

