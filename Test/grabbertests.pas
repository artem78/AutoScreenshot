unit GrabberTests;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TGrabberTestCase }

  TGrabberTestCase = class(TTestCase)
  protected
    procedure SetUp; override;
  private
    OutputDir: string;
    procedure AssertBetween(AMinExpected, AMaxExpected, AActual: Integer);
    procedure AssertBetween(const AMsg: String; AMinExpected, AMaxExpected, AActual: Integer);
  published
    procedure TestBmp;
  public
    constructor Create; override;
  end;

implementation

uses ScreenGrabber, ZStream, Forms, BGRABitmap, FileUtil;

{ TGrabberTestCase }

procedure TGrabberTestCase.SetUp;
begin
  inherited SetUp;

  if not DirectoryExists(OutputDir) then
    CreateDir(OutputDir)
  else
    DeleteDirectory(OutputDir, True);
end;

procedure TGrabberTestCase.AssertBetween(AMinExpected, AMaxExpected,
  AActual: Integer);
begin
  AssertTrue(AActual >= AMinExpected);
  AssertTrue(AActual <= AMaxExpected);
end;

procedure TGrabberTestCase.AssertBetween(const AMsg: String; AMinExpected,
  AMaxExpected, AActual: Integer);
begin
  AssertTrue(AMsg, AActual >= AMinExpected);
  AssertTrue(AMsg, AActual <= AMaxExpected);
end;

procedure TGrabberTestCase.TestBmp;
var
  Grabber: TScreenGrabber;
  FileName: String;
  BitmapSize: Integer;
  ColorDepth: TColorDepth;
  Bitmap: TBGRABitmap;
begin
  FileName := ConcatPaths([OutputDir, 'image.bmp']);

  for ColorDepth in {TColorDepth} [cd16Bit, cd24Bit, cd32Bit] do
  begin
    Grabber := TScreenGrabber.Create(fmtBMP, ColorDepth, 100, False, Tcompressionlevel.clNone);
    try
      //Grabber.CaptureAllMonitors(FileName);
      Grabber.CaptureMonitor(FileName, 0);

      AssertTrue(FileExists(FileName));

      BitmapSize := (Ord(ColorDepth) div 8) * Screen.Monitors[0].Width * Screen.Monitors[0].Height;
      AssertBetween('Image size', BitmapSize, BitmapSize + 100 {headers, etc...}, FileSize(FileName));

      try
        Bitmap := TBGRABitmap.Create(FileName);
      finally
        AssertEquals('Image width', Screen.Monitors[0].Width, Bitmap.Width);
        AssertEquals('Image height', Screen.Monitors[0].Height, Bitmap.Height);
        //AssertEquals(Ord(ColorDepth), Bitmap.???); // Todo: make this

        Bitmap.Free;
      end;
    finally
      Grabber.Free;
    end;
  end;
end;

constructor TGrabberTestCase.Create;
begin
  inherited Create;

  OutputDir := ConcatPaths([ExtractFileDir(Application.ExeName), 'out']);
  OutputDir := IncludeTrailingPathDelimiter(OutputDir);
end;

initialization

  RegisterTest(TGrabberTestCase);

end.

