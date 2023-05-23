unit ScreenGrabber;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZStream { for Tcompressionlevel };

type
  TImageFormat = (fmtPNG=0, fmtJPG, fmtBMP{, fmtGIF}, fmtTIFF);

  TColorDepth = (cd8Bit=8, cd16Bit=16, cd24Bit=24, cd32Bit=32);

  TImageFormatInfo = record
    Name: String[10];
    Extension: String[3];
    HasQuality: Boolean;
    HasGrayscale: Boolean;
    ColorDepth: Set of TColorDepth;
    HasCompressionLevel: Boolean;
  end;

  TImageFormatInfoArray = array [TImageFormat] of TImageFormatInfo;

  { TScreenGrabber }

  TScreenGrabber = class
  private
    procedure CaptureRegion(AFileName: String; ARect: TRect);

  public
    ImageFormat: TImageFormat;
    ColorDepth: TColorDepth;
    Quality: Integer;
    IsGrayscale: Boolean;
    CompressionLevel: Tcompressionlevel;

    constructor Create(AnImageFormat: TImageFormat; AColorDepth: TColorDepth;
      AnQuality: Integer; AnIsGrayscale: Boolean; ACompressionLevel: Tcompressionlevel);

    procedure CaptureMonitor(AFileName: String; AMonitorId: Integer);
    procedure CaptureAllMonitors(AFileName: String);
  end;

const
  ImageFormatInfoArray: TImageFormatInfoArray = (
    (
      Name:         'PNG';
      Extension:    'png';
      HasQuality:   False;
      HasGrayscale: True;
      ColorDepth:   [{cd8Bit, cd16Bit, cd24Bit, cd32Bit}];
      HasCompressionLevel: True
    ),
    (
      Name:         'JPG';
      Extension:    'jpg';
      HasQuality:   True;
      HasGrayscale: True;
      ColorDepth:   [];
      HasCompressionLevel: False
    ),
    (
      Name:         'BMP';
      Extension:    'bmp';
      HasQuality:   False;
      HasGrayscale: False;
      ColorDepth:   [{cd8Bit,} cd16Bit, cd24Bit, cd32Bit];
      HasCompressionLevel: False
    ){,
    (
      Name:         'GIF';
      Extension:    'gif';
      HasQuality:   False;
      HasGrayscale: False;
      ColorDepth:   [];
      HasCompressionLevel: False
    )},
    (
      Name:         'TIFF';
      Extension:    'tif';
      HasQuality:   False;
      HasGrayscale: False;
      ColorDepth:   [];
      HasCompressionLevel: False
    )
  );


implementation

uses
  {$IfDef Windows}
  windows,
  {$EndIf}
  Forms {for TMonitor}, LCLType, LCLIntf, BGRABitmap, BGRABitmapTypes, FPWriteJPEG, FPWriteBMP,
  FPWritePNG, FPImage, FPWriteTiff, LazLoggerBase;

{ TScreenGrabber }

procedure TScreenGrabber.CaptureMonitor(AFileName: String; AMonitorId: Integer);
var
  Rect: TRect;
  UsedMonitor: TMonitor;
begin
  DebugLn(['Monitor id=', AMonitorId]);

  UsedMonitor := Screen.Monitors[AMonitorId];
  Rect.Left   := UsedMonitor.Left;
  Rect.Top    := UsedMonitor.Top;
  Rect.Width  := UsedMonitor.Width;
  Rect.Height := UsedMonitor.Height;
  CaptureRegion(AFileName, Rect);
end;

procedure TScreenGrabber.CaptureAllMonitors(AFileName: String);
var
  Rect: TRect;
begin
  Rect.Left   := GetSystemMetrics(SM_XVIRTUALSCREEN);
  Rect.Top    := GetSystemMetrics(SM_YVIRTUALSCREEN);
  Rect.Width  := GetSystemMetrics(SM_CXVIRTUALSCREEN);
  Rect.Height := GetSystemMetrics(SM_CYVIRTUALSCREEN);
  CaptureRegion(AFileName, Rect);
end;

procedure TScreenGrabber.CaptureRegion(AFileName: String; ARect: TRect);
{$IfDef Linux}
const
  HWND_DESKTOP = 0;
{$EndIf}
var
  Bitmap: TBGRABitmap;
  Writer: TFPCustomImageWriter;
  //GIF: TGIFImage;
  ScreenDC: {$IfDef Windows}Windows.{$EndIf}HDC;
begin
  DebugLn('Start taking screenshot...');
  DebugLn('Region: ', DbgS(ARect));

  Bitmap := TBGRABitmap.Create(ARect.Width, ARect.Height, BGRABlack);

  //Bitmap.TakeScreenshot(Rect); // Not supports multiply monitors
  ScreenDC := GetDC(HWND_DESKTOP); // Get DC for all monitors
  DebugLn('ScreenDC=', DbgS(ScreenDC));
  {BitBlt(Bitmap.Canvas.Handle, 0, 0, ARect.Width, ARect.Height,
           ScreenDC, ARect.Left, ARect.Top, SRCCOPY);}
  Bitmap.LoadFromDevice(ScreenDC, ARect);
  ReleaseDC(0, ScreenDC);

  case ImageFormat of
    fmtPNG:      // PNG
      begin
        Writer := TFPWriterPNG.create;

        with Writer as TFPWriterPNG do
        begin
          GrayScale := IsGrayscale;
          CompressionLevel := Self.CompressionLevel;
          //Indexed := ...;
          //UseAlpha := ...;
        end;
      end;

    fmtJPG:     // JPEG
      begin
        Writer := TFPWriterJPEG.Create;

        with Writer as TFPWriterJPEG do
        begin
          CompressionQuality := Quality;
          GrayScale := IsGrayscale;
        end;
      end;

    fmtBMP:    // Bitmap (BMP)
      begin
        Writer := TFPWriterBMP.Create;

        with Writer as TFPWriterBMP do
        begin
          BitsPerPixel := Integer(ColorDepth);
          //RLECompress := ...;
        end;
      end;

    {fmtGIF:    // GIF
      begin
        GIF := TGIFImage.Create;
        try
          GIF.Assign(Bitmap);
          //GIF.OptimizeColorMap;
          GIF.SaveToFile(AFileName);
        finally
          GIF.Free;
        end;
      end;}

      fmtTIFF:
        begin
          Writer := TFPWriterTiff.Create;
        end;
  end;

  try
    try
      Bitmap.SaveToFile(AFileName, Writer);
    except
      on E : Exception do
      begin
        DebugLn('Failed to take screenshot: ', E.ToString);
        raise e;
      end;
    end;
  finally
    Writer.Free;
    Bitmap.Free;
  end;

  DebugLn('Screenshot saved to ', AFileName);
end;

constructor TScreenGrabber.Create(AnImageFormat: TImageFormat;
  AColorDepth: TColorDepth; AnQuality: Integer; AnIsGrayscale: Boolean;
  ACompressionLevel: Tcompressionlevel);
begin
  inherited Create();

  ImageFormat := AnImageFormat;
  ColorDepth := AColorDepth;
  Quality := AnQuality;
  IsGrayscale := AnIsGrayscale;
  CompressionLevel := ACompressionLevel;
end;

end.

