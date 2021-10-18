unit ScreenGrabber;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TImageFormat = (fmtPNG=0, fmtJPG, fmtBMP{, fmtGIF});

  TColorDepth = (cd8Bit=8, cd16Bit=16, cd24Bit=24, cd32Bit=32);

  TImageFormatInfo = record
    Name: String[10];
    Extension: String[3];
    HasQuality: Boolean;
    HasGrayscale: Boolean;
    ColorDepth: Set of TColorDepth;
  end;

  TImageFormatInfoArray = array [TImageFormat] of TImageFormatInfo;

  { TScreenGrabber }

  TScreenGrabber = class
  private
    procedure CaptureRegion(AFileName: String; ARect: TRect;
      AnImageFormat: TImageFormat; AColorDepth: TColorDepth;
      AnJPEGQuality: Integer; AnIsGrayscale: Boolean);
  public
    procedure CaptureMonitor(AFileName: String; AMonitorId: Integer;
      AnImageFormat: TImageFormat; AColorDepth: TColorDepth;
      AnJPEGQuality: Integer; AnIsGrayscale: Boolean);
    procedure CaptureAllMonitors(AFileName: String;
      AnImageFormat: TImageFormat; AColorDepth: TColorDepth;
      AnJPEGQuality: Integer; AnIsGrayscale: Boolean);
  end;

const
  ImageFormatInfoArray: TImageFormatInfoArray = (
    { FixMe: Unsupported modes by Free Pascal Graphics unit are commented.
      Think about use any third party graphics library instead
      (https://wiki.freepascal.org/Graphics_libraries). }
    (
      Name:         'PNG';
      Extension:    'png';
      HasQuality:   False;
      HasGrayscale: False;
      ColorDepth:   [{cd8Bit, cd16Bit, cd24Bit, cd32Bit}] // Only 24bit
    ),
    (
      Name:         'JPG';
      Extension:    'jpg';
      HasQuality:   True;
      HasGrayscale: {True} False;
      ColorDepth:   []
    ),
    (
      Name:         'BMP';
      Extension:    'bmp';
      HasQuality:   False;
      HasGrayscale: False;
      ColorDepth:   [{cd8Bit, cd16Bit,} cd24Bit, cd32Bit]
    ){,
    (
      Name:         'GIF';
      Extension:    'gif';
      HasQuality:   False;
      HasGrayscale: False;
      ColorDepth:   []
    )}
  );


implementation

uses
  Windows, Forms {for TMonitor}, Graphics;

{ TScreenGrabber }

procedure TScreenGrabber.CaptureMonitor(AFileName: String; AMonitorId: Integer;
  AnImageFormat: TImageFormat; AColorDepth: TColorDepth;
  AnJPEGQuality: Integer; AnIsGrayscale: Boolean);
var
  Rect: TRect;
  UsedMonitor: TMonitor;
begin
  UsedMonitor := Screen.Monitors[AMonitorId];
  Rect.Left   := UsedMonitor.Left;
  Rect.Top    := UsedMonitor.Top;
  Rect.Width  := UsedMonitor.Width;
  Rect.Height := UsedMonitor.Height;
  CaptureRegion(AFileName, Rect, AnImageFormat,
    AColorDepth, AnJPEGQuality, AnIsGrayscale);
end;

procedure TScreenGrabber.CaptureAllMonitors(AFileName: String;
  AnImageFormat: TImageFormat; AColorDepth: TColorDepth;
  AnJPEGQuality: Integer; AnIsGrayscale: Boolean);
var
  Rect: TRect;
begin
  Rect.Left   := GetSystemMetrics(SM_XVIRTUALSCREEN);
  Rect.Top    := GetSystemMetrics(SM_YVIRTUALSCREEN);
  Rect.Width  := GetSystemMetrics(SM_CXVIRTUALSCREEN);
  Rect.Height := GetSystemMetrics(SM_CYVIRTUALSCREEN);
  CaptureRegion(AFileName, Rect, AnImageFormat,
    AColorDepth, AnJPEGQuality, AnIsGrayscale);
end;

procedure TScreenGrabber.CaptureRegion(AFileName: String;
  ARect: TRect; AnImageFormat: TImageFormat; AColorDepth: TColorDepth;
  AnJPEGQuality: Integer; AnIsGrayscale: Boolean);
var
  Bitmap: TBitmap;
  PNG: TPortableNetworkGraphic;
  JPG: TJPEGImage;
  //GIF: TGIFImage;
  ScreenDC: HDC;
begin
  Bitmap := TBitmap.Create;

  // Set color depth for bitmap
  try
    case Integer(AColorDepth) of
      1:  Bitmap.PixelFormat := pf1bit;
      4:  Bitmap.PixelFormat := pf4bit;
      8:  Bitmap.PixelFormat := pf8bit;
      16: Bitmap.PixelFormat := pf16bit;
      24: Bitmap.PixelFormat := pf24bit;
      32: Bitmap.PixelFormat := pf32bit;
      //else raise Exception.CreateFmt('Color depth %d bit not supported in TBitmap', [Integer(ColorDepth)]);
    end;
  except
    // Leave bitmap pixel format as default
  end;

  Bitmap.Width := ARect.Width;
  Bitmap.Height := ARect.Height;
  Bitmap.Canvas.Brush.Color := clBlack;
  Bitmap.Canvas.FillRect(Classes.Rect(0, 0, ARect.Width, ARect.Height));
  ScreenDC := GetDC(HWND_DESKTOP); // Get DC for all monitors
  BitBlt(Bitmap.Canvas.Handle, 0, 0, ARect.Width, ARect.Height,
           ScreenDC, ARect.Left, ARect.Top, SRCCOPY);
  ReleaseDC(0, ScreenDC);

  try
    case AnImageFormat of
      fmtPNG:      // PNG
        begin
          PNG := TPortableNetworkGraphic.Create;
          try
            PNG.Assign(Bitmap);
            PNG.SaveToFile(AFileName);
          finally
            PNG.Free;
          end;
        end;

      fmtJPG:     // JPEG
        begin
          JPG := TJPEGImage.Create;
          try
            JPG.CompressionQuality := AnJPEGQuality;
            //JPG.GrayScale := AnIsGrayscale; FixMe: Can not set grayscale
            JPG.Assign(Bitmap);
            //JPG.Compress;
            JPG.SaveToFile(AFileName);
          finally
            JPG.Free;
          end;
        end;

      fmtBMP:    // Bitmap (BMP)
        begin
          Bitmap.SaveToFile(AFileName);
        end;

      {fmtGIF:    // GIF
        begin
          GIF := TGIFImage.Create;
          try
            GIF.Assign(Bitmap);
            //GIF.OptimizeColorMap;
            GIF.SaveToFile(FileName);
          finally
            GIF.Free;
          end;
        end;}
    end;
  finally
    Bitmap.Free;
  end;
end;

end.

