unit ScreenGrabber;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZStream { for Tcompressionlevel };

type
  TImageFormat = (fmtPNG=0, fmtJPG, fmtBMP{, fmtGIF}, fmtTIFF, fmtWEBP, fmtAVIF);

  TColorDepth = (cd8Bit=8, cd16Bit=16, cd24Bit=24, cd32Bit=32);

  TImageFormatInfo = record
    Name: String[10];
    Extension: String[4];
    HasQuality: Boolean;
    HasGrayscale: Boolean;
    ColorDepth: Set of TColorDepth;
    HasCompressionLevel: Boolean;
  end;

  TImageFormatInfoArray = array [TImageFormat] of TImageFormatInfo;

  { TScreenGrabber }

  TScreenGrabber = class
  private
    procedure CaptureRegion(AFileName: String; ARect: TRect; AIncludeCursor: boolean);

  public
    ImageFormat: TImageFormat;
    ColorDepth: TColorDepth;
    Quality: Integer;
    IsGrayscale: Boolean;
    CompressionLevel: Tcompressionlevel;

    constructor Create(AnImageFormat: TImageFormat; AColorDepth: TColorDepth;
      AnQuality: Integer; AnIsGrayscale: Boolean; ACompressionLevel: Tcompressionlevel);

    procedure CaptureMonitor(AFileName: String; AMonitorId: Integer;
      AIncludeCursor: boolean = False);
    procedure CaptureAllMonitors(AFileName: String;
      AIncludeCursor: boolean = False);
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
    ),
    (
      Name:         'WebP';
      Extension:    'webp';
      HasQuality:   False;
      HasGrayscale: False;
      ColorDepth:   [];
      HasCompressionLevel: False
    ),
    (
      Name:         'AVIF';
      Extension:    'avif';
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
  {$IfDef Linux}
  xlib,x,ctypes ,
  //mouse,
  controls {для mouse},
  {$EndIf}
  Forms, LCLType, LCLIntf, Graphics, BGRABitmap, BGRABitmapTypes, BGRAWriteWebP,
  BGRAWriteAvif, libavif, FPWriteJPEG, FPWriteBMP, FPWritePNG, FPImage,
  FPWriteTiff, LazLoggerBase;

{$IfDef Linux}
type
    PXFixesCursorImage = ^TXFixesCursorImage;
  TXFixesCursorImage = record
    x, y: cshort;
    width, height: cushort;
    xhot, yhot: cushort;
    cursor_serial: culong;
    pixels: Pculong;
    atom: TAtom;                    { Version >= 2 only }
    namer: PChar;                    { Version >= 2 only }
  end;
function XFixesGetCursorImage(dis:PDisplay):PXFixesCursorImage;cdecl;external 'libXfixes';
{$EndIf}



{$IfDef Windows}
{
   спизжено отсюда:
   https://forum.lazarus.freepascal.org/index.php/topic,37034.msg247634.html#msg247634
}


// 1. Get the handle to the current mouse-cursor and its position
function GetCursorInfo2: TCursorInfo;
var
 hWindow: HWND;
 pt: TPoint;
 pIconInfo: TIconInfo;
 dwThreadID, dwCurrentThreadID: DWORD;
begin
 Result.hCursor := 0;
 ZeroMemory(@Result, SizeOf(Result));
 // Find out which window owns the cursor
 if GetCursorPos(pt) then
 begin
   Result.ptScreenPos := pt;
   hWindow := WindowFromPoint(pt);
   if IsWindow(hWindow) then
   begin
     // Get the thread ID for the cursor owner.
     dwThreadID := GetWindowThreadProcessId(hWindow, nil);

     // Get the thread ID for the current thread
     dwCurrentThreadID := GetCurrentThreadId;

     // If the cursor owner is not us then we must attach to
     // the other thread in so that we can use GetCursor() to
     // return the correct hCursor
     if (dwCurrentThreadID <> dwThreadID) then
     begin
       if AttachThreadInput(dwCurrentThreadID, dwThreadID, True) then
       begin
         // Get the handle to the cursor
         Result.hCursor := GetCursor;
         AttachThreadInput(dwCurrentThreadID, dwThreadID, False)
;
       end;
     end
     else
     begin
       Result.hCursor := GetCursor;
     end;
   end;
 end;
end;

procedure DrawCursorOverBitmap(ABitmap: {TBITMAP} TBGRABitmap);
var
 //DC: HDC;
 //ABitmap: TBitmap;
 MyCursor: TIcon;
 CursorInfo: TCursorInfo;
 IconInfo: Windows.TIconInfo;
begin
 {// Capture the Desktop screen
 DC := GetDC(GetDesktopWindow);
 ABitmap := TBitmap.Create;
 try
   ABitmap.Width  := GetDeviceCaps(DC, HORZRES);
   ABitmap.Height := GetDeviceCaps(DC, VERTRES);
   // BitBlt on our bitmap
   BitBlt(ABitmap.Canvas.Handle,
     0,
     0,
     ABitmap.Width,
     ABitmap.Height,
     DC,
     0,
     0,
     SRCCOPY);      }
   // Create temp. Icon
   MyCursor := TIcon.Create;
   try
     // Retrieve Cursor info
     CursorInfo := GetCursorInfo2;
     DebugLn('CursorInfo.hCursor=', dbgs(CursorInfo.hCursor));
     if CursorInfo.hCursor <> 0 then
     begin
       MyCursor.Handle := CursorInfo.hCursor;
       // Get Hotspot information
       Windows.GetIconInfo(CursorInfo.hCursor, IconInfo);
       // Draw the Cursor on our bitmap
       ABitmap.Canvas.Draw(CursorInfo.ptScreenPos.X - IconInfo.xHotspot,
                           CursorInfo.ptScreenPos.Y - IconInfo.yHotspot, MyCursor);
     end
     else
       DebugLn('Failed to get cursor! (CursorInfo.hCursor = 0)');
   finally
     // Clean up
     MyCursor.ReleaseHandle;
     MyCursor.Free;
   end;
 {finally
   ReleaseDC(GetDesktopWindow, DC);
 end;    }
 //Result := ABitmap;
end;
{$EndIf}

{$IfDef Linux}
// Спиздил отсюда: https://forum.lazarus.freepascal.org/index.php/topic,37057.msg259462.html#msg259462
procedure DrawCursorOverBitmap(ABitmap: TBGRABitmap);
var  display:PDisplay;
hh:PXFixesCursorImage;
      gg,tt,gf:Integer;
      goog:TBitmap;
      vv:pbyte;


begin
  display :=XOpenDisplay(nil);
     goog:= TBitmap.Create;
     goog.Transparent:=true;
    hh:=XFixesGetCursorImage(display);


    goog.Width:=hh^.width;
    goog.Height:=hh^.height;
      gf:=0;
  for  gg := 0 to hh^.height-1 do
     for  tt := 0 to hh^.width-1 do
      begin
         vv:= @hh^.pixels[gf];
         goog.Canvas.Pixels[tt,gg]:=RGBToColor(vv[2],vv[1],vv[0]);
         Inc(gf);
        end;




   //Canvas.Draw(0,0,goog);
   abitmap.Canvas.draw(Mouse.CursorPos.X{getmousex},mouse.cursorpos.y{getmousey},goog);
    goog.Free;
     XCloseDisplay(display);


end;
{$EndIf}

{ TScreenGrabber }

procedure TScreenGrabber.CaptureMonitor(AFileName: String; AMonitorId: Integer;
  AIncludeCursor: boolean);
var
  Rect: TRect;
  UsedMonitor: TMonitor;
begin
  debugln();
  debugln('begin CaptureMonitor()');
  DebugLn(['Monitor id=', AMonitorId]);

  UsedMonitor := Screen.Monitors[AMonitorId];
  Rect.Left   := UsedMonitor.Left;
  Rect.Top    := UsedMonitor.Top;
  Rect.Width  := UsedMonitor.Width;
  Rect.Height := UsedMonitor.Height;
  CaptureRegion(AFileName, Rect, AIncludeCursor);
end;

procedure TScreenGrabber.CaptureAllMonitors(AFileName: String; AIncludeCursor: boolean);
var
  Rect: TRect;
begin
  DebugLn('');
  debugln('begin CaptureAllMonitors()');

  Rect.Left   := GetSystemMetrics(SM_XVIRTUALSCREEN);
  Rect.Top    := GetSystemMetrics(SM_YVIRTUALSCREEN);
  Rect.Width  := GetSystemMetrics(SM_CXVIRTUALSCREEN);
  Rect.Height := GetSystemMetrics(SM_CYVIRTUALSCREEN);
  CaptureRegion(AFileName, Rect, AIncludeCursor);
end;

procedure TScreenGrabber.CaptureRegion(AFileName: String; ARect: TRect;
  AIncludeCursor: boolean);
{$IfDef Linux}
const
  HWND_DESKTOP = 0;
{$EndIf}
var
  Bitmap: TBGRABitmap;
  bitmap2:Graphics.TBitmap;
  Writer: TFPCustomImageWriter;
  //GIF: TGIFImage;
  ScreenDC: {$IfDef Windows}Windows.{$EndIf}HDC;
begin
  DebugLn('Start taking screenshot...');
  DebugLn('Region: ', DbgS(ARect));
  DebugLn('With cursor: ', dbgs(AIncludeCursor));

  Bitmap := TBGRABitmap.Create(ARect.Width, ARect.Height, BGRABlack);

  //Bitmap.TakeScreenshot(Rect); // Not supports multiply monitors
  ScreenDC := GetDC(HWND_DESKTOP); // Get DC for all monitors
  DebugLn('ScreenDC=', DbgS(ScreenDC));
  if ScreenDC <> 0 then
  begin
    try
      {$IfDef Windows}
      bitmap2:=Graphics.TBitmap.Create;
      try
        bitmap2.SetSize(ARect.Width, ARect.Height);
        bitmap2.Canvas.Brush.Color:=clBlack;
        bitmap2.Canvas.FillRect(ARect);

        // https://github.com/artem78/AutoScreenshot/issues/35
        // and https://github.com/bgrabitmap/bgrabitmap/issues/200
        if BitBlt(Bitmap2.Canvas.Handle, 0, 0, ARect.Width, ARect.Height,
                 ScreenDC, ARect.Left, ARect.Top, SRCCOPY) then
          DebugLn('BitBlt call success')
        else
          DebugLn('BitBlt call failed with code %d', [GetLastError]);

       // Bitmap.Assign(bitmap2);
       Bitmap.Canvas.Draw(0,0,Bitmap2);
       //без этого костыля вот такая хуйня творится,если bitblt будет писать сразу в TBGRABitmap
       // https://github.com/artem78/AutoScreenshot/issues/37
       //https://forum.lazarus.freepascal.org/index.php/topic,74800.0.html

      finally
        bitmap2.Free;
      end;

      {$EndIf}
      {$IfDef Linux}
      // ToDo: Check bug #35 in Linux
      Bitmap.LoadFromDevice(ScreenDC, ARect);
      {$EndIf}
    finally
      ReleaseDC(0, ScreenDC);
    end;
  end
  else
  begin
    DebugLn('ScreenDC is NULL !!!');
    exit;
  end;


  //cursor
  if AIncludeCursor then
    DrawCursorOverBitmap(bitmap);


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

      fmtWEBP:
        begin
          Writer := TBGRAWriterWebP.Create;
        end;

      fmtAVIF:
        begin
          (*{$IfDef Windows}
          // flipped image fix
          Bitmap.VerticalFlip();
          {$EndIf}*)
          Writer := TBGRAWriterAvif.Create;
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

