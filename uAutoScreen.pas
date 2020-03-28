unit uAutoScreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, inifiles, Spin, FileCtrl, pngImage,
  TrayIcon, XPMan, jpeg, ShellAPI;

type
  TImageFormat = (fmtPNG=0, fmtJPG);

  TForm1 = class(TForm)
    savepath: TEdit;
    savepath_btn: TButton;
    Timer1: TTimer;
    interval: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    TrayIcon1: TTrayIcon;
    XPManifest1: TXPManifest;
    Label3: TLabel;
    btnCaptureNow: TButton;
    lbJpegCompression: TLabel;
    spedJpgCompression: TSpinEdit;
    btnOpenFolder: TButton;
    cbStopWhenInactive: TCheckBox;
    cbFormat: TComboBox;
    Label4: TLabel;
    gbAutoCaptureControl: TGroupBox;
    start_timer_btn: TButton;
    stop_timer_btn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure savepath_btnClick(Sender: TObject);
    procedure savepathChange(Sender: TObject);
    procedure intervalChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    //procedure DoMinimize(Sender: TObject);
    //procedure WMSize(var Msg : TMessage); message WM_SIZE;
    procedure ApplicationMinimize(Sender: TObject);

    procedure set_timer_enabled(is_enabled: boolean);
    function get_timer_enabled: boolean;

    property timer_enabled: boolean read get_timer_enabled write set_timer_enabled;
    procedure start_timer_btnClick(Sender: TObject);
    procedure stop_timer_btnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure btnCaptureNowClick(Sender: TObject);
    procedure spedJpgCompressionChange(Sender: TObject);
    function getSaveDir: String;
    procedure btnOpenFolderClick(Sender: TObject);
    procedure cbStopWhenInactiveClick(Sender: TObject);
    procedure cbFormatChange(Sender: TObject);
  private
    //procedure DoMinimize(Sender: TObject);
    //procedure WMSize(var Msg: TMessage);
    procedure MakeScreenshot;
    { Private declarations }
  public
    { Public declarations }
  end;

const
  ImageFormatNames: array [TImageFormat] of String = ('PNG', 'JPG');

var
  Form1: TForm1;
  ini: TIniFile;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Fmt: TImageFormat;
begin
  Application.OnMinimize := ApplicationMinimize;

  for Fmt := Low(TImageFormat) to High(TImageFormat) do
    cbFormat.Items.Append(ImageFormatNames[Fmt]);

  ini := TIniFile.Create(ExtractFilePath(Application.ExeName) + '\config.ini');

  savepath.Text := ini.ReadString('main', 'path', ExtractFilePath(Application.ExeName));
  interval.Value := ini.ReadInteger('main', 'interval', 5);
  cbStopWhenInactive.Checked := ini.ReadBool('main', 'stopWhenInactive', False);
  if ini.ReadBool('main', 'png', True) then
    cbFormat.ItemIndex := Ord(fmtPNG);
  if ini.ReadBool('main', 'jpg', False) then
    cbFormat.ItemIndex := Ord(fmtJPG);
  spedJpgCompression.MinValue := Low(TJPEGQualityRange);
  spedJpgCompression.MaxValue := High(TJPEGQualityRange);
  spedJpgCompression.Value := ini.ReadInteger('main', 'jpegCOmpression', 80);
  cbFormat.OnChange(cbFormat);

  Timer1.Interval := interval.Value * 60 * 1000;
  timer_enabled := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ini.Free;
end;

procedure TForm1.savepath_btnClick(Sender: TObject);
var
  dir: string;
begin
  dir := savepath.Text;

  if SelectDirectory('Выберите каталог', '' {savepath.Text}, dir) then
  //if SelectDirectory(dir, [sdAllowCreate, sdPerformCreate], 0) then
  begin
    savepath.Text := dir;
    ini.WriteString('main', 'path', dir);
  end;
end;

procedure TForm1.savepathChange(Sender: TObject);
begin
    ini.WriteString('main', 'path', savepath.Text);
end;

procedure TForm1.intervalChange(Sender: TObject);
begin
  ini.WriteInteger('main', 'interval', interval.Value);
  Timer1.Interval := interval.Value * 60 * 1000;
end;

function LastInput: DWord; forward;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if cbStopWhenInactive.Checked then
  begin
    // Не сохранять скриншот при бездействии пользователя
    // ToDo: Можно добавить проверку наличия заставки
    // или что пользователь вышел из сеанса
    // ToDo: Можно добавить сравнение текущего снимка с последним
    // сохранённым и если они одинаковы, не сохранять текущий
    if Timer1.Interval > LastInput then
      MakeScreenshot;
  end
  else
    MakeScreenshot;
end;

function TForm1.get_timer_enabled: boolean;
begin
  Result := Timer1.Enabled;
end;

procedure TForm1.set_timer_enabled(is_enabled: boolean);
begin
  Timer1.Enabled := is_enabled;
  start_timer_btn.Enabled := not is_enabled;
  stop_timer_btn.Enabled := is_enabled;
end;

procedure TForm1.start_timer_btnClick(Sender: TObject);
begin
  timer_enabled := True;
end;

procedure TForm1.stop_timer_btnClick(Sender: TObject);
begin
  timer_enabled := False;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  {CanClose := false;
  TrayIcon1.IconVisible := true;
  ShowWindow(Form1.Handle, SW_HIDE);    }
end;

procedure TForm1.TrayIcon1DblClick(Sender: TObject);
begin
  TrayIcon1.IconVisible := False;
  TrayIcon1.AppVisible := True;
  TrayIcon1.FormVisible := True;
  Application.Restore;
  Application.BringToFront();
end;

//procedure TForm1.DoMinimize(Sender: TObject);
//begin
  {TrayIcon1.IconVisible := true;
  ShowWindow(Form1.Handle, SW_HIDE);  }
//end;

{procedure TForm1.WMSize(var Msg: TMessage);
begin
  if msg.WParam=SIZE_MINIMIZED then
  begin
    TrayIcon1.IconVisible := true;
    ShowWindow(Form1.Handle, SW_HIDE);
  end;
end;       }

procedure TForm1.ApplicationMinimize(Sender: TObject);
begin
  TrayIcon1.AppVisible := False;
  TrayIcon1.FormVisible := False;
  TrayIcon1.IconVisible := True;
end;

procedure TForm1.MakeScreenshot;
var
  dirname, filename{, fullpath}: string;
  png: TPNGObject;
  bmp:TBitmap;
  jpg: TJPEGImage;
begin
  DateTimeToString(filename, 'yyyy-mm-dd hh.mm.ss', Now());

  dirname := getSaveDir;


  bmp := TBitmap.Create;
  bmp.Width := Screen.Width;
  bmp.Height := Screen.Height;
  BitBlt(bmp.Canvas.Handle, 0,0, Screen.Width, Screen.Height,
           GetDC(0), 0,0,SRCCOPY);

  if cbFormat.ItemIndex = Ord(fmtPNG) then
  begin                   // PNG
    PNG := TPNGObject.Create;
    try
      PNG.Assign(bmp);
      PNG.SaveToFile(dirname + filename + '.png');
    finally
      bmp.Free;
      PNG.Free;
    end;
  end;

  if cbFormat.ItemIndex = Ord(fmtJPG) then
  begin                 // JPG
    jpg := TJPEGImage.Create;
    try
      jpg.Assign(bmp);
      jpg.CompressionQuality := spedJpgCompression.Value;
      jpg.Compress;
      jpg.SaveToFile(dirname + filename + '.jpg');
    finally
      jpg.Free;
      bmp.Free;
    end;
  end;
end;

procedure TForm1.btnCaptureNowClick(Sender: TObject);
begin
  MakeScreenshot;
end;

procedure TForm1.spedJpgCompressionChange(Sender: TObject);
begin
  try
    ini.WriteInteger('main', 'jpegCompression', spedJpgCompression.Value);
  finally
  end;
end;

function TForm1.getSaveDir: String;
var
  dirname: string;
begin
  DateTimeToString(dirname, 'yyyy-mm-dd', Now());

  dirname := IncludeTrailingPathDelimiter(ini.ReadString('main', 'path', '')) + dirname + '\';
  if not DirectoryExists(dirname) then
    CreateDir(dirname);

  Result := dirname;
end;

procedure TForm1.btnOpenFolderClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(getSaveDir), nil, nil, SW_SHOWNORMAL);
end;

procedure TForm1.cbStopWhenInactiveClick(Sender: TObject);
begin
  ini.WriteBool('main', 'stopWhenInactive', cbStopWhenInactive.Checked);
end;

// Функции/процедуры

function LastInput: DWord;
var
  LInput: TLastInputInfo;
begin
  LInput.cbSize := SizeOf(TLastInputInfo);
  GetLastInputInfo(LInput);
  Result := GetTickCount - LInput.dwTime;
end;

procedure TForm1.cbFormatChange(Sender: TObject);
var
  IsJPG: Boolean;
begin
  IsJPG := cbFormat.ItemIndex = Ord(fmtJPG);
  spedJpgCompression.{Enabled}Visible := IsJPG;
  lbJpegCompression.{Enabled}Visible := IsJPG;
  Label4.{Enabled}Visible := IsJPG;

  ini.WriteBool('main', 'png', cbFormat.ItemIndex = Ord(fmtPNG));
  ini.WriteBool('main', 'jpg', IsJPG);
end;

end.
