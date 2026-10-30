unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, IniPropStorage, FileUtil;

type

  { TForm1 }

  TForm1 = class(TForm)
    DirectoryEdit1: TDirectoryEdit;
    ImportFromTransifexButton: TButton;
    ExportToTransifexButton: TButton;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    procedure ExportToTransifexButtonClick(Sender: TObject);
    procedure ImportFromTransifexButtonClick(Sender: TObject);
    procedure IniPropStorage1RestoreProperties(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses StrUtils, htmlelements;

{$R *.lfm}

const
  INI_SECTION='translation';

{ TForm1 }

procedure TForm1.ExportToTransifexButtonClick(Sender: TObject);
var
  {langs,} files: TStringList;
  inFileName, outFileName: string;
  inIni, outIni: TIniFile;
  k: string;
  allKeys: TStringList;
  //engStringsCount: Integer=0;
  msg: TStringList;
begin
  //langs:=TStringList.Create;
  files:=TStringList.Create;
  msg:=TStringList.Create;
  try
    FindAllFiles(files, ConcatPaths([DirectoryEdit1.Directory, 'lang']), '*.ini', false);
    for infilename in files do
    begin
      writeln(inFileName);
      outFileName:=ConcatPaths([ExtractFilePath(inFileName), 'export', ExtractFileName(inFileName)]);
      ForceDirectories(ExtractFilePath(outFileName));
      Writeln(outFileName);
      DeleteFile(outFileName);

      try
        allKeys:=TStringList.Create;
        outIni:=TIniFile.Create(outFileName);
        inIni:=TIniFile.Create(inFileName);
        inIni.ReadSection(INI_SECTION, allKeys);

        {if EndsStr('en.ini', outFileName) then
          engStringsCount:=allKeys.Count;}

        for k in allkeys do
        begin
          writeln(k);
          outIni.WriteString(INI_SECTION, k, inIni.ReadString(INI_SECTION, k, ''));
        end;
        outini.UpdateFile;

        msg.Append(Format('%s '#9'-'#9'%d строк', [ExtractFileName(outFileName), allKeys.Count]));
      finally
        FreeAndNil(allKeys);
        FreeAndNil(outini);
        FreeAndNil(inIni);
      end;
    end;

    msg.Insert(0, 'Готово!'+LineEnding+LineEnding);
    ShowMessage(msg.Text);

  finally
    msg.free;
    files.free;
    //langs.free;
  end;
end;

procedure TForm1.ImportFromTransifexButtonClick(Sender: TObject);
var
  tfxFiles:TStringList;
  tfxDir:string='';
  tfxFileName,langCode:string;
  p:integer;
  s:string;
  myIniFileName:string;
  myIni, tfxIni: tinifile;
  StrId, StrVal:string;
  strs:TStringList;
  i:integer;
begin
  with TSelectDirectoryDialog.Create(nil) do
  begin
    InitialDir:={IniPropStorage1.ReadString('LastTfxImportDir', '')} IniPropStorage1.StoredValue['LastTfxImportDir'];
    if Execute then
    begin
      tfxDir:=filename;
      {IniPropStorage1.WriteString('LastTfxImportDir', filename);}
      IniPropStorage1.StoredValue['LastTfxImportDir']:=filename;
    end;
    free;
  end;

  if tfxDir.IsEmpty then
    exit;

  tfxFiles:=TStringList.Create;
  try
     FindAllFiles(tfxFiles, tfxdir, '*.ini', False);
     for tfxFileName in tfxFiles do
     begin
       writeln('');
       WriteLn(tfxFileName);
       s:=ExtractFileNameWithoutExt(extractfilename(tfxFilename))   ;
       p:=pos('_', s);
       langcode:=copy(s,p+1);
       writeln(langcode);
       myIniFileName:=ConcatPaths([DirectoryEdit1.Directory, 'lang', langCode+'.ini']);
       writeln(myIniFileName);

       tfxIni := TIniFile.Create(tfxFileName, [ifoStripComments]);
       myIni := TIniFile.Create(myIniFileName);
       strs := TStringList.Create;
       try
         tfxini.ReadSection(INI_SECTION, STRS);
         for StrId in strs do
         //for i := 0 to strs.Count - 1 do
         begin
           if StartsStr('#', strid) then continue;// нах закомментированное...
           //writeln('strid:',strid);
           strval := tfxIni.ReadString(INI_SECTION, strid, '');
           if strval.IsEmpty then continue; // на всякий случай...

           strval := UnescapeHtml(strval); // &quot;  -->  "  (и не только)
           writeln(strid,' --> ',strval);



           myIni.WriteString(INI_SECTION, strid, StrVal);
         end;
       finally
         strs.free;
         myIni.free;
         tfxini.free;
       end;
     end;
  finally
    tfxFiles.free;
  end;

  showmessage('Готово!');

end;

procedure TForm1.IniPropStorage1RestoreProperties(Sender: TObject);
begin

end;

end.

