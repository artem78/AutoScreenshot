unit DeleteOldFilesTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

const
  Dir = 'files';
type

  { TDeleteOldFilesTestCase }

  TDeleteOldFilesTestCase= class(TTestCase)
  private

  protected
    procedure SetUp; override;
  published
    //procedure _Empty;

    procedure TestMain;
    procedure TestNoExtFilter;
    procedure TestNotRecursive;
    procedure TestLeaveEmptyDirs;
  end;

implementation

uses
  FileUtil, DateUtils, uUtils
  {$IfDef Windows}
    ,Windows
  {$EndIf}
  ;

var
  TenDaysAgo: TDateTime;

function CountFilesInDir(const ADir: String; AIncludeSubdirs: Boolean): Integer;
var Info: TSearchRec;
Begin
  Result:=0;
  If FindFirst (ConcatPaths([ADir, '*']),faAnyFile,Info)=0 then
  begin
    Repeat
      With Info do
      begin
        If (Attr and faDirectory) = faDirectory then
        begin
          if (Name = '.') or (Name = '..') then
            Continue;

          if AIncludeSubdirs then
            Result := Result + CountFilesInDir(ConcatPaths([ADir, Name]), AIncludeSubdirs);

          Continue;
        end;


        Inc(Result);
      end;
    Until FindNext(info)<>0;
    SysUtils.FindClose(Info);
  end;
End;

procedure TDeleteOldFilesTestCase.TestMain;
begin
  DeleteOldFiles(Dir, TenDaysAgo, True, ['PNG', 'jpeg', 'Bmp', 'JPEG' {dup.}, 'jpg'], True);

  AssertTrue (FileExists(ConcatPaths([Dir, 'my photo.jpeg'])));
  AssertFalse(FileExists(ConcatPaths([Dir, 'logo.png'])));
  AssertTrue (FileExists(ConcatPaths([Dir, 'bitmap.bmp'])));
  AssertTrue (FileExists(ConcatPaths([Dir, 'text file.txt']))); // Must be ignored
  AssertTrue (FileExists(ConcatPaths([Dir, 'Paris.JPG'])));
  AssertFalse(FileExists(ConcatPaths([Dir, 'Красная площадь в Москве.JpeG'])));

  AssertTrue(FileExists(ConcatPaths([Dir, 'subfolder1', 'photo1.jpg'])));
  AssertTrue(FileExists(ConcatPaths([Dir, 'subfolder1', 'photo2.jpg'])));
  AssertTrue(FileExists(ConcatPaths([Dir, 'subfolder1', 'photo3.jpg'])));

  AssertFalse(FileExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.1', 'image1.bmp'])));
  AssertFalse(FileExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.1', 'image2.bmp'])));
  AssertTrue (FileExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.1', 'image3.bmp'])));

  AssertFalse(FileExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2', 'image1.bmp'])));
  AssertFalse(FileExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2', 'image2.bmp'])));
  AssertFalse(FileExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2', 'image3.bmp'])));

  AssertFalse(FileExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2', 'dir', 'image4.bmp'])));

  AssertTrue (FileExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.3', '.do_not_delete']))); // Must be ignored

  AssertTrue (DirectoryExists(ConcatPaths([Dir, 'subfolder1'])));
  AssertTrue (DirectoryExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.1'])));
  AssertFalse(DirectoryExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2'])));
  AssertTrue (DirectoryExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.3'])));

  AssertEquals(9, CountFilesInDir(Dir, True));
  AssertEquals(4, CountFilesInDir(Dir, False));
  AssertEquals(3, CountFilesInDir(ConcatPaths([Dir, 'subfolder1']), False));
  AssertEquals(1, CountFilesInDir(ConcatPaths([Dir, 'subfolder1', 'subfolder1.1']), False));
  AssertEquals(1, CountFilesInDir(ConcatPaths([Dir, 'subfolder1', 'subfolder1.3']), False));
end;

procedure TDeleteOldFilesTestCase.TestNoExtFilter;
begin
  // Without extensions filter
  DeleteOldFiles(Dir, TenDaysAgo, True, [], True);

  AssertFalse(FileExists(ConcatPaths([Dir, 'text file.txt'])));

  AssertEquals(8, CountFilesInDir(Dir, True));
  AssertEquals(3, CountFilesInDir(Dir, False));
end;

procedure TDeleteOldFilesTestCase.TestNotRecursive;
begin
  // Without recursive
  DeleteOldFiles(Dir, TenDaysAgo, False, ['png', 'jpeg', 'bmp', 'jpg'], True);

  AssertTrue (FileExists(ConcatPaths([Dir, 'my photo.jpeg'])));
  AssertFalse(FileExists(ConcatPaths([Dir, 'logo.png'])));
  AssertTrue (FileExists(ConcatPaths([Dir, 'bitmap.bmp'])));
  AssertTrue (FileExists(ConcatPaths([Dir, 'text file.txt']))); // Must be ignored
  AssertTrue (FileExists(ConcatPaths([Dir, 'Paris.JPG'])));
  AssertFalse(FileExists(ConcatPaths([Dir, 'Красная площадь в Москве.JpeG'])));

  AssertTrue (FileExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.1', 'image1.bmp'])));
  AssertTrue (FileExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2', 'dir', 'image4.bmp'])));

  AssertTrue(DirectoryExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2'])));

  AssertEquals(15, CountFilesInDir(Dir, True));
  AssertEquals(4,  CountFilesInDir(Dir, False));
end;

procedure TDeleteOldFilesTestCase.TestLeaveEmptyDirs;
begin
  // Do not delete empty folders
  DeleteOldFiles(Dir, TenDaysAgo, True, ['png', 'jpeg', 'bmp', 'jpg'], False);

  AssertTrue(DirectoryExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2', 'dir'])));
  AssertTrue(DirectoryExists(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2'])));

  AssertEquals(0, CountFilesInDir(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2']), True));

  AssertEquals(9, CountFilesInDir(Dir, True));
end;

procedure TDeleteOldFilesTestCase.SetUp;
  {$IfDef Windows}
  procedure SetFileCreatedTime(const AFileName: String; ATime: TDateTime);
  // Source: https://forum.lazarus.freepascal.org/index.php/topic,25541.msg155322.html#msg155322
  var
    fileHandle: THandle;
    fileTime: TFILETIME;
    LFileTime: TFILETIME;
    LSysTime: TSystemTime;
  begin
    //Result:=False;
    try
      DecodeDate(ATime, LSysTime.Year, LSysTime.Month, LSysTime.Day);
      DecodeTime(ATime, LSysTime.Hour, LSysTime.Minute, LSysTime.Second, LSysTime.Millisecond);
      if SystemTimeToFileTime(LSysTime, LFileTime) then
      begin
        if LocalFileTimeToFileTime(LFileTime, fileTime) then
        begin
          fileHandle:=FileOpen{UTF8}(aFilename, fmOpenReadWrite or fmShareExclusive);
          {if} SetFileTime(fileHandle, fileTime, fileTime, fileTime) {then
            Result:=True;}
        end;
      end;
    finally
      FileClose(fileHandle);
    end;
  end;
  {$EndIf}

  procedure SetFileModifiedTime(const AFileName: String; ATime: TDateTime);
  begin
    FileSetDate(AFileName, DateTimeToFileDate(ATime));
  end;

  procedure CreateFile(const AFileName: String; ACreatedTime: TDateTime = 0;
            AModifiedTime: TDateTime = 0);
  var
    F: TFileStream;
  begin
    if not DirectoryExists(ExtractFileDir(AFileName)) then
      CreateDir(ExtractFileDir(AFileName));

    F := TFileStream.Create(AFileName, fmOpenWrite or fmCreate);
    try
      F.WriteAnsiString(AFileName + ' content...');
    finally
      F.Free;
    end;

    {$IfDef Windows}
    if ACreatedTime <> 0 then
      SetFileCreatedTime(AFileName, ACreatedTime);
    {$EndIf}

    if AModifiedTime <> 0 then
      SetFileModifiedTime(AFileName, AModifiedTime)
    else
    begin
      if ACreatedTime <> 0 then
        SetFileModifiedTime(AFileName, ACreatedTime);
    end;
  end;

var
  TwoMonthsAgo, WeekAgo, Yesterday, ThreeYearsAgo: TDateTime;
begin
  if not DirectoryExists(Dir) then
    CreateDir(Dir)
  else
    DeleteDirectory(Dir, True);

  TwoMonthsAgo:=IncMonth(now, -2);
  WeekAgo:=IncDay(now, -7);
  Yesterday:=IncDay(now, -1);
  ThreeYearsAgo:=IncYear(now, -3);


  CreateFile(ConcatPaths([Dir, 'my photo.jpeg']));
  CreateFile(ConcatPaths([Dir, 'logo.png']),      TwoMonthsAgo);
  CreateFile(ConcatPaths([Dir, 'bitmap.bmp']),    WeekAgo);
  CreateFile(ConcatPaths([Dir, 'text file.txt']), TwoMonthsAgo);
  CreateFile(ConcatPaths([Dir, 'Paris.JPG']),     Yesterday);
  CreateFile(ConcatPaths([Dir, 'Красная площадь в Москве.JpeG']), ThreeYearsAgo);

  CreateFile(ConcatPaths([Dir, 'subfolder1', 'photo1.jpg']));
  CreateFile(ConcatPaths([Dir, 'subfolder1', 'photo2.jpg']));
  CreateFile(ConcatPaths([Dir, 'subfolder1', 'photo3.jpg']));

  CreateFile(ConcatPaths([Dir, 'subfolder1', 'subfolder1.1', 'image1.bmp']), TwoMonthsAgo);
  CreateFile(ConcatPaths([Dir, 'subfolder1', 'subfolder1.1', 'image2.bmp']), TwoMonthsAgo);
  CreateFile(ConcatPaths([Dir, 'subfolder1', 'subfolder1.1', 'image3.bmp']));

  CreateFile(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2', 'image1.bmp']), TwoMonthsAgo);
  CreateFile(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2', 'image2.bmp']), TwoMonthsAgo);
  CreateFile(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2', 'image3.bmp']), TwoMonthsAgo);

  {$IfDef Windows}
  // Different created and modified date
  CreateFile(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2', 'dir', 'image4.bmp']), TwoMonthsAgo, Yesterday);
  {$EndIf}
  {$IfDef Linux}
  CreateFile(ConcatPaths([Dir, 'subfolder1', 'subfolder1.2', 'dir', 'image4.bmp']), TwoMonthsAgo, TwoMonthsAgo);
  {$EndIf}


  CreateFile(ConcatPaths([Dir, 'subfolder1', 'subfolder1.3', '.do_not_delete']));
  {$IfDef Windows}
  FileSetAttr(ConcatPaths([Dir, 'subfolder1', 'subfolder1.3', '.do_not_delete']), faHidden);
  {$EndIf}
end;

{procedure TDeleteOldFilesTestCase._Empty;
begin

end;     }


initialization

  TenDaysAgo:=IncDay(Now, -10);

  RegisterTest('TUtilsTestCase', TDeleteOldFilesTestCase);
end.


