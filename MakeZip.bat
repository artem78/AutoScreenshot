@echo off

REM ******************************************************
REM *                                                    *
REM *           Batch file for creating ZIP              *
REM *                                                    *
REM * Note:                                              *
REM *    Before run this bat file you need to manually   *
REM *    compile executable using delphi 7 ide           *
REM *    (not tested with older versions).               *
REM *                                                    *
REM * Note 2:                                            *
REM *    This tool used 7Zip, so first you need to       *
REM *    install it (visit https://www.7-zip.org/) and   *
REM *    provide path to your "7z.exe" in "SevenZipPath" *
REM *    variable.                                       *
REM *                                                    *
REM ******************************************************


REM ***  Set variables ***
REM Path to 7Zip console archiver (7z.exe)
set SevenZipPath="C:\Program Files (x86)\7-Zip\7z.exe"

REM Output dirs
set BuildDir="build\files"
set TargetZipDir="build\zip"

REM Program version
set ProgramVersion=
for /f tokens^=4^ delims^=^"^\ %%a in ('findstr /rc:"VALUE \"ProductVersion\"," "res\VERSIONINFO.rc"') do (
	set ProgramVersion=%%a
	goto EndLoop
)
:EndLoop
REM echo version=%ProgramVersion%

REM ***********************


REM Some initial checks
if not exist %SevenZipPath% call :StopBuild "Make sure you have installed 7-zip and set correct path to your 7z.exe in "SevenZipPath" variable in this batch file"
if not exist AutoScreen.exe call :StopBuild "You need to manually compile project from Delphi 7 IDE before run this batch"


REM Create and clear build directory
echo Clear build directory...
if not exist %BuildDir% mkdir %BuildDir%
del /S /Q %BuildDir%\
echo Done!
echo.

REM Executable
echo Copy EXE...
copy AutoScreen.exe %BuildDir%\AutoScreenshot.exe
echo Done!
echo.

REM REM Config
REM echo Copy config.ini...
REM copy config.sample.ini %BuildDir%\config.ini
REM echo Done!
REM echo.

REM Translations
echo Copy translation files...
if not exist %BuildDir%\lang mkdir %BuildDir%\lang
del /S /Q %BuildDir%\lang\
copy lang\ %BuildDir%\lang\
echo Done!
echo.

REM Pack to ZIP archive
echo Pack all files to ZIP archive...
%SevenZipPath% a -tzip %TargetZipDir%\autoscreenshot_%ProgramVersion%.zip .\%BuildDir%\*
echo Done!

echo.
echo.
echo === Build successfully finished!  ===
echo.
pause

exit 0

:StopBuild
echo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
echo !!!                Fatal error!                !!!
echo.
echo.%~1
echo.
echo !!!          Build failed. Sorry :(            !!!
echo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
echo .
pause
exit 1