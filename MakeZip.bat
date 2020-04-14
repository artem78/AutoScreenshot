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
REM Path to 7Zip archiver
set SevenZipPath="C:\Program Files (x86)\7-Zip\7z.exe"

REM Program version
set ProgramVersion=
for /f "usebackq tokens=1,3 delims='; " %%i in ("version.inc") do (
	REM echo i=%%i j=%%j
	if "%%i" equ "ProgramVersion" (
		set ProgramVersion=%%j
	)
)


REM Create and clear build directory
echo Clear build directory...
if not exist build mkdir build
del /S /Q build\
echo Done!
echo.

REM Executable
echo Copy EXE...
copy AutoScreen.exe build\AutoScreenshot.exe
echo Done!
echo.

REM REM Config
REM echo Copy config.ini...
REM copy config.sample.ini build\config.ini
REM echo Done!
REM echo.

REM Translations
echo Copy translation files...
if not exist build\lang mkdir build\lang
del /S /Q build\lang\
copy lang\ build\lang\
echo Done!
echo.

REM Pack to ZIP archive
echo Pack all files to ZIP archive...
%SevenZipPath% a -tzip build\autoscreenshot_%ProgramVersion%.zip .\build\*
echo Done!

echo.
echo.
echo === Build finished!  ===
echo.
pause