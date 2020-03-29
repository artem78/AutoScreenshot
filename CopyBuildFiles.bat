@echo off

if not exist build mkdir build

echo Copy translation files...
if not exist build\lang mkdir build\lang
del build\lang\ /S /Q anydir
copy lang\ build\lang\

echo Done!

pause