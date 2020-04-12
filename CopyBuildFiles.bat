@echo off

if not exist build mkdir build

echo Copy translation files...
if not exist build\lang mkdir build\lang
del /S /Q build\lang\
copy lang\ build\lang\

echo Done!

pause