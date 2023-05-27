#!/bin/bash

# Script for building project on Linux or Windows (using Cygwin or MinGW)
#
# Note for Winows only!
#    Do not forget to define variables LazarusDir and InnoSetupDir first,
#    for example in your ~/.bashrc:
#        export LazarusDir="/path/to/lazarus"
#        export InnoSetupDir="/path/to/inno setup"
#


# ***  Functions  ***

function StopBuild(){
	echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
	echo "!!!                Fatal error!                !!!"
	echo ""
	echo $1
	echo ""
	echo "!!!          Build failed. Sorry :(            !!!"
	echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
	echo ""
	Pause
	exit 1
}

function Pause(){
	read -n 1 -s -r -p "Press any key to continue..."
}

function Compile(){
	# Run compile project
	echo "Starting compile project..."
	if IsWindows; then
		"$LazarusDir/lazbuild.exe" --build-mode="Release (32bit)" --verbose AutoScreenshot.lpi
	elif IsLinux; then
		"lazbuild" --build-mode="Release" --verbose AutoScreenshot.lpi
	fi
	echo "Compiling finished!"
	echo ""
}

function MakeZip(){
	Compile
	RunTests
	
	# Create and clear build directory
	echo "Clear build directory..."
	rm -f -r "$BuildDir"
	mkdir -p "$BuildDir"
	mkdir -p "$TargetZipDir"
	echo "Done!"
	echo ""

	# Executable
	echo "Copy EXE..."
	if IsWindows; then
		local ExecutableFileName="AutoScreenshot.exe"
	elif IsLinux; then
		local ExecutableFileName="AutoScreenshot"
	fi
	cp -v --preserve=timestamps "$ExecutableFileName" "$BuildDir"
	echo "Done!"
	echo ""

	# DLLs
	if IsWindows; then
		echo "Copy DLLs..."
		cp -v --preserve=timestamps "$LazarusDir/libeay32.dll" "$BuildDir"
		cp -v --preserve=timestamps "$LazarusDir/ssleay32.dll" "$BuildDir"
		echo "Done!"
		echo ""
	fi

	# # Config
	# echo "Copy config.ini..."
	# cp -v --preserve=timestamps config.sample.ini "$BuildDir/config.ini"
	# echo "Done!"
	# echo ""

	# Translations
	echo "Copy translation files..."
	mkdir -p "$BuildDir/lang"
	cp -v --preserve=timestamps lang/*.ini "$BuildDir/lang/"
	echo "Done!"
	echo ""

	# Pack to ZIP archive
	### Note! For MinGW (Git Bash) see https://stackoverflow.com/a/55749636 ###
	echo "Pack all files to ZIP archive..."
	if IsWindows; then
		local OsTypeName="windows"
	elif IsLinux; then
		local OsTypeName="linux"
	fi
	ZipPath="$TargetZipDir/autoscreenshot_${ProgramVersion}_${OsTypeName}_portable.zip"
	rm -f "$ZipPath"
	cd "$BuildDir"
	zip -r "$ZipPath" *
	#tar -C "$BuildDir" -cvf "$ZipPath" "$BuildDir/*"
	echo "Done!"
	echo ""
}

function MakeInstaller(){
	Compile
	RunTests

	# Make installation file
	echo "Make installation file..."
	"$InnoSetupDir/iscc.exe" //F"autoscreenshot_${ProgramVersion}_setup" "setup.iss"
	echo "Done!"
	echo ""
}

function RunTests(){
	cd Test
	
	echo "Compile tests..."
	if IsWindows; then
		local LazBuildPath="$LazarusDir/lazbuild.exe"
	elif IsLinux; then
		local LazBuildPath="lazbuild"
	fi
#	"$LazBuildPath" --verbose AutoScreenshotTest.lpi
	"$LazBuildPath" AutoScreenshotTest.lpi
	echo "Tests compiled!"
	echo ""
	
	echo "Run tests..."
	if IsWindows; then
		local TestExecutableFileName="./AutoScreenshotTest.exe"
	elif IsLinux; then
		local TestExecutableFileName="./AutoScreenshotTest"
	fi
	"$TestExecutableFileName" --all --format=plain
	echo "Tests finished!"
	echo ""
	
	cd ..
}

function IsWindows {
	if [[ "$OSTYPE" == "cygwin" ]]; then
		return 0
	elif [[ "$OSTYPE" == "msys" ]]; then
		return 0
	elif [[ "$OSTYPE" == "win32" ]]; then
		return 0
	else
		return 1
	fi
}

function IsLinux {
	if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        return 0
	else
		return 1
	fi
}

# ***********************

set -e

if IsWindows; then
	# ***  Set variables ***
	LazarusDir=${LazarusDir:="/c/lazarus"}
	#if [[ $(uname -s | tr '[:upper:]' '[:lower:]') == *"cygwin"* ]]; then
	#	LazarusDir="/cygdrive${LazarusDir}"
	#fi
	echo "LazarusDir=${LazarusDir}"
fi


# Output dirs
BuildDir=$(realpath -m "build/files")
TargetZipDir=$(realpath -m "build/zip")

if IsWindows; then
	InnoSetupDir=${InnoSetupDir:="/c/Program Files/Inno Setup 5"}
	#if [[ $(uname -s | tr '[:upper:]' '[:lower:]') == *"cygwin"* ]]; then
	#	InnoSetupDir="/cygdrive${InnoSetupDir}"
	#fi
	echo "InnoSetupDir=${InnoSetupDir}"
fi

# Program version
#ProgramVersion=$(grep -Po '\<StringTable.+ ProductVersion="\K[0-9\.]+' AutoScreenshot.lpi)
ProgramVersion=$(git describe --dirty)
echo "Current program version: $ProgramVersion"
echo ""

usage="$(basename "$0") [-z] [-i]

where:
    -z  make zip arhive
    -l  make installer (not available for Linux)"

# ***********************


if [ -z $* ]
then
	#echo "No options found!"
	echo "$usage" >&2
	exit 1
fi

if IsWindows; then
	while getopts "zih" opt
	do
		case $opt in
			z) MakeZip;;
			i) MakeInstaller;;
			h) echo "$usage"
				exit 0;;
			*) echo "$usage">&2
				exit 1;;
		esac
	done
elif IsLinux; then
	while getopts "zh" opt
	do
		case $opt in
			z) MakeZip;;
			h) echo "$usage"
				exit 0;;
			*) echo "$usage">&2
				exit 1;;
		esac
	done
else
	echo "This OS not supported!">&2
	exit 1
fi

Pause
exit 0
