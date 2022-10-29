#!/bin/bash

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
	"$LazarusDir/lazbuild.exe" --build-mode="Release (32bit)" --verbose AutoScreen.lpi
	echo "Compiling finished!"
	echo ""
}

function MakeZip(){
	Compile
	
	# Create and clear build directory
	echo "Clear build directory..."
	rm -f -r $BuildDir
	mkdir -p $BuildDir
	mkdir -p $TargetZipDir
	echo "Done!"
	echo ""

	# Executable
	echo "Copy EXE..."
	cp -v --preserve=timestamps AutoScreenshot.exe $BuildDir
	echo "Done!"
	echo ""

	# DLLs
	echo "Copy DLLs..."
	cp -v --preserve=timestamps $LazarusDir/libeay32.dll $BuildDir
	cp -v --preserve=timestamps $LazarusDir/ssleay32.dll $BuildDir
	echo "Done!"
	echo ""

	# # Config
	# echo "Copy config.ini..."
	# cp -v --preserve=timestamps config.sample.ini $BuildDir/config.ini
	# echo "Done!"
	# echo ""

	# Translations
	echo "Copy translation files..."
	mkdir -p $BuildDir/lang
	cp -v --preserve=timestamps lang/*.ini $BuildDir/lang/
	echo "Done!"
	echo ""

	# Pack to ZIP archive
	### Note! For MinGW (Git Bash) see https://stackoverflow.com/a/55749636 ###
	echo "Pack all files to ZIP archive..."
	ZipPath=$TargetZipDir/autoscreenshot_${ProgramVersion}_portable.zip
	rm -f $ZipPath
	cd $BuildDir
	zip -r $ZipPath *
	#tar -C $BuildDir -cvf $ZipPath $BuildDir/*
	echo "Done!"
	echo ""
}

function MakeInstaller(){
	Compile;

	# Make installation file
	echo "Make installation file..."
	"$InnoSetupDir/iscc.exe" "setup.iss"
	echo "Done!"
	echo ""
}

# ***********************

set -e

# ***  Set variables ***
LazarusDir="/f/Programms/lazarus_2.0.12_32bit"
if [[ $(uname -s | tr '[:upper:]' '[:lower:]') == *"cygwin"* ]]; then
	LazarusDir="/cygdrive${LazarusDir}"
fi
echo "LazarusDir=${LazarusDir}"


# Output dirs
BuildDir=$(realpath -m "build/files")
TargetZipDir=$(realpath -m "build/zip")

InnoSetupDir="/d/Программы/Inno Setup 5"
if [[ $(uname -s | tr '[:upper:]' '[:lower:]') == *"cygwin"* ]]; then
	InnoSetupDir="/cygdrive${InnoSetupDir}"
fi
echo "InnoSetupDir=${InnoSetupDir}"

# Program version
ProgramVersion=$(grep -Po '\<StringTable.+ ProductVersion="\K[0-9\.]+' AutoScreen.lpi)
#echo "Current program version: $ProgramVersion"
#echo ""

usage="$(basename "$0") [-z] [-i]

where:
    -z  make zip arhive
    -l  make installer"

# ***********************


if [ -z $* ]
then
	#echo "No options found!"
	echo "$usage" >&2
	exit 1
fi

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

Pause
exit 0
