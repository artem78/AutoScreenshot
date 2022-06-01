# ******************************************************
# *                                                    *
# *       Shell script for creating installer          *
# *                                                    *
# ******************************************************


# ***  Functions  ***
Pause()
{
read -n 1 -s -r -p "Press any key to continue..."
}

# ***  Set variables ***
LazarusDir="/cygdrive/f/Programms/lazarus_2.0.12_32bit"
InnoSetupDir="/cygdrive/d/Программы/Inno Setup 5"

# ***********************

# Run compile project
echo "Starting compile project..."
"$LazarusDir/lazbuild.exe" --build-mode="Release (32bit)" --verbose AutoScreen.lpi
echo "Compiling finished!"
echo ""

# Make installation file
echo "Make installation file..."
"$InnoSetupDir/iscc.exe" "setup.iss"
echo "Done!"
echo ""


Pause
exit 0
