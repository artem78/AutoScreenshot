git clone -n --depth=1 --filter=tree:0 https://github.com/madebybowtie/FlagKit.git
cd FlagKit/
git sparse-checkout set --no-cone "Assets/PNG"
git checkout
cd Assets/PNG
rm *@?x.png
