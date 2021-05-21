npm -g uninstall _release/ligo-0.0.0.tgz
rm -rf _release
esy @minimal npm-release

cd _release

npm pack .

npm -g install ligo-0.0.0.tgz

cd ..

ligo compile-contract src/test/contracts/website2.ligo main
ligo compile-storage src/test/contracts/website2.ligo main 1
ligo compile-parameter src/test/contracts/website2.ligo main "Increment(1)"
ligo dry-run src/test/contracts/website2.ligo main "Increment(1)" 1
