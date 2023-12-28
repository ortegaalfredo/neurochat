cp ../../neurochat neurochat/usr/local/bin/
cp ../../llama.cpp/buildshared/libllama.so neurochat/usr/local/lib/
dpkg-deb --build neurochat
