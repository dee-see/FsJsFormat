#!/bin/sh
cd FsJsFormat
dotnet publish --self-contained --configuration Release --runtime linux-x64
cd ..
