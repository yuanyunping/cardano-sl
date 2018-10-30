# Get custom GHC
mkdir c:\ghc

Invoke-WebRequest "https://s3.eu-central-1.amazonaws.com/ci-static/ghc-8.2.2-x86_64-unknown-mingw32.tar.xz" -OutFile "D:\ghc\ghc.tar.xz" -UserAgent "Curl"

7z x D:\ghc\ghc.tar.xz -oD:\ghc

7z x D:\ghc\ghc.tar -oD:\ghc
Set-PSDebug -Trace 1

$env:PATH="$env:PATH;D:\ghc\ghc-8.2.2\bin"

# OpenSSL
#
$env:USERPROFILE
(New-Object Net.WebClient).DownloadFile('https://slproweb.com/download/Win64OpenSSL-1_0_2p.exe', "$($env:USERPROFILE)\Win64OpenSSL.exe")
#cmd /c start /wait "$($env:USERPROFILE)\Win64OpenSSL.exe" /silent /verysilent /sp- /suppressmsgboxes /DIR=C:\OpenSSL-Win64-v102
# Install stack
#Start-FileDownload http://www.stackage.org/stack/windows-x86_64 -FileName stack.zip
#7z x stack.zip stack.exe

#git clone https://github.com/facebook/rocksdb.git --branch v4.13.5
#Start-FileDownload 'https://s3.eu-central-1.amazonaws.com/ci-static/serokell-rocksdb-haskell-325427fc709183c8fdf777ad5ea09f8d92bf8585.zip' -FileName rocksdb.zip
#7z x rocksdb.zip

# CSL-1509: After moving the 'cardano-sl' project itself into a separate folder ('lib/'), the 'cardano-text.exe' executable fails on AppVeyor CI.
# After some investigation, it was discovered that this was because 'rocksdb.dll' has to be located in this folder as well, or else the test executable doesn't work.
#copy rocksdb.dll node
#copy rocksdb.dll lib
#copy rocksdb.dll wallet
#copy rocksdb.dll wallet-new

# Install liblzma/xz
#Start-FileDownload https://tukaani.org/xz/xz-5.2.3-windows.zip -Filename xz-5.2.3-windows.zip
#7z -oC:\xz_extracted x xz-5.2.3-windows.zip
