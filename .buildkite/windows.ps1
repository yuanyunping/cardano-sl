# Get custom GHC
mkdir c:\ghc

Invoke-WebRequest "https://s3.eu-central-1.amazonaws.com/ci-static/ghc-8.2.2-x86_64-unknown-mingw32.tar.xz" -OutFile "C:\ghc\ghc.tar.xz" -UserAgent "Curl"

7z x C:\ghc\ghc.tar.xz -oC:\ghc

7z x C:\ghc\ghc.tar -oC:\ghc

$env:PATH="$env:PATH;C:\ghc\ghc-8.2.2\bin"
