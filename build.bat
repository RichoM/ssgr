call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
native-image --report-unsupported-elements-at-runtime ^
             --initialize-at-build-time ^
             --no-server ^
             -jar ./target/uberjar/ssgr-0.1.0-SNAPSHOT-standalone.jar ^
             -H:Name=./target/ssgr
