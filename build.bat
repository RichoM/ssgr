call lein uberjar
call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
native-image --report-unsupported-elements-at-runtime ^
             --initialize-at-build-time ^
             --no-server ^
             -jar ./target/uberjar/ssgr-1.0-standalone.jar ^
             -H:ReflectionConfigurationFiles=reflect-config.json ^
             -H:Name=./target/ssgr
