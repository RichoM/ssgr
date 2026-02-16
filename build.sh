#!/bin/sh

lein uberjar

export JAVA_HOME=/Library/Java/JavaVirtualMachines/graalvm-jdk-25.0.1+8.1/Contents/Home
export PATH=/Library/Java/JavaVirtualMachines/graalvm-jdk-25.0.1+8.1/Contents/Home/bin/:$PATH

native-image --report-unsupported-elements-at-runtime \
             --initialize-at-build-time \
             --no-server \
             -jar ./target/uberjar/ssgr-1.1-standalone.jar \
             -H:ReflectionConfigurationFiles=reflect-config.json \
             -H:Name=./target/ssgr