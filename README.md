# SSGR

Static Site Generator by Richo

## Compilation

To build a jar file you can close the REPL and execute:

    $ lein uberjar

This command will generate a jar file in the `/target/uberjar/` directory. You can then start the server by running the jar file:

    $ java -jar target/uberjar/ssgr-x.y.z-standalone.jar

### Native image

To generate an `.exe` using GraalVM native image simply run

    $ .\build.bat

This will first generate the uberjar, and then call the native-image command with the appropiate parameters.