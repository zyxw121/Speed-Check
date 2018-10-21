# Speed-Check
Lightweight Download/Upload Speed Testing Server and Client

## Requirements
The latest version of [Racket](https://racket-lang.org/).

## Getting Started

Compile with `make`. 

This creates two executables, the Speed-Check client and server. 

## Usage
Start the server. 
```
$ speedcheck-server 
Started Speed-Check server
>
```

Quit the server by entering `quit` into the prompt.
```
>quit
Quitting Speed-Check
```

While `speedcheck-server` is running on the target, call `speedcheck` with the target's hostname. 

```
$ speedcheck localhost 
Testing Download
100%
1904.86 Mbps
Testing Upload
100%
5333.33 Mbps
```

The test should take about 40 seconds in total. There is a progress indicator for the download and upload portion.

## Options
Both the server and client accept several optional flags.

* `-p [port]` to change the port the service uses. Default is 8080.
* -v for verbose mode
* -h for help
