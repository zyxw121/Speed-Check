# Speed-Check
Lightweight Download/Upload Speed Testing Server and Client

## Requirements
The latest version of [Racket](https://racket-lang.org/).

## Getting Started

Compile the Racket files `speedcheck.rkt` and `speedcheck-server.rkt`:

```
$ raco exe speedcheck.rkt
$ raco exe speedcheck-server.rkt
```

This creates two executables, the Speed-Check client and server. 

## Usage
Start the server by calling it with an optional port number to use (default is 8080).
```
$ speedcheck-server 1234
Started Speed-Check server
>
```

Quit the server by entering `quit` into the prompt.
```
>quit
Quitting Speed-Check
```

While `speedcheck-server` is running on the target, call `speedcheck` with the target's hostname and optionally a port number (default is 8080).

```
$ speedcheck localhost 1234
Testing Download
100%
1904.86 Mbps
Testing Upload
100%
5333.33 Mbps
```

The test should take about 40 seconds in total. There is a percentage indicator for the download and upload portion.
