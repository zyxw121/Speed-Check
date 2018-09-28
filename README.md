# Speed-Check
Lightweight Download/Upload Speed Testing Server and Client

## Getting Started

Compile the Racket files `speedcheck.rkt` and `speedcheck-server.rkt`:

```
$ raco exe speedcheck.rkt
$ raco exe speedcheck-server.rkt
```

This creates two executables, the Speed-Check client and server. 

The service communicates using port 8080, so make sure it's unblocked.

## Usage
While `speedcheck-server` is running on the target, call `speedcheck` with the target's hostname.

```
$ speedcheck localhost
```

