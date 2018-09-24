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
Run `speedcheck` on the client to open the interactive prompt.

```
$ speedcheck
Speedcheck, version 0.0.1: https://github.com/zyxw121/Speed-Check   :? for help
>
```

To connect to a server, make sure the `speedcheck-server` is running on the target. Then use the `connect [address]` command.

```
>connect 127.0.0.1
Connected to server 127.0.0.1
>
```

Once you're connected, you can use the commands `up [number of MB to send]` and `down [number of MB to send]` to test upload and download speeds.

```
>up 10
72ms   138.89MBps
```

The command `:d` disconnects from the current server and `:q` quits the session.


