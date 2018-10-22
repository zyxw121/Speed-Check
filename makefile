all: speedcheck speedcheck-server
speedcheck: speedcheck.rkt 
	raco exe speedcheck.rkt
speedcheck-server: speedcheck-server.rkt
	raco exe speedcheck-server.rkt
