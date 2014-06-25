#! /usr/local/bin/gforth
\ udp terminal for mecrips stellaris

require unix/socket.fs
require unix/pthread.fs

4201 Constant term-port#
1474 Constant max-udp#

new-udp-socket Value term-sock

sockaddr_in4 %size buffer: sockaddr
max-udp# buffer: inbuf

: info>string ( addr -- addr u )
    dup ai_addr @ swap ai_addrlen l@ ;

: send-term ( addr u -- )
    term-sock -rot 0 sockaddr sockaddr_in4 %size sendto
    dup 0< IF  ?ior  ELSE  drop  THEN ;

: recv-term ( -- addr u )
    term-sock inbuf max-udp# 0 sockaddr alen recvfrom
    dup 0< IF  ?ior  THEN  inbuf swap ;

0 Value recv-task

pollfd %size 2* buffer: pollfds
: fds!+ ( fileno flag addr -- addr' )
    >r r@ events w!  r@ fd l!  r> pollfd %size + ; 

: prep-socks ( -- )  pollfds >r
    term-sock    POLLIN  r> fds!+ >r
    stdin fileno POLLIN  r> fds!+ drop ;

: open-term ( addr u -- )
    SOCK_DGRAM >hints AF_INET hints ai_family l!
    term-port# get-info info>string sockaddr swap move
    s\" \r" send-term ; \ ping terminal

Variable key$ \ string for all keys in a row

: key-translate ( char -- char' )
    case
	127 of  8  endof
	4   of  -28 throw  endof \ ^d terminates
	dup endcase ;

: read-keys ( -- )   key key-translate key$ c$+!
    BEGIN  key?  WHILE  key key-translate key$ c$+!  REPEAT ;

2variable ptimeout #100000000 0 ptimeout 2!

: clear-events ( -- )  pollfds
    2 0 DO  0 over revents w!  pollfd %size +  LOOP  drop ;

0 Value term-file
$100 Constant term-line#

: term-include ( addr u -- ) r/o open-file throw to term-file
    BEGIN  pad term-line# term-file read-line throw  WHILE
	    pad swap 2dup + #cr swap c! 1+ send-term
	    recv-term 2dup type
	    dup 4 u>= >r 2dup + 4 - 4 " ok\n" str= r> and
	0= UNTIL  ELSE  drop  THEN
    "\3" send-term \ normal flushing enabled
    term-file close-file 0 to term-file throw ;

: term-type ( addr u -- )
    over c@ 2 = IF  1 /string term-include  ELSE  type  THEN ;

: run-term ( -- )  prep-socks ." Gforth UDP terminal" cr
    BEGIN
	clear-events  pollfds 2 ptimeout 0 ppoll
	0> IF
	    pollfds revents w@ POLLIN = IF  recv-term term-type  THEN
	    pollfds [ pollfd %size revents ]L + w@ POLLIN = IF
		read-keys  key$ $@ send-term key$ $off  THEN
	THEN
    AGAIN ;

: do-term ( -- ) ['] run-term catch dup -28 <> and throw ;

script? [IF]
    next-arg open-term do-term bye
[THEN]