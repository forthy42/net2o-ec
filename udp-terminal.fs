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

: recv& ( -- )
    stacksize4 newtask4 activate
    BEGIN  recv-term type  AGAIN ;

: open-term ( addr u -- )
    SOCK_DGRAM >hints AF_INET hints ai_family l!
    term-port# get-info info>string sockaddr swap move
    s\" .\" hello\" cr\n" send-term recv& ; \ ping terminal
