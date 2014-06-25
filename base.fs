compiletoflash

: spaces 0 ?do space loop ;
: invert  not  inline 1-foldable ;

: count dup 1+ swap c@ ; ( cstr-addr -- addr count )
: bounds ( addr len -- end start ) over + swap inline 2-foldable ;

: octal 8 base ! ;
: sqr ( n -- n^2 ) dup * 1-foldable ;
: star 42 emit ;

: Flamingo cr
."      _" cr
."     ^-)" cr
."      (.._          .._" cr
."       \`\\        (\`\\        (" cr
."        |>         ) |>        |)" cr
." ______/|________ (7 |` ______\|/_______a:f" cr
;


\ Port definitions for Tiva TM4C1294

$40058000 constant PORTA_BASE
$40059000 constant PORTB_BASE
$4005A000 constant PORTC_BASE
$4005B000 constant PORTD_BASE
$4005C000 constant PORTE_BASE
$4005D000 constant PORTF_BASE
$4005E000 constant PORTG_BASE
$4005F000 constant PORTH_BASE
$40060000 constant PORTJ_BASE
$40061000 constant PORTK_BASE
$40062000 constant PORTL_BASE
$40063000 constant PORTM_BASE
$40064000 constant PORTN_BASE
$40065000 constant PORTP_BASE
$40066000 constant PORTQ_BASE

PORTF_BASE $3FC + constant PORTF_DATA ( IO Data Register, all bits selected )
PORTF_BASE $400 + constant PORTF_DIR  ( Pin Direction )
PORTF_BASE $500 + constant PORTF_DR2R ( 2 mA drive current )
PORTF_BASE $504 + constant PORTF_DR4R ( 4 mA )
PORTF_BASE $508 + constant PORTF_DR8R ( 8 mA )
PORTF_BASE $53C + constant PORTF_DR12R ( 12 mA )
PORTF_BASE $50C + constant PORTF_ODR  ( Open Drain )
PORTF_BASE $510 + constant PORTF_PUR  ( Pullup Resistor )
PORTF_BASE $514 + constant PORTF_PDR  ( Pulldown Resistor )
PORTF_BASE $518 + constant PORTF_SLR  ( Slew Rate )
PORTF_BASE $51C + constant PORTF_DEN  ( Digital Enable )

PORTJ_BASE $3FC + constant PORTJ_DATA ( IO Data Register, all bits selected )
PORTJ_BASE $400 + constant PORTJ_DIR  ( Pin Direction )
PORTJ_BASE $500 + constant PORTJ_DR2R ( 2 mA drive current )
PORTJ_BASE $504 + constant PORTJ_DR4R ( 4 mA )
PORTJ_BASE $508 + constant PORTJ_DR8R ( 8 mA )
PORTJ_BASE $53C + constant PORTJ_DR12R ( 12 mA )
PORTJ_BASE $50C + constant PORTJ_ODR  ( Open Drain )
PORTJ_BASE $510 + constant PORTJ_PUR  ( Pullup Resistor )
PORTJ_BASE $514 + constant PORTJ_PDR  ( Pulldown Resistor )
PORTJ_BASE $518 + constant PORTJ_SLR  ( Slew Rate )
PORTJ_BASE $51C + constant PORTJ_DEN  ( Digital Enable )

PORTN_BASE $3FC + constant PORTN_DATA ( IO Data Register, all bits selected )
PORTN_BASE $400 + constant PORTN_DIR  ( Pin Direction )
PORTN_BASE $500 + constant PORTN_DR2R ( 2 mA drive current )
PORTN_BASE $504 + constant PORTN_DR4R ( 4 mA )
PORTN_BASE $508 + constant PORTN_DR8R ( 8 mA )
PORTN_BASE $53C + constant PORTN_DR12R ( 12 mA )
PORTN_BASE $50C + constant PORTN_ODR  ( Open Drain )
PORTN_BASE $510 + constant PORTN_PUR  ( Pullup Resistor )
PORTN_BASE $514 + constant PORTN_PDR  ( Pulldown Resistor )
PORTN_BASE $518 + constant PORTN_SLR  ( Slew Rate )
PORTN_BASE $51C + constant PORTN_DEN  ( Digital Enable )

\ Hardware definitions for Tiva Connected Launchpad

1 1 lshift  constant led-1 \ On Port N Bit 1
1 0 lshift  constant led-2 \ On Port N Bit 0
1 4 lshift  constant led-3 \ On Port F Bit 4
1 0 lshift  constant led-4 \ On Port F Bit 0

1 0 lshift  constant switch-1 \ On Port J Bit 0
1 1 lshift  constant switch-2 \ On Port J Bit 1

: init ( -- )

  \ Set wires for LEDs
  led-1 led-2 or portn_den !  \ LED 1&2 connections as digital lines
  led-1 led-2 or portn_dir !  \ LED 1&2 connections should be outputs
  led-3 led-4 or portf_den !  \ LED 3&4 connections as digital lines
  led-3 led-4 or portf_dir !  \ LED 3&4 connections should be outputs

  \ Set wires for switches
  switch-1 switch-2 or portj_den ! \ Switch connections as digital lines, inputs
  switch-1 switch-2 or portj_pur ! \ Activate pullup resistors for switches

  cr
  Flamingo
  cr
  ." Have a nice day !" cr
;

: switch1? ( -- ? ) switch-1 portj_data bit@ not ;
: switch2? ( -- ? ) switch-2 portj_data bit@ not ;

: switches begin ." Switch 1: " switch1? . ."  Switch 2: " switch2? . cr ?key until ;

: leds ( -- )
  begin
    key
    dup
    case
      [char] 1 of led-1 portn_data xor! endof
      [char] 2 of led-2 portn_data xor! endof
      [char] 3 of led-3 portf_data xor! endof
      [char] 4 of led-4 portf_data xor! endof
    endcase
    27 =
  until
;

: systick ( ticks -- )
    $E000E014 ! \ How many ticks between interrupts ?
  7 $E000E010 ! \ Enable the systick interrupt.
;

: systick-1Hz ( -- ) 16000000 systick ; \ Tick every second with 16 MHz clock

: cornerstone ( Name ) ( -- )
  <builds begin here $3FFF and while 0 h, repeat
  does>   begin dup  $3FFF and while 2+   repeat 
          eraseflashfrom
;

\ If you want to go back to this dictionary state later, type:
\ cornerstone Rewind-to-Basis

: table   cr 11 1 do
                    11 1 do i j * . loop
                    cr
                  loop ;
table

( Roman numerals taken from Leo Brodies "Thinking Forth" )

: create> <builds does> ;
( In ANS, this would simply be done with "create" )

create> romans
  (      ones ) char I c,  char V c,
  (      tens ) char X c,  char L c,
  (  hundreds ) char C c,  char D c,
  ( thousands ) char M c,

0 variable column# ( current_offset )
: ones      0 column# ! ;
: tens      2 column# ! ;
: hundreds  4 column# ! ;
: thousands 6 column# ! ;

: column ( -- address-of-column ) romans column# @ + ;

: .symbol ( offset -- ) column + c@ emit ;
: oner  0 .symbol ;
: fiver 1 .symbol ;
: tener 2 .symbol ;

: oners ( #-of-oners )
  ?dup if 0 do oner loop then ;

: almost ( quotient-of-5 -- )
  oner if tener else fiver then ;

: romandigit ( digit -- )
  5 /mod over 4 = if almost drop else if fiver then oners then ;

: roman ( number -- )
  1000 /mod thousands romandigit
   100 /mod  hundreds romandigit
    10 /mod      tens romandigit
                 ones romandigit ;

: mealtime   19 u<= if
                      ." Fruit salad "
                    else
                      ." Green salad "
                    then
                    ." would be nice !" ;

: mealsforwholeday cr 25 6 do i dup roman ." : " mealtime cr 2 +loop cr ;
mealsforwholeday

: tick  ( -- ) ." Tick" cr ;

: clock ( -- ) 
  ['] tick irq-systick !
  systick-1Hz
  eint
;

\ deferred words, works only on RAM now

: >body ( cfa -- pfa ) $E + ;
: Defer ( "name" -- )  <builds ['] nop , does> @ execute ;
: is ( xt "name" -- )
  ' >body state @ IF  literal, postpone !  ELSE  ! THEN immediate ;

\ Compatibility layer for ANS standard code

: variable> ( x -- ) variable ;  \ Initialised Variable
: variable  ( -- ) 0 variable ;   \ Uninitialised Variable

: 2variable> ( xd -- ) 2variable ;  \ Initialised Variable
: 2variable  ( -- ) 0. 2variable ;   \ Uninitialised Variable

: (create) create ;              \ Create without action
: create <builds does> ;          \ Create with ANS default action

: cells ( n -- n ) 2 lshift inline 1-foldable ;
: cell+ ( n -- n ) 4 +      inline 1-foldable ;

: aligned ( addr -- addr' ) 3 + -4 and inline 1-foldable ;

\ field

: +field ( offset size "name" -- )  <builds over , + does> @ + ;
: cfield: ( u1 "name" -- u2 )  1 +field ;
: field: ( u1 "name" -- u2 )   aligned cell +field ;

\ multitasker

0 0 flashvar-here 3 cells - 3 nvariable boot-task \ boot task has no extra stackspace

boot-task variable> up \ user pointer
: next-task ( -- task )  up @ inline ;
: save-task ( -- save )  up @ cell+ inline ;
: handler ( -- handler ) up @ cell+ cell+ inline ;

: (pause) [ $B430 h, ]  \ push { r4  r5 } to save I and I'
    rp@ sp@ save-task ! \ save return stack and stack pointer
    next-task @ up !    \ switch to next task
    save-task @ sp! rp! \ restore pointers
    unloop ;            \ pop { r4  r5 } to restore the loop registers

$20 cells Constant stackspace \ 32 stack elements for a background task

: task: ( "name" -- )  stackspace cell+ 2* cell+ buffer: ;

: active? ( task -- ? ) \ Checks if a task is currently inside of round-robin list
  next-task
  begin
    ( Task-Address )
    2dup = if 2drop true exit then
    @ dup next-task = \ Stop when end of circular list is reached
  until
  2drop false
;

: (wake) ( task -- ) \ wake a task
    dup active? IF  drop  exit  THEN
    next-task @ over ! next-task ! ;

: wake ( task -- ) dint (wake) eint ;

: activate ( task r:continue -- )
    r> swap >r 0 r@ cell+ cell+ ! \ no handler
    r@ stackspace cell+ cell+ + dup r@ cell+ !
    dup stackspace + tuck 2 cells - swap ! !
    r> wake ;

: multitask ( -- ) ['] (pause) hook-pause ! ;
: singletask ( -- ) ['] nop hook-pause ! ;

: prev-task ( -- addr )
  \ Find the task that has the currently running one in its next field
  next-task begin dup @ up @ <> while @ repeat ;

: sleep [ $BF30 h, ] inline ; \ WFI Opcode, Wait For Interrupt, enters sleep mode

task: lowpower-task

: lowpower& ( -- )
    lowpower-task activate
    begin
	dint next-task dup @ = if eint sleep then
	\ Is this task the only one remaining in round-robin list ?
	pause
    again
;

: stop ( -- ) \ Remove current task from round robin list
    \ Store the "next" of the current task into the "next" field of the previous task
    \ which short-circuits and unlinks the currently running one.
    dint next-task @ prev-task !

    \ Do final task switch out of the current task
    \ which is not linked in anymore in round-robin list.
    pause \ pause contains an eint
;

: kill ( task -- ) activate stop ;

\ --------------------------------------------------
\  Debugging helpers
\ --------------------------------------------------

: tasks ( -- ) \ Show tasks currently in round-robin list
  hook-pause @ singletask \ Stop multitasking as this list may be changed during printout.

  \ Start with current task.
  next-task cr

  begin
    ( Task-Address )
    dup             ." Task Address: " hex.
    dup           @ ." Next Task: " hex.
    dup 1 cells + @ ." Stack: " hex.
    dup 2 cells + @ ." Handler: " hex. cr

    @ dup next-task = \ Stop when end of circular list is reached
  until
  drop

  hook-pause ! \ Restore old state of multitasking
;

\ exception handling

: catch ( x1 .. xn xt -- y1 .. yn throwcode / z1 .. zm 0 )
    [ $B430 h, ]  \ push { r4  r5 } to save I and I'
    sp@ >r handler @ >r rp@ handler !  execute
    r> handler !  rdrop  0 unloop ;
: throw ( throwcode -- )  dup IF
	handler @ 0= IF  stop  THEN \ unhandled error: stop task
	handler @ rp! r> handler ! r> swap >r sp! drop r>
	UNLOOP  EXIT
    ELSE  drop  THEN ;

' nop variable> flush-hook
: term-flush flush-hook @ execute ;
: quit-loop ( -- )  BEGIN  term-flush query interpret ."  ok" cr  AGAIN ;
: quit-catch ( -- )  BEGIN  ['] quit-loop catch
	dup IF  ." Throw: " . cr  ELSE  drop  THEN  AGAIN ;

: sysfault ( -- ) -9 throw ;

: init ( -- )
    init  multitask
    ['] quit-catch hook-quit !
    ['] sysfault irq-fault ! ;

compiletoram
init
quit
