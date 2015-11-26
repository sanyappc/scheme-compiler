\ работа с ошибками - начало 

: error-internal 								( addr u -- )
	." error: " type
	cr
;

: error-internal-critical 						( addr u -- )
	error-internal bye
;

: allocate-check 								( f -- addr u )
  0<> if
    s" allocation error" error-internal-critical
  then
;

\ работа с ошибками - конец

\ работа со строками - начало

: s 										( addr u -- addr u )
	{ addr u }
	u allocate allocate-check				( addr2 )
	dup										( addr2 addr2 )
	addr swap u								( addr2 addr1 addr2 u )
	cmove u					
;

: s0 										( -- addr u)
  0 allocate
  allocate-check
  0
;

: s+c 										( addr1 u1 c -- addr2 u2 )
  { addr u c: c }
  addr u s drop
  u 1+ dup -rot resize
  allocate-check
  dup u + c swap c!
  swap
;

: c+s ( addr1 u1 c -- addr2 u2)
  { u c: c }
  u 1+ allocate allocate-check
  dup -rot 1+ u cmove \ add free!
  dup c swap c!
  u 1+
;

: s+s 										( addr2 u2 addr1 u1 -- addr u) \ s1s2
	{ addr2 u2 addr1 u1 }
	u1 u2 + dup								( u u )
	addr1 u1 s drop							( u u addr1' )
	\ addr1 
	swap									( u addr1' u )
	resize									( u addr wior )
	allocate-check							( u addr )
	dup u1 +								( u addr addr' )
	addr2 swap u2 cmove						( u addr )
	swap									( addr u )
;

: i>s ( n -- addr u)
  0 <<# #s #> #>>
;

\ работа со строками - конец

\ работа с файлами - начало

0 value fd-in
0 value fd-out
: open-input ( addr u -- )  r/o open-file throw to fd-in ;
: open-output ( addr u -- )  w/o create-file throw to fd-out ;
: close-input ( -- )  fd-in close-file throw ;
: close-output ( -- )  fd-out close-file throw ;

4 constant eof
false value eof?

8 constant shift

1 value line-start
1 value column-start
1 value line
0 value column
-1 value curr
false value newline?

-1 value current-char

: error ( addr u -- )
	." line: " line-start .
	." col: " column-start .
	." error: " type
	cr
;

: error-critical ( addr u -- )
	error bye
;

: readchar ( -- c )
	newline? if
		false to newline?
		line 1+ to line
		0 to column
	then
	column 1+ to column
	fd-in key-file
	dup 10 = if 
		true to newline? 
	then
	dup eof = if
		true to eof?
	then
;

: readchar! ( -- c )
	readchar 
	dup eof = if
		s" unexpected EOF" error-critical
	then
	dup bl = if
		s" unexpected space" error-critical
	then
	dup 9 = if
		s" unexpected tab" error-critical
	then
	dup 10 = if
		s" unexpected newline" error-critical
	then
; 

: readchar!-end ( -- c )
	readchar 
	dup eof = if
		s" unexpected EOF" error-critical
	then
	dup 10 = if
		s" unexpected newline" error-critical
	then
;

: splitter ( c -- f )
	dup '" = 									( c f )
    over '( = or								
    over ') = or
    over bl = or
    over 10 = or
    over 9 = or
    swap eof = or
;

    
0 value read \ символ уже считан
    
: readword-until-splitter 						( -- addr u )
	s0											( addr u )
	begin
		readchar dup							( addr u c c )
		splitter false =						( addr u c f )
	while										( addr u c )
		s+c										( addr u )
	repeat	
	to read
;

: readword-until-splitter!
	readword-until-splitter
	dup 0= if
		s" unexpected splitter" error-critical
	then
;

: integer? ( addr u -- f )
  s>number? -rot 2drop
  dpl @ -1 = and
;

: float? ( addr u -- f )
  >float dup
  if
    fdrop
  then
;

0 value @bracket-left
1 value @bracket-right
2 value @boolean
3 value @integer
4 value @float
5 value @character
6 value @string
7 value @name
8 value @eof

: readword ( -- addr u type)
	read 0<> if
		read
		0 to read
	else
		readchar
	then
	line to line-start
	column to column-start
	case
		eof of 0 0 @eof endof
		'' of 
			s\" \"'\" is not supported" error-critical 
		endof
		'( of s" (" @bracket-left endof
		') of s" )" @bracket-right endof
		bl of recurse endof
		9 of recurse endof
		10 of recurse endof
		'# of
			readchar
			case
				't of s" -1" @boolean endof 
				'f of s" 0" @boolean endof
				'\ of 
					s" '" readchar! s+c @character
				endof
				'b of 
					readword-until-splitter! '%  c+s 
					2dup integer? true <> if
						s" only integer binaries is supported" error-critical
					then
					@integer
				endof
				'x of
					readword-until-splitter! '$  c+s
					2dup integer? true <> if
						s" only integer hexademical is supported" error-critical
					then
					@integer
				endof
				s" unexpected token" error-critical
			endcase
		endof
		'" of
			s\" s\\\" "
			begin
				readchar!-end s+c					( addr u )
				2dup 2dup						( addr u addr u addr u )
				2 - + c@ '\ <>					( addr u addr u f ) 
				-rot							( addr u f addr u )
				1- + c@ '" = and				( addr u f )
			until
			@string
		endof									( c )
		dup										( c c )
		readword-until-splitter					( c c addr u )
		rot										( c addr u c )
		c+s										( c addr u )
		2dup integer? if
			@integer							( c addr u type )
		else
			2dup float? if
				>float drop 20 15 25 f>str-rdp		( c addr u )
				@float							( c addr u type )
			else
				@name							( c addr u type )
			then
		then
		-rot									( c type addr u )
		2swap swap								( addr u type c )
	endcase
;

: writeword ( addr u -- )
  fd-out write-file 0<>
  s\" \n" fd-out write-file 
  0<> or if
    s" writing to file error" error-internal-critical
  then
;

\ работа с файлами - конец

\ работа с функциями - начало

\ встроенные функции
0 value func-builtin 							( )
0 value func-builtin-next 						( )

\ количетсво аргументов функций
0 value func-n 									( )
0 value func-n-next 							( )

\ добавление количества аргументов функции
: func-n-add 									( eaddr n -- )
	2 allocate allocate-check dup 				( eaddr n naddr naddr )
	2swap rot 2! 								( naddr )
	0 swap										( 0 naddr )
	2 allocate allocate-check dup				( 0 naddr eaddr eaddr)
	2swap rot 2!								( eaddr )
	func-n-next 0= if							( eaddr )
		dup										( eaddr eaddr )
		to func-n								( eaddr )
		shift + 								( eadrr-next )
		to func-n-next							( )
	else
		dup shift +								( eaddr eaddr-next )
		swap									( eaddr-next eaddr )
		func-n-next								( eaddr-next eaddr prev-eaddr-next)
		!										( eaddr-next )
		to func-n-next							( )
	then
;

\ проверка количества аргументов функции
: func-n-find									( eaddr n -- f )
	{ eaddr n }
	func-n										( eaddr )
	begin 
		dup 0<>									( eaddr f )
	while										( eaddr )
		2@										( eaddr-next naddr )
		2@										( eaddr-next eaddr n )
		dup -1 = n 0> and						( eaddr-next eaddr n f )
		-rot									( eaddr-next f eaddr n )
		n =										( eaddr-next f eaddr f )
		swap									( eaddr-next f f eaddr )
		eaddr = -rot or	and						( eaddr-next f )
		if										( eaddr-next )
			drop -1								( true )
			exit
		then									( eaddr-next )
	repeat										( eaddr )
	drop 0										( false )
;

\ добавление встроенных функций
: func-builtin-add 								( saddr su n -- )
	{ n }										( saddr su )
	2 allocate allocate-check dup 				( saddr su naddr naddr )
	2swap rot 2! 								( naddr )
	dup n func-n-add
	0 swap										( 0 naddr )
	2 allocate allocate-check dup				( 0 naddr eaddr eaddr)
	2swap rot 2!								( eaddr )
	func-builtin-next 0= if						( eaddr )
		dup										( eaddr eaddr )
		to func-builtin							( eaddr )
		shift + 								( eadrr-next )
		to func-builtin-next					( )
	else
		dup shift +								( eaddr eaddr-next )
		swap									( eaddr-next eaddr )
		func-builtin-next						( eaddr-next eaddr prev-eaddr-next)
		!										( eaddr-next )
		to func-builtin-next					( )
	then
;

\ поиск по списку встроенных функций
: func-builtin-find								( saddr su n -- f )
	{ saddr su n }
	func-builtin								( eaddr )
	begin 
		dup 0<>									( eaddr f )
	while										( eaddr )
		2@										( eaddr-next naddr )
		dup n func-n-find						( eaddr-next naddr f)
		swap									( eaddr-next f naddr)			
		2@										( eaddr-next f saddr su )
		saddr su compare						( eaddr-next f n )
		0= and if								( eaddr-next )
			drop -1								( true )
			exit
		then									( eaddr-next )
	repeat										( eaddr )
	drop 0										( false )
;

\ список встроенных функций
require new-funcs.fs

\ поиск: s" +" 5 func-builtin-find .s


\ глубина определенных функций
0 value func-deep 								( )
0 value func-deep-next 							( )

\ добавление глубины функции
: func-deep-add 								( eaddr n -- )
	2 allocate allocate-check dup 				( eaddr n naddr naddr )
	2swap rot 2! 								( naddr )
	0 swap										( 0 naddr )
	2 allocate allocate-check dup				( 0 naddr eaddr eaddr)
	2swap rot 2!								( eaddr )
	func-deep-next 0= if						( eaddr )
		dup										( eaddr eaddr )
		to func-deep							( eaddr )
		shift + 								( eadrr-next )
		to func-deep-next						( )
	else
		dup shift +								( eaddr eaddr-next )
		swap									( eaddr-next eaddr )
		func-deep-next							( eaddr-next eaddr prev-eaddr-next)
		!										( eaddr-next )
		to func-deep-next						( )
	then
;

\ проверка глубины функции
: func-deep-find								( eaddr n -- f )
	{ eaddr n }
	func-deep									( eaddr )
	begin 
		dup 0<>									( eaddr f )
	while										( eaddr )
		2@										( eaddr-next naddr )
		2@										( eaddr-next eaddr n )
		dup -1 <> swap							( eaddr-next eaddr f n )
		n <= and								( eaddr-next eaddr f )
		swap									( eaddr-next f eaddr )
		eaddr = and								( eaddr-next f )
		if										( eaddr-next )
			drop -1								( true )
			exit
		then									( eaddr-next )
	repeat										( eaddr )
	drop 0										( false )
;

\ проверка глубины функции
: func-deep-find!								( eaddr n -- [eaddr true | false] )
	{ eaddr n }
	func-deep									( eaddr )
	begin 
		dup 0<>									( eaddr f )
	while										( eaddr )
		2@										( eaddr-next naddr )
		2@										( eaddr-next eaddr n )
		dup -1 <> swap							( eaddr-next eaddr f n )
		n = and									( eaddr-next eaddr f )
		swap									( eaddr-next f eaddr )
		dup eaddr = rot and						( eaddr-next eaddr f )
		if										( eaddr-next eaddr )
			swap drop -1						( eaddr true )
			exit
		then									( eaddr-next eaddr )
		drop
	repeat										( eaddr )
	drop 0										( false )
;

\ удаление глубины функции
: func-deep-delete								( eaddr -- )
	{ eaddr }
	func-deep									( eaddr )
	begin 
		dup 0<>									( eaddr f )
	while										( eaddr )
		2@										( eaddr-next naddr )
		dup										( eaddr-next naddr naddr)
		2@										( eaddr-next naddr eaddr n )
		drop									( eaddr-next naddr eaddr )
		dup eaddr =								( eaddr-next naddr eaddr f )
		if										( eaddr-next naddr eaddr )
			-1 rot 2!							( eaddr-next )
			drop								( )
			exit
		else
			2drop								( eaddr-next )
		then									( eaddr-next )
	repeat										( eaddr )
	drop										( )
;

\ определенные функции
0 value func-defined 							( )
0 value func-defined-next 						( )
0 value func-defined-counter

: func-defined-subfind							( saddr su deep -- )
	{ saddr su deep }
	func-defined								( eaddr )
	begin 
		dup 0<>									( eaddr f )
	while										( eaddr )
		dup @									( eaddr naddr )
		swap shift 2* + @ swap					( eaddr-next naddr )
		dup 2@									( eaddr-next naddr saddr su )
		saddr su compare						( eaddr-next naddr n )
		0= if									( eaddr-next naddr )
			dup deep func-deep-find!			( eaddr-next naddr [eaddr true | false])
			if									( eaddr-next naddr eaddr)
				func-deep-delete				( eaddr-next naddr )
			then								( eaddr-next naddr )
			dup deep func-deep-find
			if
				s" ' already exists"
				saddr su s+s
				s" function '" s+s
				error-critical
			then			
		then									( eaddr-next naddr )
		drop
	repeat										( eaddr )
	drop										( )
;

\ добавление определенных функций
: func-defined-add 								( saddr su n d -- saddr su )
	{ n deep }									( saddr su )
	2dup n func-builtin-find					( saddr su f )
	if
		s"  arguments already exists"
		n i>s s+s 
		s" ' with " s+s 
		2swap s+s
		s" function '" s+s
		error-critical
	then
	2dup deep func-defined-subfind				( saddr su )
	2dup func-defined-counter 1+				( saddr su saddr su i )
	dup to func-defined-counter					( saddr su saddr su i )
	i>s 2swap s+s								( saddr su saddr' su')
	2 allocate allocate-check dup 				( saddr su saddr' su' naddr' naddr' )
	2swap rot 2! 								( saddr su naddr' )
	-rot										( naddr' saddr su )
	2 allocate allocate-check dup 				( naddr' saddr su naddr naddr )
	2swap rot 2! 								( naddr' naddr )
	dup n func-n-add							( naddr' naddr )
	dup deep func-deep-add						( naddr' naddr )
	over swap 									( naddr' naddr' naddr )
	3 allocate allocate-check dup				( naddr' naddr' naddr eaddr eaddr)
	2swap rot 2!								( naddr' eaddr )
	dup shift 2* + 0 swap !						( naddr' eaddr )	
	func-defined-next 0= if						( naddr' eaddr )
		dup										( naddr' eaddr eaddr )
		to func-defined							( naddr' eaddr )
		shift 2* + 								( naddr' eadrr-next )
		to func-defined-next					( naddr' )
	else
		dup shift 2* +							( naddr' eaddr eaddr-next )
		swap									( naddr' eaddr-next eaddr )
		func-defined-next						( naddr' eaddr-next eaddr prev-eaddr-next)
		!										( naddr' eaddr-next )
		to func-defined-next					( naddr' )
	then
	2@
;

\ поиск по списку определенных функций с учетом глубины
: func-defined-find 							( saddr su n d -- [saddr' su' true| false])
	{ saddr su n deep }	
	func-defined								( eaddr )
	begin 
		dup 0<>									( eaddr f )
	while										( eaddr )
		dup										( eaddr eaddr )
		2@										( eaddr naddr' naddr )
		dup n func-n-find						( eaddr naddr' naddr f)
		swap									( eaddr naddr' f naddr)
		dup deep func-deep-find					( eaddr naddr' f naddr f)
		swap									( eaddr naddr' f f naddr)
		2@ saddr su compare 0=					( eaddr naddr' f f f )
		and and
		if 										( eaddr naddr' )
			swap drop							( naddr' )
			2@ -1								( saddr' su' true)
			exit
		then									( eaddr naddr' )
		drop shift 2* + @						( eaddr-next )
	repeat										( eaddr )
	drop 0										( false )	
;

: func-defined-delete							( d -- )
	{ deep }
	func-deep									( eaddr )
	begin 
		dup 0<>									( eaddr f )
	while										( eaddr )
		2@										( eaddr-next naddr )
		dup										( eaddr-next naddr naddr)
		2@										( eaddr-next naddr eaddr n )
		deep >									( eaddr-next naddr eaddr f )
		if										( eaddr-next naddr eaddr )
			-1 rot 2!							( eaddr-next )
			drop								( )
			exit
		else
			2drop								( eaddr-next )
		then									( eaddr-next )
	repeat										( eaddr )
	drop										( )
;

: func-check 									( saddr su n deep -- )
	{ saddr su n deep }
	saddr su n deep func-defined-find 
	dup if 
		-rot 2drop
	then
	saddr su n func-builtin-find or
	0= if
		s"  arguments not found"
		n i>s s+s 
		s" ' with " s+s 
		saddr su s+s
		s" function '" s+s
		error-critical
	then
;

\ аргументы функции
0 value func-arguments 							( )
0 value func-arguments-next 					( )

\ добавление аргументов функций
: func-arguments-add 							( saddr su deep -- )
	{ deep }									( saddr su )
	2 allocate allocate-check dup 				( saddr su naddr naddr )
	2swap rot 2! 								( naddr )
	dup deep func-deep-add
	0 swap										( 0 naddr )
	2 allocate allocate-check dup				( 0 naddr eaddr eaddr)
	2swap rot 2!								( eaddr )
	func-arguments-next 0= if						( eaddr )
		dup										( eaddr eaddr )
		to func-arguments							( eaddr )
		shift + 								( eadrr-next )
		to func-arguments-next					( )
	else
		dup shift +								( eaddr eaddr-next )
		swap									( eaddr-next eaddr )
		func-arguments-next						( eaddr-next eaddr prev-eaddr-next)
		!										( eaddr-next )
		to func-arguments-next					( )
	then
;

\ проверка глубины аргумента функции
: func-arguments-deep-find						( eaddr deep -- f )
	{ eaddr deep }
	func-deep									( eaddr )
	begin 
		dup 0<>									( eaddr f )
	while										( eaddr )
		2@										( eaddr-next naddr )
		2@										( eaddr-next eaddr n )
		dup -1 <> swap							( eaddr-next eaddr f n )
		deep <=	and								( eaddr-next eaddr f )
		swap									( eaddr-next f eaddr )
		eaddr = and								( eaddr-next f )
		if										( eaddr-next )
			drop -1								( true )
			exit
		then									( eaddr-next )
	repeat										( eaddr )
	drop 0										( false )
;

\ поиск по списку аргументов функций
: func-arguments-find							( saddr su deep -- f )
	{ saddr su deep }
	func-arguments								( eaddr )
	begin 
		dup 0<>									( eaddr f )
	while										( eaddr )
		2@										( eaddr-next naddr )
		dup deep func-arguments-deep-find		( eaddr-next naddr f)
		swap									( eaddr-next f naddr)			
		2@										( eaddr-next f saddr su )
		saddr su compare						( eaddr-next f n )
		0= and if								( eaddr-next )
			drop -1								( true )
			exit
		then									( eaddr-next )
	repeat										( eaddr )
	drop 0										( false )
;

: func-arguments-check							( saddr su deep -- )
	{ saddr su deep }
	saddr su 0 deep func-defined-find 
	dup if 
		-rot 2drop
	then
	saddr su 0 func-builtin-find or
	if
		s"  ' shadows name from outer scope"
		saddr su s+s
		s"  argument '" s+s
		error-critical
	then
;

\ работа с функциями - конец

\ s" +" 2 0 func-check  .s
\ s" f" 2 1 func-defined-add type
\ s" f" 2 0 func-defined-add type
\ s" f" 3 5 func-check

\ парсер - начало


\ строка для определений функций
s0
value define-u
value define-addr

\ строка для среды выполнения
s0
value runtime-u
value runtime-addr

0 value deep
true value !bracket-right

0 value define

defer subparse

: subparse-lambda ( addr u -- [addr u true | false] )
	s" lambda" compare
	0= dup
	if
		readword									( f ... type )
		0 { n }
		@bracket-left = if
			2drop
			s0
			begin
				readword
				case 
					@name of
						s" _" s+s
						2dup deep func-arguments-check
						2dup deep func-arguments-add
						s" eval pack { " s+s
						s"  } " 2swap s+s 
					endof
					@bracket-right of
						false to !bracket-right
						2drop
						s0
					endof
					s" unexpected token" error-critical
				endcase	
				!bracket-right							
			while
				n 1+ to n
				2swap s+s
			repeat
			2drop
			true to !bracket-right						( addr u )
		else
			s" unexpected token" error-critical
		then
		define 1+ to define
		func-defined-counter i>s s" _lambda" s+s n deep 1- 
		func-defined-add 2dup 
		{ name-addr name-u }
		s" : " s+s
		s"  recursive drop " 2swap s+s
		s+s
		s"  " 2swap s+s
		s0											( addr u )
		begin										( addr u )
			subparse								( addr u addr' u' )
			!bracket-right							( addr u addr' u' f )
		while										( addr u addr' u' )
			s" eval " 2swap s+s
			2swap s+s								( addr u )
		repeat
		2drop
		true to !bracket-right						( addr u )
		2swap s+s
		s" ; " 2swap s+s
		define-addr define-u s+s
		to define-u
		to define-addr
		s"  @function " name-addr name-u s+s 
		s"  ' " s+s n i>s s+s
		rot
		define 1- to define
	then
;

: subparse-define 								( addr u -- f )
	s" define" compare							( n )
	0= dup 										( f f )
	if											( f )
		readword									( f ... type )
		0 { n }
		s0 { name-addr name-u }
		case
			@name of
				to name-u
				to name-addr
				s0
			endof
			@bracket-left of
				2drop
				readword
				@name = if
					to name-u
					to name-addr
					s0
					begin
						readword
						case 
							@name of
								s" _" s+s
								2dup deep func-arguments-check
								2dup deep func-arguments-add
								s" eval pack { " s+s
								s"  } " 2swap s+s 
							endof
							@bracket-right of
								false to !bracket-right
								2drop
								s0
							endof
							s" unexpected token" error-critical
						endcase	
						!bracket-right							
					while
						n 1+ to n
						2swap s+s
					repeat
					2drop
					true to !bracket-right						( addr u )
				else
					s" unexpected token" error-critical
				then
			endof
			s" unexpected token" error-critical
		endcase
		define 1+ to define
		name-addr name-u s" _" s+s n deep 1- 
		func-defined-add
		s" : " s+s
		s"  recursive drop " 2swap s+s
		s+s
		s"  " 2swap s+s
		s0											( addr u )
		begin										( addr u )
			subparse								( addr u addr' u' )
			!bracket-right							( addr u addr' u' f )
		while										( addr u addr' u' )
			dup 0<> if
				s" eval "
			else
				s" ( define ) " 
			then
			2swap s+s
			2swap s+s								( addr u )
		repeat
		2drop
		true to !bracket-right						( addr u )
		2swap s+s
		s" ; " 2swap s+s
		define-addr define-u s+s
		to define-u
		to define-addr
		define 1- to define
	then
;

: subparse-name									( addr u -- addr' u')
	s" _" s+s
	{ name-addr name-u }						( )
	0 { n }
	s0											( addr u )
	begin										( addr u )
		subparse								( addr u addr' u' )
		!bracket-right							( addr u addr' u' f )
	while										( addr u addr' u' )
		n 1+ to n								( addr u addr' u' )
		s+s										( addr u )
	repeat
	2drop
	true to !bracket-right						( addr u )
	name-addr name-u n deep func-check
	name-addr name-u n deep func-defined-find
	if
		to name-u
		to name-addr
	then										( addr u )
	s"  @function " name-addr name-u s+s 
	define 0= if
		s"  ' "
	else
		s"  ['] "
	then
	s+s n i>s s+s 2swap s+s
;

:noname 										( -- addr u )
	readword
	case
		@eof of 
			2drop 
			s0 
			false to !bracket-right 
		endof
		@boolean of s"  @boolean " 2swap s+s endof
		@integer of s"  @integer " 2swap s+s endof
		@float of s"  @float " 2swap s+s endof
		@character of s"  @character " 2swap s+s endof
		@string of s"  @string " 2swap s+s endof
		@bracket-left of
			2drop								( )
			deep 1+ to deep						( )
			readword							( addr u type )
			case
				@name of
					2dup subparse-define		( addr u f )
					if							( addr u )
						2drop					( )
						s0						( addr u )
					else
						2dup subparse-lambda
						if
							2swap 
							2drop
						else
							subparse-name		( addr u -- addr' u')
						then
					then
				endof
				@bracket-left of
					2drop
					readword
					@name = if
						subparse-lambda
						0= if
							s" unexpected token" error-critical
						then
						0 { n }
						s0						( addr u )
						begin					( addr u )
							subparse			( addr u addr' u' )
							!bracket-right		( addr u addr' u' f )
						while					( addr u addr' u' )
							n 1+ to n			( addr u addr' u' )
							s+s					( addr u )
						repeat
						2drop
						\ n i>s 2swap s+s
						s+s
					else
						s" unexpected token" error-critical
					then
				endof
				s" unexpected token" error-critical
			endcase
		endof
		@bracket-right of 
			deep 1- to deep
			deep func-defined-delete
			false to !bracket-right
			2drop
			s0
		endof
		@name of
			s" _" s+s
			2dup deep func-arguments-find
			if
				s"  unpack " 2swap s+s
			else
				2dup 0 deep func-check
				2dup 0 deep func-defined-find
				if
					2swap
					2drop
				then
				s"  @function " 2swap s+s
				define 0= if
					s"  ' "
				else
					s"  ['] "
				then
				s+s s" 0" s+s
			then
		endof
		s" unexpected token" error-critical
	endcase
;
is subparse

: submain ( -- )
	begin
		eof? true <>
	while
		subparse
		dup 0> if 
			s" eval " 2swap s+s
			runtime-addr runtime-u s+s
			to runtime-u
			to runtime-addr
		else
			2drop
		then
	repeat
	next-arg open-output
	s" require new-api.fs" writeword
	define-addr define-u writeword
	runtime-addr runtime-u writeword
	s\" cr .\" last state: \" cr .\" stack:\" .s cr .\" fstack:\" f.s cr" writeword
	s" bye" writeword
	close-output
;

\ парсер - конец

\ main - начало

: main ( -- )
	next-arg open-input
	submain
	close-input
;

main
bye
