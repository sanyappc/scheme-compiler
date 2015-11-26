0 constant stub

8 constant shift

2 value @boolean
3 value @integer
4 value @float
5 value @character
6 value @string
9 value @list
10 value @function
11 value @unspecified

\ работа с ошибками - начало 

: error-internal 								( addr u -- )
	." error: " type
	cr bye
;

: allocate-check 								( f -- addr u )
  0<> if
    s" allocation error" error-internal
  then
;

\ работа с ошибками - конец

: i>f
	dup
	0>=
	if
		0 d>f
	else
		-1 d>f
	then
;

: pack 											( data[1, 2] type -- addr )
	dup											( data type type )
	case
		@boolean of
			2 allocate allocate-check 			( data type addr )
			dup									( data type addr addr )
			2swap								( addr addr data type )
			rot									( addr data type addr )
			2!									( addr )
		endof
		@integer of
			2 allocate allocate-check 			( data type addr )
			dup									( data type addr addr )
			2swap								( addr addr data type )
			rot									( addr data type addr )
			2!									( addr )
		endof
		@float of
			2 allocate allocate-check 			( type addr f: data )
			dup									( type addr addr f: data )
			rot swap							( addr type addr f: data )
			!									( addr f: data )
			dup shift +							( addr addr' f: data )
			f!									( addr f: )
		endof
		@character of
			2 allocate allocate-check 			( data type addr )
			dup									( data type addr addr )
			2swap								( addr addr data type )
			rot									( addr data type addr )
			2!									( addr )
		endof
		@string of
			3 allocate allocate-check			( data type addr )
			dup									( data type addr addr )
			rot swap							( data addr type addr )
			!									( data-addr data-u addr )
			dup									( data-addr data-u addr addr )
			2swap								( addr addr data-addr data-u )
			rot									( addr data-addr data-u addr )
			shift +								( addr data-addr data-u addr' )
			2!									( addr )
		endof
		@list of
			3 allocate allocate-check			( data type addr )
			dup									( data type addr addr )
			rot swap							( data addr type addr )
			!									( data-tail data-head addr )
			dup									( data-tail data-head addr addr )
			2swap								( addr addr data-tail data-head )
			rot									( addr data-tail data-head addr )
			shift +								( addr data-tail data-head addr' )
			2!									( addr )
		endof
		@unspecified of
			1 allocate allocate-check			( type addr )
			dup									( type addr addr )
			-rot								( addr type addr )
			!									( addr )
		endof
		s" pack: unexpected type of argument" error-internal
	endcase
;

: unpack										( addr -- data[1, 2] type )
	dup											( addr addr )
	8 +	swap									( addr' addr )
	@											( addr' type )
	dup											( addr' type type )
	case
		@boolean of								( addr' type )
			swap @								( type data )
			swap								( data type )
		endof
		@integer of								( addr' type )
			swap @								( type data )
			swap								( data type )
		endof
		@float of								( addr' type )
			swap f@								( type f: data )
		endof
		@character of	
			swap @								( type data )
			swap								( data type )				
		endof
		@string of								( addr' type )
			swap								( type addr' )
			2@									( type data-addr data-u )
			rot									( data-addr data-u type )
		endof
		@list of
			swap								( type addr' )
			2@									( type data-tail data-head )
			rot									( data-tail data-head type )
		endof
		@unspecified of							( addr' type )
			swap 								( type addr' )
			drop
		endof
		s" unpack: unexpected type of argument" error-internal
	endcase
;

: drop-typed recursive							( data type )
	case
		@boolean of
			drop
		endof
		@integer of
			drop
		endof
		@float of
			fdrop
		endof
		@character of		
			drop
		endof
		@string of
			2drop
		endof
		@list of
			2drop
		endof
		@function of
			drop
			0 ?do
				drop-typed
			loop
		endof
		@unspecified of
		endof
		s" drop-typed: unexpected type of argument" error-internal
	endcase
;

: eval 											( type -- type' )
	dup @function =
	if
		drop
		execute
	then
;

: 2_+
	eval pack { x }
	eval pack { y }
	x unpack										
	case 
		@integer of
			y unpack
			case
				@integer of
					+ @integer
				endof
				@float of
					i>f fswap f+ @float
				endof
				s" +: unexpected type" error-internal
			endcase
		endof
		@float of
			y unpack
			case
				@integer of
					i>f f+ @float
				endof
				@float of
					f+ @float
				endof
				s" +: unexpected type" error-internal
			endcase
		endof
		s" +: unexpected type" error-internal
	endcase
;

: _+ 											( ... n -- result type)
	{ n }
	eval
	n 1 ?do
		2_+
	loop
;

: 2_-
	eval pack { x }
	eval pack { y }
	x unpack										
	case 
		@integer of
			y unpack
			case
				@integer of
					- @integer
				endof
				@float of
					i>f fswap f- @float
				endof
				s" unexpected type for '-'" error-internal
			endcase
		endof
		@float of
			y unpack
			case
				@integer of
					i>f f- @float
				endof
				@float of
					f- @float
				endof
				s" unexpected type for '-'" error-internal
			endcase
		endof
		s" unexpected type for '-'" error-internal
	endcase
;

: _- 											( ... n -- result type)
	{ n }
	n 1 = if
		eval
		0 @integer 2_-
	else
		n 1 ?do
			2_-
		loop
	then
;

: 2_*
	eval pack { x }
	eval pack { y }
	x unpack										
	case 
		@integer of
			y unpack
			case
				@integer of
					* @integer
				endof
				@float of
					i>f fswap f* @float
				endof
				s" unexpected type for '*'" error-internal
			endcase
		endof
		@float of
			y unpack
			case
				@integer of
					i>f f* @float
				endof
				@float of
					f* @float
				endof
				s" unexpected type for '*'" error-internal
			endcase
		endof
		s" unexpected type for '*'" error-internal
	endcase
;

: _* 											( ... n -- result type)
	{ n }
	eval
	n 1 ?do
		2_*
	loop
;

: 2_/
	eval pack { x }
	eval pack { y }
	x unpack										
	case 
		@integer of
			y unpack
			case
				@integer of
					i>f i>f fswap f/ @float
				endof
				@float of
					i>f fswap f/ @float
				endof
				s" unexpected type for '/'" error-internal
			endcase
		endof
		@float of
			y unpack
			case
				@integer of
					i>f f/ @float
				endof
				@float of
					f/ @float
				endof
				s" unexpected type for '/'" error-internal
			endcase
		endof
		s" unexpected type for '/'" error-internal
	endcase
;

: _/											( ... n -- result type)
	{ n }
	n 1 = if 
		eval
		case
			@integer of
				i>f 1/f @float
			endof
			@float of
				1/f @float
			endof
			s" /: unexpected type" error-internal
		endcase
	else
		n 1 ?do
			2_/
		loop
	then
;

: _number?											( ... n -- result type)
	drop
	eval
	case
		@integer of
			drop
			true @boolean
		endof
		@float of
			fdrop
			true @boolean
		endof
		drop-typed
		false @boolean
		stub
	endcase
;

: _real?											( ... n -- result type)
	drop
	eval
	case
		@float of
			fdrop
			true @boolean
		endof
		drop-typed
		false @boolean
		stub
	endcase
;

: _integer?											( ... n -- result type)
	drop
	eval
	case
		@integer of
			drop
			true @boolean
		endof
		drop-typed
		false @boolean
		stub
	endcase
;

: _zero?
	drop
	eval
	case
		@integer of
			0= @boolean
		endof
		@float of
			f0= @boolean
		endof
		s" unexpected type for 'zero?'" error-internal
	endcase
;

: _positive?
	drop
	eval
	case
		@integer of
			0> @boolean
		endof
		@float of
			f0> @boolean
		endof
		s" unexpected type for 'positive?'" error-internal
	endcase
;

: _negative?
	drop
	eval
	case
		@integer of
			0< @boolean
		endof
		@float of
			f0< @boolean
		endof
		s" unexpected type for 'negative?'" error-internal
	endcase
;

: _abs
	drop
	eval
	case
		@integer of
			abs @integer
		endof
		@float of
			fabs @float
		endof
		s" unexpected type for 'abs'" error-internal
	endcase
;

: 2_max
	eval pack { x }
	eval pack { y }
	x unpack										
	case 
		@integer of
			y unpack
			case
				@integer of
					max @integer
				endof
				@float of
					i>f fswap fmax @float
				endof
				s" unexpected type for 'max'" error-internal
			endcase
		endof
		@float of
			y unpack
			case
				@integer of
					i>f fmax @float
				endof
				@float of
					fmax @float
				endof
				s" unexpected type for 'max'" error-internal
			endcase
		endof
		s" unexpected type for 'max'" error-internal
	endcase
;

: _max											( ... n -- result type)
	{ n }
	eval
	n 1 ?do
		2_max
	loop
;

: 2_min
	eval pack { x }
	eval pack { y }
	x unpack										
	case 
		@integer of
			y unpack
			case
				@integer of
					min @integer
				endof
				@float of
					i>f fswap fmin @float
				endof
				s" unexpected type for 'min'" error-internal
			endcase
		endof
		@float of
			y unpack
			case
				@integer of
					i>f fmin @float
				endof
				@float of
					fmin @float
				endof
				s" unexpected type for 'min'" error-internal
			endcase
		endof
		s" unexpected type for 'min'" error-internal
	endcase
;

: _min											( ... n -- result type)
	{ n }
	eval
	n 1 ?do
		2_min
	loop
;

: _round
	drop
	eval
	case
		@float of
			fround f>d drop @integer
		endof
		s" unexpected type for 'round'" error-internal
	endcase
;

: _floor
	drop
	eval
	case
		@float of
			floor f>d drop @integer
		endof
		s" unexpected type for 'floor'" error-internal
	endcase
;

: _expt \ x ** y
	drop
	eval pack { x }
	eval pack { y }
	x unpack										
	case 
		@integer of
			y unpack
			case
				@integer of
					swap i>f
					i>f
					f**
					f>d drop
					@integer
				endof
				@float of
					i>f
					fswap
					f**
					@float
				endof
				s" unexpected type for 'expt'" error-internal
			endcase
		endof
		@float of
			y unpack
			case
				@integer of
					i>f
					f** @float
				endof
				@float of
					f** @float
				endof
				s" unexpected type for 'expt'" error-internal
			endcase
		endof
		s" unexpected type for 'expt'" error-internal
	endcase
;

: _exp
	drop
	eval
	case
		@float of
			fexpm1 @float
		endof
		@integer of
			i>f
			fexpm1 @float
		endof
		s" unexpected type for 'exp'" error-internal
	endcase
;

: _log
	drop
	eval
	case
		@float of
			fln @float
		endof
		@integer of
			dup 
			1 = if
				drop 0 @integer
			else
				i>f
				fln @float
			then
		endof
		s" unexpected type for 'log'" error-internal
	endcase
;

: _sin
	drop
	eval
	case
		@integer of
			i>f
			fsin @float
		endof
		@float of
			fsin @float
		endof
		s" unexpected type for 'sin'" error-internal
	endcase
;

: _cos
	drop
	eval
	case
		@integer of
			i>f
			fcos @float
		endof
		@float of
			fcos @float
		endof
		s" unexpected type for 'cos'" error-internal
	endcase
;

: _tan
	drop
	eval
	case
		@integer of
			i>f
			ftan @float
		endof
		@float of
			ftan @float
		endof
		s" unexpected type for 'tan'" error-internal
	endcase
;

: _asin
	drop
	eval
	case
		@integer of
			i>f
			fasin @float
		endof
		@float of
			fasin @float
		endof
		s" unexpected type for 'asin'" error-internal
	endcase
;

: _acos
	drop
	eval
	case
		@integer of
			i>f
			facos @float
		endof
		@float of
			facos @float
		endof
		s" unexpected type for 'acos'" error-internal
	endcase
;

: _atan
	case
		1 of
			eval
			case
				@integer of
					i>f
					fatan @float
				endof
				@float of
					fatan @float
				endof
				s" unexpected type for 'atan'" error-internal
			endcase
		endof
		2 of
			eval pack { x }
			eval pack { y }
			x unpack										
			case 
				@integer of
					y unpack
					case
						@integer of
							swap i>f
							i>f
							fatan2 @float
						endof
						@float of
							i>f
							fswap
							fatan2 @float
						endof
						s" unexpected type for 'atan'" error-internal
					endcase
				endof
				@float of
					y unpack
					case
						@integer of
							i>f
							fatan2 @float
						endof
						@float of
							fatan2 @float
						endof
						s" unexpected type for 'atan'" error-internal
					endcase
				endof
				s" unexpected type for 'atan'" error-internal
			endcase
		endof
		s" unexpected argumnets for 'atan'" error-internal
	endcase
;

: _sinh
	drop
	eval
	case
		@integer of
			i>f
			fsinh @float
		endof
		@float of
			fsinh @float
		endof
		s" unexpected type for 'sinh'" error-internal
	endcase
;

: _cosh
	drop
	eval
	case
		@integer of
			i>f
			fcosh @float
		endof
		@float of
			fcosh @float
		endof
		s" unexpected type for 'cosh'" error-internal
	endcase
;

: _tanh
	drop
	eval
	case
		@integer of
			i>f
			ftanh @float
		endof
		@float of
			ftanh @float
		endof
		s" unexpected type for 'tanh'" error-internal
	endcase
;

: _pi
	drop
	pi @float
;

: _quotient
	drop
	eval pack { x }
	eval pack { y }
	x unpack										
	case 
		@integer of
			y unpack
			case
				@integer of
					/ @integer
				endof
				s" unexpected type for 'quotient'" error-internal
			endcase
		endof
		s" unexpected type for 'quotient'" error-internal
	endcase
;

: _remainder
	drop
	eval pack { x }
	eval pack { y }
	x unpack										
	case 
		@integer of
			y unpack
			case
				@integer of
					/mod drop @integer
				endof
				s" unexpected type for 'remainder'" error-internal
			endcase
		endof
		s" unexpected type for 'remainder'" error-internal
	endcase
;

: _=
	case
		1 of 
			eval
			drop-typed
			true @boolean
		endof
		true { flag }
		1 ?do
			eval pack { x }
			eval pack { y }
			x unpack
			case
				@integer of
					y unpack
					case
						@integer of
							= 
							flag and to flag
						endof
						@float of
							i>f fswap f= 
							flag and to flag
						endof
						s" unexpected type for '='" error-internal
					endcase
				endof
				@float of
					y unpack
					case
						@integer of
							i>f f= 
							flag and to flag
						endof
						@float of
							f=
							flag and to flag
						endof
						s" unexpected type for '='" error-internal
					endcase
				endof
				s" unexpected type for '='" error-internal
			endcase
			y unpack
		loop
		drop-typed
		flag @boolean
		stub
	endcase
;

: _<
	case
		1 of 
			eval
			drop-typed
			true @boolean
		endof
		true { flag }
		1 ?do
			eval pack { x }
			eval pack { y }
			x unpack
			case
				@integer of
					y unpack
					case
						@integer of
							< 
							flag and to flag
						endof
						@float of
							i>f fswap f< 
							flag and to flag
						endof
						s" unexpected type for '<'" error-internal
					endcase
				endof
				@float of
					y unpack
					case
						@integer of
							i>f f< 
							flag and to flag
						endof
						@float of
							f<
							flag and to flag
						endof
						s" unexpected type for '<'" error-internal
					endcase
				endof
				s" unexpected type for '<'" error-internal
			endcase
			y unpack
		loop
		drop-typed
		flag @boolean
		stub
	endcase
;

: _<=
	case
		1 of 
			eval
			drop-typed
			true @boolean
		endof
		true { flag }
		1 ?do
			eval pack { x }
			eval pack { y }
			x unpack
			case
				@integer of
					y unpack
					case
						@integer of
							<= 
							flag and to flag
						endof
						@float of
							i>f fswap f<= 
							flag and to flag
						endof
						s" unexpected type for '<='" error-internal
					endcase
				endof
				@float of
					y unpack
					case
						@integer of
							i>f f<= 
							flag and to flag
						endof
						@float of
							f<=
							flag and to flag
						endof
						s" unexpected type for '<='" error-internal
					endcase
				endof
				s" unexpected type for '<='" error-internal
			endcase
			y unpack
		loop
		drop-typed
		flag @boolean
		stub
	endcase
;

: _>
	case
		1 of 
			eval
			drop-typed
			true @boolean
		endof
		true { flag }
		1 ?do
			eval pack { x }
			eval pack { y }
			x unpack
			case
				@integer of
					y unpack
					case
						@integer of
							> 
							flag and to flag
						endof
						@float of
							i>f fswap f> 
							flag and to flag
						endof
						s" unexpected type for '>'" error-internal
					endcase
				endof
				@float of
					y unpack
					case
						@integer of
							i>f f> 
							flag and to flag
						endof
						@float of
							f>
							flag and to flag
						endof
						s" unexpected type for '>'" error-internal
					endcase
				endof
				s" unexpected type for '>'" error-internal
			endcase
			y unpack
		loop
		drop-typed
		flag @boolean
		stub
	endcase
;

: _>=
	case
		1 of 
			eval
			drop-typed
			true @boolean
		endof
		true { flag }
		1 ?do
			eval pack { x }
			eval pack { y }
			x unpack
			case
				@integer of
					y unpack
					case
						@integer of
							>= 
							flag and to flag
						endof
						@float of
							i>f fswap f>= 
							flag and to flag
						endof
						s" unexpected type for '>='" error-internal
					endcase
				endof
				@float of
					y unpack
					case
						@integer of
							i>f f>= 
							flag and to flag
						endof
						@float of
							f>=
							flag and to flag
						endof
						s" unexpected type for '>='" error-internal
					endcase
				endof
				s" unexpected type for '>='" error-internal
			endcase
			y unpack
		loop
		drop-typed
		flag @boolean
		stub
	endcase
;

: _boolean?											( ... n -- result type)
	drop
	eval
	case
		@boolean of
			drop
			true @boolean
		endof
		drop-typed
		false @boolean
		stub
	endcase
;


: _not												( ... n -- result type)
	drop
	eval
	case
		@boolean of
			invert @boolean
		endof
		s" unexpected type for 'not'" error-internal
	endcase
;


: _if
	drop
	eval
	case
		@boolean of
			case
				true of
					eval 
					pack { x }
					drop-typed
					x unpack
				endof
				false of
					drop-typed
					eval
				endof
				s" unexpected type for 'if'" error-internal
			endcase
		endof
		drop-typed
		eval
		pack { x }
		drop-typed
		x unpack
		stub
	endcase
;

: _and
	false { flag }
	true @boolean pack { v }
	0 ?do
		flag if
			drop-typed
		else
			eval
			dup @boolean =
			if 
				drop
				if
					true @boolean pack to v
				else
					true to flag
					false @boolean pack to v
				then
			else
				pack to v
			then
		then
	loop
	v unpack	
;

: _or
	false { flag }
	false @boolean pack { v }
	0 ?do
		flag if
			drop-typed
		else
			eval
			dup @boolean =
			if
				drop
				if
					true to flag
					true @boolean pack to v
				else
					false @boolean pack to v
				then
			else
				true to flag
				pack to v
			then
		then
	loop
	v unpack
;

: _list
	dup 0<> if
		0 { list-head }
		0 { list-tail }
		0 ?do
			eval pack 
			0 swap @list pack  
			list-head 0= if
				dup to list-head
			then
			list-tail 0<> if
				dup
				list-tail shift 2* + !
			then
			to list-tail
		loop
		list-head unpack
	else
		drop
		0 0 @list
	then
;

: _null?
	drop
	eval
	case
		@list of
			0= if
				drop
				true @boolean
			else
				drop
				false @boolean
			then
		endof
		drop-typed
		false @boolean 
		stub
	endcase
;

: _list?
	drop
	eval
	case 
		@list of
			2drop
			true @boolean
		endof
		drop-typed
		false @boolean
		stub
	endcase
;

: _car
	drop
	eval
	case 
		@list of
			case
				0 of
					s" car: list is empty" error-internal
				endof
				swap drop
				unpack
				stub
			endcase
		endof
		s" unexpected type for 'car'" error-internal
	endcase
;

: _cdr
	drop
	eval
	case 
		@list of
			0<> if
				case
					0 of
						0 0 @list
					endof
					unpack
					stub
				endcase
			else
				s" cdr: empty list" error-internal
			then
		endof
		s" unexpected type for 'cdr'" error-internal
	endcase
;

: _cons
	drop
	eval pack { head }
	eval pack { tail }
	tail head @list	
;

: _begin
	{ n }
	n 1 ?do
		eval
		drop-typed
	loop
	n 0> if
		eval
	then
;

: _char?											( ... n -- result type)
	drop
	eval
	case
		@character of
			drop
			true @boolean
		endof
		drop-typed
		false @boolean
		stub
	endcase
;

: _char=?
	drop
	eval pack { x }
	eval pack { y }
	x unpack
	case
		@character of
			y unpack
			case
				@character of
					= @boolean
				endof
				s" unexpected type for 'char=?'" error-internal
			endcase
		endof
		s" unexpected type for 'char=?'" error-internal
	endcase
;

: _char<?
	drop
	eval pack { x }
	eval pack { y }
	x unpack
	case
		@character of
			y unpack
			case
				@character of
					< @boolean
				endof
				s" unexpected type for 'char<?'" error-internal
			endcase
		endof
		s" unexpected type for 'char<?'" error-internal
	endcase
;

: _char<=?
	drop
	eval pack { x }
	eval pack { y }
	x unpack
	case
		@character of
			y unpack
			case
				@character of
					<= @boolean
				endof
				s" unexpected type for 'char<=?'" error-internal
			endcase
		endof
		s" unexpected type for 'char<=?'" error-internal
	endcase
;

: _char>?
	drop
	eval pack { x }
	eval pack { y }
	x unpack
	case
		@character of
			y unpack
			case
				@character of
					> @boolean
				endof
				s" unexpected type for 'char>?'" error-internal
			endcase
		endof
		s" unexpected type for 'char>?'" error-internal
	endcase
;

: _char>=?
	drop
	eval pack { x }
	eval pack { y }
	x unpack
	case
		@character of
			y unpack
			case
				@character of
					>= @boolean
				endof
				s" unexpected type for 'char>=?'" error-internal
			endcase
		endof
		s" unexpected type for 'char>=?'" error-internal
	endcase
;

: _char-upcase
	drop
	eval
	case
		@character of
			toupper @character
		endof
		s" char-upcase: unexpected type" error-internal
	endcase
;

: _char->integer
	drop
	eval
	case
		@character of
			@integer
		endof
		s" char->integer: unexpected type" error-internal
	endcase

;

: _integer->char
	drop
	eval
	case
		@integer of
			@character
		endof
		s" integer->char: unexpected type" error-internal
	endcase
;

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

: _string?
	drop
	eval
	case 
		@string of
			2drop
			true @boolean
		endof
		drop-typed
		false @boolean
		stub
	endcase
;

: _string
	s0 { s0-addr s0-u }
	0 ?do
		eval
		case 
			@character of
				s0-addr s0-u rot
				s+c
				to s0-u to s0-addr
			endof
			s" string: unexpected type" error-internal
		endcase
	loop
	s0-addr s0-u @string
;

: _string-length
	drop
	eval
	case 
		@string of
			swap drop
			@integer
		endof
		s" string-length: unexpected type" error-internal
	endcase
;

: _make-string
	1 = if
		eval
		case 
			@integer of
				dup 
				allocate allocate-check swap
				@string
			endof
			s" make-string: unexpected type" error-internal
		endcase
	else
		eval
		case 
			@integer of
				case
					@character of
						{ character }
						dup							( n n )
						allocate allocate-check		( n addr )
						swap 2dup					( addr n addr n )
						character fill				( addr n )
						@string
					endof
				s" make-string: unexpected type" error-internal
				endcase
			endof
			s" make-string: unexpected type" error-internal
		endcase
	then
;

: _string=?
	drop
	eval pack { x }
	eval pack { y }
	x unpack
	case
		@string of
			y unpack
			case
				@string of
					str= @boolean
				endof
				s" string=?: unexpected type" error-internal
			endcase
		endof
		s" string=?: unexpected type" error-internal
	endcase
;

: _string<?
	drop
	eval pack { x }
	eval pack { y }
	x unpack
	case
		@string of
			y unpack
			case
				@string of
					compare @boolean
				endof
				s" string<?: unexpected type" error-internal
			endcase
		endof
		s" string<?: unexpected type" error-internal
	endcase
;

: _string<=?
	drop
	eval pack { x }
	eval pack { y }
	x unpack
	case
		@string of
			y unpack
			case
				@string of
					compare dup 0= or @boolean
				endof
				s" string<=?: unexpected type" error-internal
			endcase
		endof
		s" string<=?: unexpected type" error-internal
	endcase
;

: _string>?
	drop
	eval pack { x }
	eval pack { y }
	x unpack
	case
		@string of
			y unpack
			case
				@string of
					compare 1 = @boolean
				endof
				s" string>?: unexpected type" error-internal
			endcase
		endof
		s" string>?: unexpected type" error-internal
	endcase
;

: _string>=?
	drop
	eval pack { x }
	eval pack { y }
	x unpack
	case
		@string of
			y unpack
			case
				@string of
					compare dup 1 = swap 0= or @boolean
				endof
				s" string>=?: unexpected type" error-internal
			endcase
		endof
		s" string>=?: unexpected type" error-internal
	endcase
;

: _string-ref
	drop
	eval pack { string }
	eval pack { k }
	string unpack
	case
		@string of
			k unpack
			case
				@integer of
					dup
					-rot
					>
					if
						+ c@ @character
					else
						s" string-ref: index out of range" error-internal
					then
				endof
				s" string-ref: unexpected type" error-internal
			endcase
		endof
		s" string-ref: unexpected type" error-internal
	endcase
;

: _string-set!
	drop
	eval pack { string }
	eval pack { k }
	eval pack { character }
	string unpack 
	case
		@string of									( addr u )
			k unpack
			case
				@integer of							( addr u k )
					dup								( addr u k k )
					-rot							( addr k u k )
					>								( addr k )
					if
						+							( addr' )
						character unpack
						case
							@character of			( addr' c )
								swap				( c addr' )
								c!					( )
							endof
							s" string-set!: unexpected type" error-internal
						endcase
					else
						s" string-set!: index out of range" error-internal
					then
				endof
				s" string-set!: unexpected type" error-internal
			endcase
		endof
		s" string-set!: unexpected type" error-internal
	endcase
	string unpack
;

: _string-append 
	s0 { s0-addr s0-u }
	0 ?do
		eval
		case 
			@string of
				s0-addr s0-u s+s
				to s0-u
				to s0-addr
			endof
			s" string-append: unexpected type" error-internal
		endcase
	loop
	s0-addr s0-u @string
;

: _string->list
	drop
	0 { list-head }
	0 { list-tail }
	eval
	case
		@string of									( addr u )
			dup
			0= if
				2drop
				0 0 @list
			else
				0 ?do									( addr )
					dup									( addr addr )
					c@									( addr c )
					@character pack						( addr c-packed )
					0 swap @list pack					( addr list-packed )
					list-head 0= if
						dup to list-head
					then
					list-tail 0<> if
						dup
						list-tail shift 2* + !
					then
					to list-tail
					1+
				loop
				drop
				list-head unpack
			then
		endof
		s" string->list: unexpected type" error-internal
	endcase
;

: _list->string
	drop
	eval
	case
		@list of
			dup 
			0<> if									( addr-tail addr-head )
				s0 { s0-addr s0-u }
				begin								( addr-tail addr-head )
					dup 0<>							( addr-tail addr-head )
				while
					unpack
					case
						@character of
							s0-addr s0-u 
							rot s+c
							to s0-u
							to s0-addr				( addr-tail )
							dup 0<> if
								unpack
								@list <> if
									s" list->string: unexpected type" error-internal
								then
							else
								0
							then
						endof
						s" list->string: unexpected type" error-internal
					endcase
				repeat
				2drop
				s0-addr s0-u @string
			else
				2drop
				s0 @string
			then
		endof
		s" list->string: unexpected type" error-internal
	endcase
;

: _newline
	drop
	cr
	@unspecified
;

: _display recursive
	drop
	eval
	case
		@boolean of
			if
				." #t "
			else
				." #f "
			then
		endof
		@integer of
			.
		endof
		@float of
			f.
		endof
		@character of
			emit space
		endof
		@string of
			type space
		endof
		@list of
			." ("
			begin 
				dup 0<>
			while
				unpack stub _display
				drop
				dup 0<> if
					unpack
					@list <> if
						s" display: unexpected type" error-internal
					then
				else
					0
				then
			repeat
			2drop
			." ) "
		endof
		@unspecified of
			." unspecified "
		endof
		s" display: unexpected type of argument" error-internal
	endcase
	@unspecified
;

: _eq? recursive
	drop
	eval pack { x }
	eval pack { y }
	x unpack
	case
		@boolean of
			y unpack
			case 
				@boolean of
					= @boolean
				endof
				drop-typed
				@boolean drop-typed
				false @boolean
				stub
			endcase
		endof
		@integer of
			y unpack
			case 
				@integer of
					= @boolean
				endof
				drop-typed
				@integer drop-typed
				false @boolean
				stub
			endcase
		endof
		@float of
			y unpack
			case 
				@float of
					f= @boolean
				endof
				drop-typed
				@float drop-typed
				false @boolean
				stub
			endcase
		endof
		@character of
			y unpack
			case 
				@character of
					= @boolean
				endof
				drop-typed
				@character drop-typed
				false @boolean
				stub
			endcase
		endof
		@string of
			y unpack
			case 
				@string of
					str= @boolean
				endof
				drop-typed
				@string drop-typed
				false @boolean
				stub
			endcase
		endof
		@list of
			y unpack 
			case
				@list of
					true { flag }
					{ y-next y-curr }
					{ x-next x-curr }
					begin
						x-curr 0<>
						y-curr 0<>
						and
						flag
						and
					while
						x-curr unpack
						y-curr unpack
						stub _eq? drop
						flag and to flag
						x-next 0<> if
							x-next unpack
							@list <> if
								s" eq?: unexpected type" error-internal
							then
							to x-curr
							to x-next
						else
							0 to x-curr
						then
						y-next 0<> if
							y-next unpack
							@list <> if
								s" eq?: unexpected type" error-internal
							then
							to y-curr
							to y-next
						else
							0 to y-curr
						then
					repeat
					x-curr 0= y-curr 0= and flag and 
					@boolean
				endof
				drop-typed
				@list drop-typed
				false @boolean
				stub
			endcase
		endof
		@unspecified of
			y unpack
			case 
				@unspecified of
					true @boolean
				endof
				drop-typed
				false @boolean
				stub
			endcase
		endof
		s" eq?: unexpected type of argument" error-internal
	endcase
;

: _map
	drop
	case 
		@function of
				{ xt }
				1 <> if
					s" map: unexpected type for first argument" error-internal
				then
				eval
				case 
					@list of
						dup								( list-tail list-head list-head)
						0<> if
							0 0 { list-head list-tail }
							begin						( list-tail list-head )
								dup 0<>					( list-tail list-head )						
							while
								unpack 					( list-tail data[1, 3] )
								1 xt @function eval		( list-tail data-result[1, 3] )
								pack					( list-tail data-result-packed )
								0 swap @list pack		( list-tail list-head-packed )
								list-head 0= if			( list-tail list-head-packed )
									dup to list-head	( list-tail list-head-packed )
								else
									dup 				( list-tail list-head-packed list-head-packed )
									list-tail !         ( list-tail list-head-packed )
								then
								shift 2* + to list-tail	( list-tail )
								dup						( list-tail list-tail )
								0<> if					( list-tail )
									unpack
									@list <> if
									s" map: unexpected type" error-internal
									then				( list-tail-next list-head-next )	
								else
									0
								then
							repeat
							2drop
							list-head unpack
						else
							@list
						then
					endof
					s" map: unexpected type for second argument" error-internal
				endcase
		endof
		s" map: unexpected type for first argument" error-internal
	endcase
;

: _for-each
	drop
	case 
		@function of
				{ xt }
				1 <> if
					s" for-each: unexpected type for first argument" error-internal
				then
				eval
				case 
					@list of						( list-tail list-head )
					begin
						dup 0<>						( list-tail list-head )
					while
						unpack
						1 xt @function eval
						drop-typed
						dup							( list-tail list-tail )
						0<> if						( list-tail )
							unpack
							@list <> if
								s" for-each: unexpected type" error-internal
							then					( list-tail-next list-head-next )				 
						else
							0
						then
					repeat
					2drop
					@unspecified
					endof
					s" for-each: unexpected type for second argument" error-internal
				endcase
		endof
		s" for-each: unexpected type for first argument" error-internal
	endcase
;

: reverse
 0 u+do i roll loop ;

: _apply
	drop
	case
		@function of
			{ n xt }
			eval
			case 
				@list of							( list-tail list-head )
					0 { list-prev }
					0 { m }
					begin
						dup 0<>
					while							( list-tail list-head )
						list-prev swap @list					
						pack to list-prev
						m 1+ dup
						n > if
							s" apply: unexpected count of arguments" error-internal
						then
						to m
						dup 0<> if
							unpack
							@list <> if
								s" apply: unexpected type" error-internal
							then
						else
							0
						then
					repeat
					2drop
					m n <> if
						s" apply: unexpected count of arguments" error-internal
					then
					m 0> if
						list-prev unpack
					else
						0 0 @list
					then
					drop							( list-tail list-head )
					begin
						dup 0<>
					while
						swap { list-tail }			( list-head )
						unpack
						list-tail
						dup 0<> if
							unpack
							@list <> if
								s" apply: unexpected type" error-internal
							then
						else
							0
						then
					repeat
					2drop
					n xt @function eval			
				endof
			s" apply: unexpected type for second argument" error-internal
			endcase
		endof
		s" apply: unexpected type for first argument" error-internal
	endcase
;

: _reverse
	drop
	eval
	case 
		@list of
			0 { list-prev }
			begin
				dup 0<>
			while							
				list-prev swap @list					
				pack to list-prev
				dup 0<> if
					unpack
					@list <> if
						s" reverse: unexpected type" error-internal
					then
				else
					0
				then
			repeat
			2drop
			list-prev 0<> if
				list-prev unpack
			else
				0 0 @list
			then	
			
		endof
		s" reverse: unexpected type" error-internal
	endcase
;








