s" _number?" 1 func-builtin-add
s" _real?" 1 func-builtin-add
s" _integer?" 1 func-builtin-add

s" _+" -1 func-builtin-add
s" _-" -1 func-builtin-add
s" _*" -1 func-builtin-add
s" _/" -1 func-builtin-add

s" _zero?" 1 func-builtin-add
s" _positive?" 1 func-builtin-add
s" _negative?" 1 func-builtin-add
s" _abs" 1 func-builtin-add

s" _max" -1 func-builtin-add
s" _min" -1 func-builtin-add

s" _round" 1 func-builtin-add
s" _floor" 1 func-builtin-add

s" _expt" 2 func-builtin-add
s" _exp" 1 func-builtin-add
s" _log" 1 func-builtin-add

s" _sin" 1 func-builtin-add
s" _cos" 1 func-builtin-add
s" _tan" 1 func-builtin-add
s" _asin" 1 func-builtin-add
s" _acos" 1 func-builtin-add
s" _atan" 1 func-builtin-add
\ всегда будет float
s" _atan" 2 func-builtin-add
s" _sinh" 1 func-builtin-add
s" _cosh" 1 func-builtin-add
s" _tanh" 1 func-builtin-add
s" _pi" 0 func-builtin-add

\ только для целых чисел
s" _quotient" 2 func-builtin-add
s" _remainder" 2 func-builtin-add

s" _=" -1 func-builtin-add
s" _<" -1 func-builtin-add
s" _<=" -1 func-builtin-add
s" _>" -1 func-builtin-add
s" _>=" -1 func-builtin-add

s" _boolean?" 1 func-builtin-add
s" _not" 1 func-builtin-add

s" _if" 3 func-builtin-add
s" _or" 0 func-builtin-add
s" _or" -1 func-builtin-add
s" _and" 0 func-builtin-add
s" _and" -1 func-builtin-add

s" _list" -1 func-builtin-add
s" _list" 0 func-builtin-add
s" _null?" 1 func-builtin-add
s" _list?" 1 func-builtin-add
s" _car" 1 func-builtin-add
s" _cdr" 1 func-builtin-add
\ результат: список 1 элемент - голова, второй - хвост (только список)
s" _cons" 2 func-builtin-add
\ сделать индексацию, append

s" _begin" -1 func-builtin-add

s" _char?" 1 func-builtin-add
s" _char=?" 2 func-builtin-add
s" _char<?" 2 func-builtin-add
s" _char<=?" 2 func-builtin-add
s" _char>?" 2 func-builtin-add
s" _char>=?" 2 func-builtin-add
s" _char-upcase" 1 func-builtin-add
s" _char->integer" 1 func-builtin-add
s" _integer->char" 1 func-builtin-add

s" _string" -1 func-builtin-add
s" _string?" 1 func-builtin-add
s" _string-length" 1 func-builtin-add
s" _make-string" 1 func-builtin-add
s" _make-string" 2 func-builtin-add
s" _string=?" 2 func-builtin-add
s" _string<?" 2 func-builtin-add
s" _string<=?" 2 func-builtin-add
s" _string>?" 2 func-builtin-add
s" _string>=?" 2 func-builtin-add
s" _string-ref" 2 func-builtin-add
s" _string-set!" 3 func-builtin-add
s" _string-append" -1 func-builtin-add
s" _string->list" 1 func-builtin-add
s" _list->string" 1 func-builtin-add

s" _newline" 0 func-builtin-add
s" _display" 1 func-builtin-add

s" _eq?" 2 func-builtin-add

s" _map" 2 func-builtin-add
s" _for-each" 2 func-builtin-add
s" _apply" 2 func-builtin-add

s" _reverse" 1 func-builtin-add





