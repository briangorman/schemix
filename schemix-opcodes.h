/* Schemix - Scheme in the Linux kernel.  Copyright (C) 2003, William Bland.
 *
 * This is a Scheme interpreter for people wanting to explore
 * the Linux kernel.
 * More details: http://www.abstractnonsense.com/schemix
 *
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 *
 *
 * Schemix is based on TinyScheme (http://tinyscheme.sf.net/)
 *
 * ORIGINAL LICENSE OF TINYSCHEME:
 *
 * Copyright (c) 2000, Dimitrios Souflis
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * Neither the name of Dimitrios Souflis nor the names of the
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

_OP_DEF(op_toplevel, 0, 0, 0, 0, OP_T0LVL, "")
_OP_DEF(op_toplevel, 0, 0, 0, 0, OP_REP, "")
_OP_DEF(op_toplevel, 0, 0, 0, 0, OP_T1LVL, "")
_OP_DEF(op_gensym, "gensym", 0, 0, 0, OP_GENSYM,
        "(gensym) returns a new symbol of the form gensym-n, where n is a (decimal) number that is incremented each time gensym is called.")
_OP_DEF(op_print, 0, 0, 0, 0, OP_VALUEPRINT, "")
_OP_DEF(op_print, 0, 0, 0, 0, OP_P0LIST, "")
_OP_DEF(op_print, 0, 0, 0, 0, OP_P1LIST, "")
_OP_DEF(op_print, 0, 0, 0, 0, OP_PVECFROM, "")
_OP_DEF(op_eval, "eval", 1, 2, TST_ANY TST_ENVIRONMENT, OP_PEVAL,
        "(eval expression environment) evaluates expression in the specified environment and returns its value.  Expression must be a valid Scheme expression represented as data, for example the list '(+ 1 2)")
_OP_DEF(op_eval, 0, 0, 0, 0, OP_EVAL, "")
_OP_DEF(op_eval, 0, 0, 0, 0, OP_E0ARGS, "")
_OP_DEF(op_eval, 0, 0, 0, 0, OP_E1ARGS, "")
_OP_DEF(op_apply, "apply", 1, INF_ARG, TST_NONE, OP_PAPPLY, "")
_OP_DEF(op_apply, 0, 0, 0, 0, OP_APPLY, "")
_OP_DEF(op_lambda, 0, 0, 0, 0, OP_LAMBDA, "")
#if SCHEMIX_USE_DEVFS
_OP_DEF(op_device, 0, 0, 0, 0, OP_MAKE_DEVICE, "")
_OP_DEF(op_device, "device?", 1, 1, TST_ANY, OP_DEVICEP,
        "(device? obj) returns #t if obj is a 'device' (an object returned by make-device), and #f otherwise.")
_OP_DEF(op_device, "destroy-device", 1, 1, TST_DEVICE, OP_DESTROY_DEVICE,
        "(destroy-device dev) removes dev's associated entry in the 'devfs' file-system, and then frees up the memory that was used by the attached Scheme interpreter.")
#endif
#if SCHEMIX_USE_KALLSYMS
_OP_DEF(op_ctype, "kernel-lambda", 2, 2, TST_ANY TST_MEMORY_REFERENCE, OP_KERNEL_LAMBDA, "")
_OP_DEF(op_ctype, "make-structure", 1, 1, TST_SYMBOL, OP_MAKE_STRUCTURE,
        "(make-structure struct-name) creates an instance of the C structure specified by struct-name.  The value returned by make-structure is a handle that can be used in subsequent calls to other functions that understand C structures.")
_OP_DEF(op_ctype, "assert-type", 2, 2, TST_ANY TST_MEMORY_REFERENCE, OP_ASSERT_TYPE,
        "(assert-type typespec ref) is used to inform Schemix that the memory-reference ref refers to a C-style object whose type is given by typespec.  Example typespecs are '(int) for a C int, and '(char*) for a C char*.")
_OP_DEF(op_array, "array?", 1, 1, TST_ANY, OP_ARRAYP,
        "(array? obj) returns #t if its argument is an array and #f otherwise.")
_OP_DEF(op_array, "array-rank", 1, 1, TST_ARRAY, OP_ARRAY_RANK,
        "(array-rank array) returns the number of dimensions of array.")
_OP_DEF(op_array, "array-start", 2, 2, TST_ARRAY TST_NUMBER, OP_ARRAY_START,
        "(array-start array k) returns the lower bound for the index along dimension k.")
_OP_DEF(op_array, "array-end", 2, 2, TST_ARRAY TST_NUMBER, OP_ARRAY_END,
        "(array-end array k) returns the upper bound for the index along dimension k.")
_OP_DEF(op_array, "array-ref", 2, INF_ARG, TST_ANY, OP_ARRAY_REF,
        "(array-ref array k ...) returns the contents of the element of array at index k ....  The sequence k ... must be a valid index to array.")
_OP_DEF(op_array, "array-set!", 3, INF_ARG, TST_ANY, OP_ARRAY_SET,
        "(array-set! array k ... obj) stores obj in the element of array at index k ....  The sequence k ... must be a valid index to array.")
#endif
_OP_DEF(op_closure, "make-closure", 1, 2, TST_PAIR TST_ENVIRONMENT, OP_MKCLOSURE, "")
_OP_DEF(op_closure, "get-closure-code", 1, 1, TST_NONE, OP_GET_CLOSURE, "")
_OP_DEF(op_quote, 0, 0, 0, 0, OP_QUOTE, "")
_OP_DEF(op_define, 0, 0, 0, 0, OP_DEF0, "")
_OP_DEF(op_define, 0, 0, 0, 0, OP_DEF1, "")
_OP_DEF(op_define, "defined?", 1, 2, TST_SYMBOL TST_ENVIRONMENT, OP_DEFP,
        "(defined? sym) returns #t if sym has a value, and #f otherwise.")
_OP_DEF(op_debug, "help", 1, 1, TST_ANY, OP_HELP,
        "(help obj) returns a string containing human-readable information on the Scheme object obj.")
_OP_DEF(op_debug, "printk-debugging", 1, 1, TST_ANY, OP_PRINTK_DEBUGGING,
        "(printk-debugging obj) switches printk debugging on if obj is anything other than #f, and switches it off if obj is #f.")
_OP_DEF(op_begin, 0, 0, 0, 0, OP_BEGIN, "")
_OP_DEF(op_if, 0, 0, 0, 0, OP_IF0, "")
_OP_DEF(op_if, 0, 0, 0, 0, OP_IF1, "")
_OP_DEF(op_if, 0, 0, 0, 0, OP_COND0, "")
_OP_DEF(op_if, 0, 0, 0, 0, OP_COND1, "")
_OP_DEF(op_set, 0, 0, 0, 0, OP_SET0, "")
_OP_DEF(op_set, 0, 0, 0, 0, OP_SET1, "")
_OP_DEF(op_let, 0, 0, 0, 0, OP_LET0, "")
_OP_DEF(op_let, 0, 0, 0, 0, OP_LET1, "")
_OP_DEF(op_let, 0, 0, 0, 0, OP_LET2, "")
_OP_DEF(op_let, 0, 0, 0, 0, OP_LET0AST, "")
_OP_DEF(op_let, 0, 0, 0, 0, OP_LET1AST, "")
_OP_DEF(op_let, 0, 0, 0, 0, OP_LET2AST, "")
_OP_DEF(op_let, 0, 0, 0, 0, OP_LET0REC, "")
_OP_DEF(op_let, 0, 0, 0, 0, OP_LET1REC, "")
_OP_DEF(op_let, 0, 0, 0, 0, OP_LET2REC, "")
_OP_DEF(op_delay, 0, 0, 0, 0, OP_DELAY, "")
_OP_DEF(op_delay, "force", 1, 1, TST_ANY, OP_FORCE, "")
_OP_DEF(op_delay, 0, 0, 0, 0, OP_SAVE_FORCED, "")
_OP_DEF(op_logic, "not", 1, 1, TST_NONE, OP_NOT, "")
_OP_DEF(op_logic, 0, 0, 0, 0, OP_AND0, "")
_OP_DEF(op_logic, 0, 0, 0, 0, OP_AND1, "")
_OP_DEF(op_logic, 0, 0, 0, 0, OP_OR0, "")
_OP_DEF(op_logic, 0, 0, 0, 0, OP_OR1, "")
_OP_DEF(op_macro, 0, 0, 0, 0, OP_DOMACRO, "")
_OP_DEF(op_macro, 0, 0, 0, 0, OP_MACRO0, "")
_OP_DEF(op_macro, 0, 0, 0, 0, OP_MACRO1, "")
_OP_DEF(op_if, 0, 0, 0, 0, OP_CASE0, "")
_OP_DEF(op_if, 0, 0, 0, 0, OP_CASE1, "")
_OP_DEF(op_if, 0, 0, 0, 0, OP_CASE2, "")
_OP_DEF(op_callcc, "call-with-current-continuation", 1, 1, TST_NONE, OP_CONTINUATION, "")
_OP_DEF(op_math, "+", 0, INF_ARG, TST_NUMBER, OP_ADD, "")
_OP_DEF(op_math, "-", 1, INF_ARG, TST_NUMBER, OP_SUB, "")
_OP_DEF(op_math, "*", 0, INF_ARG, TST_NUMBER, OP_MUL, "")
_OP_DEF(op_math, "/", 1, INF_ARG, TST_NUMBER, OP_DIV, "")
_OP_DEF(op_math, "remainder", 2, 2, TST_INTEGER, OP_REM, "")
_OP_DEF(op_math, "modulo", 2, 2, TST_INTEGER, OP_MOD, "")
_OP_DEF(op_list, "car", 1, 1, TST_PAIR, OP_CAR, "")
_OP_DEF(op_list, "cdr", 1, 1, TST_PAIR, OP_CDR, "")
_OP_DEF(op_list, "cons", 2, 2, TST_NONE, OP_CONS,
        "(cons obj1 obj2) returns a newly allocated pair whose car is obj1 and whose cdr is obj2.  The pair is guaranteed to be different (in the sense of eqv? - and hence also eq?) from every existing object.")
_OP_DEF(op_list, "set-car!", 2, 2, TST_PAIR TST_ANY, OP_SETCAR, "")
_OP_DEF(op_list, "set-cdr!", 2, 2, TST_PAIR TST_ANY, OP_SETCDR, "")
_OP_DEF(op_list, "reverse", 1, 1, TST_PAIR, OP_REVERSE, "")
_OP_DEF(op_list, "list*", 1, INF_ARG, TST_NONE, OP_LIST_STAR, "")
_OP_DEF(op_list, "append", 0, INF_ARG, TST_NONE, OP_APPEND, "")
_OP_DEF(op_list, "length", 1, 1, TST_LIST, OP_LIST_LENGTH,
        "(length lst) returns the length of the list lst.  By definition, all lists have finite length and are terminated by the empty list.  The length of a list is the number of elements, which is the same as the number of pairs.  The empty list is a special object of its own type (it is not a pair); it has no elements and its length is zero.")
_OP_DEF(op_list, "assq", 2, 2, TST_NONE, OP_ASSQ, "")
_OP_DEF(op_char, "char->integer", 1, 1, TST_CHAR, OP_CHAR2INT, "")
_OP_DEF(op_char, "integer->char", 1, 1, TST_NATURAL, OP_INT2CHAR, "")
_OP_DEF(op_char, "char-upcase", 1, 1, TST_CHAR, OP_CHARUPCASE, "")
_OP_DEF(op_char, "char-downcase", 1, 1, TST_CHAR, OP_CHARDNCASE, "")
_OP_DEF(op_string, "symbol->string", 1, 1, TST_SYMBOL, OP_SYM2STR, "")
_OP_DEF(op_string, "atom->string", 1, 1, TST_ANY, OP_ATOM2STR, "")
_OP_DEF(op_string, "string->symbol", 1, 1, TST_STRING, OP_STR2SYM, "")
_OP_DEF(op_string, "string->atom", 1, 1, TST_STRING, OP_STR2ATOM, "")
_OP_DEF(op_string, "make-string", 1, 2, TST_NATURAL TST_CHAR, OP_MKSTRING, "")
_OP_DEF(op_string, "string-length", 1, 1, TST_STRING, OP_STRLEN, "")
_OP_DEF(op_string, "string-ref", 2, 2, TST_STRING TST_NATURAL, OP_STRREF, "")
_OP_DEF(op_string, "string-set!", 3, 3, TST_STRING TST_NATURAL TST_CHAR, OP_STRSET, "")
_OP_DEF(op_string, "string-append", 0, INF_ARG, TST_STRING, OP_STRAPPEND, "")
_OP_DEF(op_string, "substring", 2, 3, TST_STRING TST_NATURAL, OP_SUBSTR, "")
_OP_DEF(op_vector, "vector", 0, INF_ARG, TST_NONE, OP_VECTOR, "")
_OP_DEF(op_vector, "make-vector", 1, 2, TST_NATURAL TST_ANY, OP_MKVECTOR, "")
_OP_DEF(op_vector, "vector-length", 1, 1, TST_VECTOR, OP_VECLEN, "")
_OP_DEF(op_vector, "vector-ref", 2, 2, TST_VECTOR TST_NATURAL, OP_VECREF, "")
_OP_DEF(op_vector, "vector-set!", 3, 3, TST_VECTOR TST_NATURAL TST_ANY, OP_VECSET, "")
_OP_DEF(op_type, "boolean?", 1, 1, TST_NONE, OP_BOOLP, "")
_OP_DEF(op_type, "eof-object?", 1, 1, TST_NONE, OP_EOFOBJP, "")
_OP_DEF(op_type, "null?", 1, 1, TST_NONE, OP_NULLP, "")
_OP_DEF(op_type, "symbol?", 1, 1, TST_ANY, OP_SYMBOLP, "")
_OP_DEF(op_type, "number?", 1, 1, TST_ANY, OP_NUMBERP, "")
_OP_DEF(op_type, "string?", 1, 1, TST_ANY, OP_STRINGP, "")
_OP_DEF(op_type, "integer?", 1, 1, TST_ANY, OP_INTEGERP, "")
_OP_DEF(op_type, "real?", 1, 1, TST_ANY, OP_REALP, "")
_OP_DEF(op_type, "char?", 1, 1, TST_ANY, OP_CHARP, "")
_OP_DEF(op_type, "port?", 1, 1, TST_ANY, OP_PORTP, "")
_OP_DEF(op_type, "input-port?", 1, 1, TST_ANY, OP_INPORTP, "")
_OP_DEF(op_type, "output-port?", 1, 1, TST_ANY, OP_OUTPORTP, "")
_OP_DEF(op_type, "procedure?", 1, 1, TST_ANY, OP_PROCP, "")
_OP_DEF(op_type, "pair?", 1, 1, TST_ANY, OP_PAIRP, "")
_OP_DEF(op_type, "list?", 1, 1, TST_ANY, OP_LISTP, "")
_OP_DEF(op_type, "environment?", 1, 1, TST_ANY, OP_ENVP, "")
_OP_DEF(op_type, "vector?", 1, 1, TST_ANY, OP_VECTORP, "")
_OP_DEF(op_type, "closure?", 1, 1, TST_NONE, OP_CLOSUREP, "")
_OP_DEF(op_type, "macro?", 1, 1, TST_NONE, OP_MACROP, "")
_OP_DEF(op_compare, "=", 2, INF_ARG, TST_NUMBER, OP_NUMEQ, "")
_OP_DEF(op_compare, "<", 2, INF_ARG, TST_NUMBER, OP_LESS, "")
_OP_DEF(op_compare, ">", 2, INF_ARG, TST_NUMBER, OP_GRE, "")
_OP_DEF(op_compare, "<=", 2, INF_ARG, TST_NUMBER, OP_LEQ, "")
_OP_DEF(op_compare, ">=", 2, INF_ARG, TST_NUMBER, OP_GEQ, "")
_OP_DEF(op_compare, "eq?", 2, 2, TST_ANY, OP_EQ,
        "eq? is similar to eqv? except that in some cases it is capable of discerning distinctions finer than those detectable by eqv?.  eq? and eqv? are guaranteed to have the same behavior on symbols, booleans, the empty list, pairs, procedures, and non-empty strings and vectors.  eq?'s behavior on numbers and characters is implementation-dependent, but it will always return either true or false, and will return true only when eqv? would also return true.")
_OP_DEF(op_compare, "eqv?", 2, 2, TST_ANY, OP_EQV,
        "The `eqv?' procedure defines a useful equivalence relation on objects.  Briefly, (eqv? obj1 obj2) returns #t if obj1 and obj2 should normally be regarded as the same object.  If two objects are eq? then they are eqv?, but the opposite is not true.")
_OP_DEF(op_print, "write", 1, 2, TST_ANY TST_OUTPORT, OP_WRITE, "")
_OP_DEF(op_print, "write-char", 1, 2, TST_CHAR TST_OUTPORT, OP_WRITE_CHAR, "")
_OP_DEF(op_print, "display", 1, 2, TST_ANY TST_OUTPORT, OP_DISPLAY, "")
_OP_DEF(op_print, "newline", 0, 1, TST_OUTPORT, OP_NEWLINE, "")
_OP_DEF(op_print, "error", 1, INF_ARG, TST_NONE, OP_ERR0, "")
_OP_DEF(op_print, 0, 0, 0, 0, OP_ERR1, "")
_OP_DEF(op_quit, 0, 0, 0, 0, OP_QUIT, "")
_OP_DEF(op_quit, "exit", 0, 0, 0, OP_EXIT, "")
_OP_DEF(op_gc, "gc", 0, 0, 0, OP_GC, "")
_OP_DEF(op_gc, "gc-verbose", 0, 1, TST_NONE, OP_GCVERB, "")
_OP_DEF(op_gc, "new-segment", 0, 1, TST_NUMBER, OP_NEWSEGMENT, "")
_OP_DEF(op_gc, "oblist", 0, 0, 0, OP_OBLIST, "")
_OP_DEF(op_port, "current-input-port", 0, 0, 0, OP_CURR_INPORT, "")
_OP_DEF(op_port, "current-output-port", 0, 0, 0, OP_CURR_OUTPORT, "")
_OP_DEF(op_port, "open-input-file", 1, 1, TST_STRING, OP_OPEN_INFILE, "")
_OP_DEF(op_port, "open-output-file", 1, 1, TST_STRING, OP_OPEN_OUTFILE, "")
_OP_DEF(op_port, "open-input-output-file", 1, 1, TST_STRING, OP_OPEN_INOUTFILE, "")
_OP_DEF(op_port, "close-input-port", 1, 1, TST_INPORT, OP_CLOSE_INPORT, "")
_OP_DEF(op_port, "close-output-port", 1, 1, TST_OUTPORT, OP_CLOSE_OUTPORT, "")
_OP_DEF(op_env, "interaction-environment", 0, 0, 0, OP_INT_ENV, "")
_OP_DEF(op_env, "current-environment", 0, 0, 0, OP_CURR_ENV, "")
_OP_DEF(op_read, 0, 0, 0, 0, OP_SKIP_WHITESPACE, "")
_OP_DEF(op_read, 0, 0, 0, 0, OP_READ_INTERNAL, "")
_OP_DEF(op_read, "read", 0, 1, TST_INPORT, OP_READ, "")
_OP_DEF(op_read, "read-char", 0, 1, TST_INPORT, OP_READ_CHAR, "")
_OP_DEF(op_read, "peek-char", 0, 1, TST_INPORT, OP_PEEK_CHAR, "")
_OP_DEF(op_read, "char-ready?", 0, 1, TST_INPORT, OP_CHAR_READY, "")
_OP_DEF(op_read, "set-input-port", 1, 1, TST_INPORT, OP_SET_INPORT, "")
_OP_DEF(op_read, "set-output-port", 1, 1, TST_OUTPORT, OP_SET_OUTPORT, "")
_OP_DEF(op_read, 0, 0, 0, 0, OP_RDSEXPR, "")
_OP_DEF(op_read, 0, 0, 0, 0, OP_RDLIST, "")
_OP_DEF(op_read, 0, 0, 0, 0, OP_RDDOT, "")
_OP_DEF(op_read, 0, 0, 0, 0, OP_RDQUOTE, "")
_OP_DEF(op_read, 0, 0, 0, 0, OP_RDQQUOTE, "")
_OP_DEF(op_read, 0, 0, 0, 0, OP_RDQQUOTEVEC, "")
_OP_DEF(op_read, 0, 0, 0, 0, OP_RDUNQUOTE, "")
_OP_DEF(op_read, 0, 0, 0, 0, OP_RDUQTSP, "")
_OP_DEF(op_read, 0, 0, 0, 0, OP_RDVEC, "")
#undef _OP_DEF
