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

#include <linux/init.h>
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/vmalloc.h>
#include <linux/string.h>
#include <linux/fs.h>
#include <linux/stat.h>
#include <asm/uaccess.h>
#include <linux/elf.h>

#define SCHEMIX_USE_KALLSYMS 1
#if SCHEMIX_USE_KALLSYMS
#include <linux/kallsyms.h>
#include "schemix-structs.h"
#endif

#define SCHEMIX_USE_DEVFS 1
#if SCHEMIX_USE_DEVFS
#include <linux/devfs_fs_kernel.h>
#endif

#define EOF -1

#ifndef _SCHEME_H
#define _SCHEME_H

#ifndef USE_FLATTEN_CASE
# define USE_FLATTEN_CASE 0
#endif

#ifndef FIRST_CELLSEGS
# define FIRST_CELLSEGS 1
#endif

/* Tokens */
#define TOK_EOF     (-1)
#define TOK_LPAREN  0
#define TOK_RPAREN  1
#define TOK_DOT     2
#define TOK_ATOM    3
#define TOK_QUOTE   4
#define TOK_COMMENT 5
#define TOK_DQUOTE  6
#define TOK_BQUOTE  7
#define TOK_COMMA   8
#define TOK_ATMARK  9
#define TOK_SHARP   10
#define TOK_SHARP_CONST 11
#define TOK_VEC     12
#define BACKQUOTE '`'

typedef struct scheme scheme;
typedef struct cell *pointer;

/* Types */
enum scheme_types {
	T_STRING=1,
	T_NUMBER=2,
	T_SYMBOL=3,
	T_PROC=4,
	T_PAIR=5,
	T_CLOSURE=6,
	T_KERNEL_PROCEDURE=7,
	T_CONTINUATION=8,
	T_CHARACTER=9,
	T_PORT=10,
	T_VECTOR=11,
	T_MACRO=12,
	T_PROMISE=13,
	T_ENVIRONMENT=14,
	T_DEVICE=15,
	T_MEMORY_REFERENCE=16,
	T_LAST_SYSTEM_TYPE=16
};

/* ADJ is enough slack to align cells in a TYPE_BITS-bit boundary */
#define ADJ 32
#define TYPE_BITS 5
#define T_MASKTYPE      31    /* 0000000000011111 */
#define T_SYNTAX      4096    /* 0001000000000000 */
#define T_IMMUTABLE   8192    /* 0010000000000000 */
#define T_ATOM       16384    /* 0100000000000000 */   /* only for gc */
#define CLRATOM      49151    /* 1011111111111111 */   /* only for gc */
#define MARK         32768    /* 1000000000000000 */
#define UNMARK       32767    /* 0111111111111111 */

enum scheme_opcodes {
#define _OP_DEF(A,B,C,D,E,OP,HELP) OP,
#include "schemix-opcodes.h"
	OP_MAXDEFINED
};

/***************************** Internals ************************/

enum scheme_port_kind {
	port_free=0,
	port_string=2,
	port_input=16,
	port_output=32
};

typedef struct port {
	unsigned char kind;
	union {
		struct {
			int closeit;
		} stdio;
		struct {
			char *start;
			char *past_the_end;
			char *curr;
		} string;
	} rep;
} port;

enum scheme_num_kind {
	num_long,
};

/* num, for generic arithmetic */
typedef struct num {
	enum scheme_num_kind num_type;
	union {
		long long_value;
	} value;
} num;

typedef void *(*function_t)(void);

/* memory reference */
typedef struct memory_reference {
	unsigned int type; /* this is an index into the structures table */
	void *ptr;
	function_t function;
	unsigned long length;
	pointer dim;
	/* If dim is sc->NIL, this is not an array */
	/* If this is a function, dim is the list of arguments */
} memory_reference;

/* cell structure */
struct cell {
	unsigned int _flag;
	union {
		struct {
			char   *_svalue;
			int   _length;
		} _string;
		num _number;
		memory_reference *_memory_reference;
		scheme *_scheme;
		port *_port;
		struct {
			struct cell *_car;
			struct cell *_cdr;
		} _cons;
	} _object;
};

struct scheme {
	/* devfs stuff */
	char Device_Open;
#if SCHEMIX_USE_DEVFS
	devfs_handle_t devfs_handle;
#else
	int major;
#endif

#define OUTPUT_BUFFER_LENGTH 1024
	char output_buffer_start[OUTPUT_BUFFER_LENGTH];
	char *output_buffer_end;
	char *output_buffer_pos;
#define INPUT_BUFFER_LENGTH 1024
	char input_buffer[INPUT_BUFFER_LENGTH];

	/* return code */
	int retcode;

#define CELL_SEGSIZE    5000  /* # of cells in one segment */
#define CELL_NSEGMENT   10    /* # of segments for cells */
	char *alloc_seg[CELL_NSEGMENT];
	pointer cell_seg[CELL_NSEGMENT];
	int     last_cell_seg;

	pointer args;            /* register for arguments of function */
	pointer envir;           /* stack register for current environment */
	pointer code;            /* register for current code */
	pointer dump;            /* stack register for next evaluation */
	pointer value;           /* return from last opcode executed */

	struct cell _sink;
	pointer sink;            /* when mem. alloc. fails */
	struct cell _NIL;
	pointer NIL;             /* cell representing empty cell */
	struct cell _HASHT;
	pointer T;               /* cell representing #t */
	struct cell _HASHF;
	pointer F;               /* cell representing #f */
	struct cell _EOF_OBJ;
	pointer EOF_OBJ;         /* cell representing end-of-file object */
	pointer oblist;          /* pointer to symbol table */
	pointer global_env;      /* pointer to global environment */
	pointer mem_ref_list;    /* pointer to cache of memory references */

	/* global pointers to special symbols */
	pointer LAMBDA;          /* pointer to syntax lambda */
	pointer MAKE_DEVICE;     /* pointer to syntax make-device */
	pointer QUOTE;           /* pointer to syntax quote */

	pointer QQUOTE;          /* pointer to symbol quasiquote */
	pointer UNQUOTE;         /* pointer to symbol unquote */
	pointer UNQUOTESP;       /* pointer to symbol unquote-splicing */
	pointer FEED_TO;         /* => */
	pointer SHARP_HOOK;      /* *sharp-hook* */
	pointer ZERO;            /* Avoid creating lots of zero objects */

	int DEBUG;               /* print debug messages? */

	pointer free_cell;       /* pointer to top of free cells */
	long    fcells;          /* # of free cells */

	pointer inport;
	pointer outport;
   port lport;
	pointer loadport;

	pointer ON_READ;         /* Event handlers for attached device */
	pointer ON_WRITE;
	pointer ON_SEEK;

	int open_parens;

	char gc_verbose;      /* if gc_verbose is not zero, print gc status */
	char no_memory;       /* Whether mem. alloc. has failed */

	char strbuff[256];

	int tok;
	int print_flag;
	int op;

	long gensym_cnt;

	void *dump_base;      /* pointer to base of allocated dump stack */
	int dump_size;        /* number of frames allocated for dump stack */

	scheme *next;
};

static scheme *list_of_schemes=NULL;

/********************** End of internals ***************************/

static num num_zero;
static num num_one;
static long number_of_mallocs;
static scheme *root_sc=NULL;

#define is_true(p)       ((p) != sc->F)
#define is_false(p)      ((p) == sc->F)
#define is_nil(p)        ((p) == sc->NIL)
#define s_retbool(tf)    s_return(sc,(tf) ? sc->T : sc->F)
#define strvalue(p)      ((p)->_object._string._svalue)
#define strlength(p)     ((p)->_object._string._length)
#define long_value_unchecked(p)       ((p)->_object._number.value.long_value)
#define mem_ref(p)       ((p)->_object._memory_reference)
#define mem_ref_addr(p)  ((p)->_object._memory_reference->ptr)
#define mem_ref_type(p)  ((p)->_object._memory_reference->type)
#define set_integer(p)   (p)->_object._number.num_type=num_long;
#define is_inport(p) (type(p)==T_PORT && p->_object._port->kind&port_input)
#define is_outport(p) (type(p)==T_PORT && p->_object._port->kind&port_output)
#define car(p)           ((p)->_object._cons._car)
#define cdr(p)           ((p)->_object._cons._cdr)
#define procnum(p)       long_value_unchecked(p)
#define cont_dump(p)     cdr(p)
#define setenvironment(p)    typeflag(p) = T_ENVIRONMENT
#define is_atom(p)       (typeflag(p)&T_ATOM)
#define setatom(p)       typeflag(p) |= T_ATOM
#define clratom(p)       typeflag(p) &= CLRATOM
#define is_mark(p)       (typeflag(p)&MARK)
#define setmark(p)       typeflag(p) |= MARK
#define clrmark(p)       typeflag(p) &= UNMARK
#define caar(p)          car(car(p))
#define cadr(p)          car(cdr(p))
#define cdar(p)          cdr(car(p))
#define cddr(p)          cdr(cdr(p))
#define cadar(p)         car(cdr(car(p)))
#define caddr(p)         car(cdr(cdr(p)))
#define cadaar(p)        car(cdr(car(car(p))))
#define cadddr(p)        car(cdr(cdr(cdr(p))))
#define cddddr(p)        cdr(cdr(cdr(cdr(p))))
#define num_long_value(n) (n).value.long_value
#define cons(sc,a,b) _cons(sc,a,b,0)
#define list1(sc,p) _cons(sc,p,sc->NIL,0)
#define immutable_cons(sc,a,b) _cons(sc,a,b,1)
#define typeflag(p)      ((p)->_flag)
#define type(p)          (typeflag(p)&T_MASKTYPE)
#define isdigit(x) is_one_of("1234567890",(x))
#define isspace(x) ((x)==' ' || (x)=='\n' || (x)=='\t')
#define toupper(x) (((x) >= 'a' && (x) <= 'z') ? ((x) + 'A' - 'a') : (x))
#define tolower(x) (((x) >= 'A' && (x) <= 'Z') ? ((x) - 'A' + 'a') : (x))

static num num_add(num a, num b);
static num num_mul(num a, num b);
static num num_div(num a, num b);
static num num_sub(num a, num b);
static num num_rem(num a, num b);
static num num_mod(num a, num b);
static int num_eq(num a, num b);
static int num_gt(num a, num b);
static int num_ge(num a, num b);
static int num_lt(num a, num b);
static int num_le(num a, num b);
static inline int is_string(pointer p) { return (type(p)==T_STRING); }
static inline int is_vector(pointer p) { return (type(p)==T_VECTOR); }
static inline int is_device(pointer p) { return (type(p)==T_DEVICE); }
static void fill_vector(pointer vec, pointer obj);
static pointer vector_elem(pointer vec, int ielem);
static pointer set_vector_elem(pointer vec, int ielem, pointer a);
static inline int is_number(pointer p) { return (type(p)==T_NUMBER); }
static inline int is_integer(pointer p) {
	return ((p)->_object._number.num_type == num_long); }
static inline int is_character(pointer p) { return (type(p)==T_CHARACTER); }
static inline char *string_value(pointer p) { return strvalue(p); }
static inline num nvalue(pointer p) { return ((p)->_object._number); }
static long charvalue(pointer p)  { return long_value_unchecked(p); }
static inline int is_port(pointer p)     { return (type(p)==T_PORT); }
static inline int is_pair(pointer p)     { return (type(p)==T_PAIR); }
static inline int is_symbol(pointer p)   { return (type(p)==T_SYMBOL); }
static char *symname(pointer p)   { return strvalue(car(p)); }
static inline int is_syntax(pointer p)      { return (typeflag(p)&T_SYNTAX); }
static inline int is_proc(pointer p)        { return (type(p)==T_PROC); }
static inline int is_kernel_proc(pointer p) {
	return (type(p)==T_KERNEL_PROCEDURE); }
#if SCHEMIX_USE_KALLSYMS
static memory_reference *kernel_lookup( scheme *sc, char *module, char *name );
static pointer mk_kernel_procedure(scheme *sc, pointer args, pointer name);
static pointer mk_structure(scheme *sc, pointer name);
#endif
static inline int is_memory_reference(pointer p) {
	return (type(p)==T_MEMORY_REFERENCE); }
static inline int is_array(pointer p) {
	return (is_memory_reference(p)
		&& mem_ref(p)->dim != NULL
		&& is_pair(mem_ref(p)->dim)); }
static int is_any(pointer p) { return 1;}
static int is_num_integer(pointer p) {
	return is_number(p) && ((p)->_object._number.num_type==num_long); }
static int is_nonneg(pointer p) {
	return is_num_integer(p) && long_value_unchecked(p)>=0;}
static const char *procname(pointer x);
static inline int is_closure(pointer p)  { return (type(p)==T_CLOSURE); }
static inline int is_macro(pointer p)    { return (type(p)==T_MACRO); }
static pointer closure_code(pointer p)   { return car(p); }
static pointer closure_env(pointer p)    { return cdr(p); }
static inline int is_continuation(pointer p) {
	return (type(p)==T_CONTINUATION); }
/* To do: promise should be forced ONCE only */
static inline int is_promise(pointer p)  { return (type(p)==T_PROMISE); }
static pointer mk_environment(scheme *sc, pointer x, pointer y);
static inline int is_environment(pointer p) {
	return (type(p)==T_ENVIRONMENT); }
static inline int is_immutable(pointer p) { return (typeflag(p)&T_IMMUTABLE); }
static inline void setimmutable(pointer p) { typeflag(p) |= T_IMMUTABLE; }
static char *strlwr(char *s);
static int is_one_of(char *s, int c);
static int alloc_cellseg(scheme *sc, int n);
static long binary_decode(const char *s);
static pointer get_cell(scheme *sc, pointer a, pointer b);
static pointer _get_cell(scheme *sc, pointer a, pointer b);
static pointer get_consecutive_cells(scheme *sc, int n);
static pointer find_consecutive_cells(scheme *sc, int n);
static void finalize_cell(scheme *sc, pointer a);
static int count_consecutive_cells(pointer x, int needed);
static pointer find_slot_in_env(scheme *sc, pointer env, pointer sym, int all);
static pointer mk_number(scheme *sc, num n);
static pointer mk_memory_reference(scheme *sc, memory_reference *ref);
static pointer mk_empty_string(scheme *sc, int len, char fill);
static char *store_string(scheme *sc, int len, const char *str, char fill);
static pointer mk_vector(scheme *sc, int len);
static pointer mk_atom(scheme *sc, char *q);
static pointer mk_sharp_const(scheme *sc, char *name);
static pointer mk_port(scheme *sc, port *p);
static void port_close(scheme *sc, pointer p, int flag);
static void mark(pointer a);
static void gc(scheme *sc, pointer a, pointer b);
static int inchar(scheme *sc);
static void backchar(scheme *sc, int c);
static char *readstr_upto(scheme *sc, char *delim);
static pointer readstrexp(scheme *sc);
static void skipspace(scheme *sc);
static int token(scheme *sc);
static void printslashstring(scheme *sc, char *s, int len);
static void atom2str(scheme *sc, pointer l, int f, char **pp, int *plen);
static void printatom(scheme *sc, pointer l, int f);
static pointer mk_proc(scheme *sc, enum scheme_opcodes op);
static pointer mk_closure(scheme *sc, pointer c, pointer e);
#if SCHEMIX_USE_DEVFS
static pointer mk_device(scheme *sc, pointer name, pointer bindings);
#endif
static pointer mk_continuation(scheme *sc, pointer d);
static pointer reverse(scheme *sc, pointer a);
static pointer reverse_in_place(scheme *sc, pointer term, pointer list);
static pointer append(scheme *sc, pointer a, pointer b);
static int list_length(scheme *sc, pointer a);
static int eqv(pointer a, pointer b);
static void dump_stack_mark(scheme *);


static pointer op_toplevel(scheme *sc, enum scheme_opcodes op);
static pointer op_read(scheme *sc, enum scheme_opcodes op);
static pointer op_gensym(scheme *sc, enum scheme_opcodes op);
static pointer op_print(scheme *sc, enum scheme_opcodes op);
static pointer op_eval(scheme *sc, enum scheme_opcodes op);
static pointer op_apply(scheme *sc, enum scheme_opcodes op);
static pointer op_lambda(scheme *sc, enum scheme_opcodes op);
#if SCHEMIX_USE_DEVFS
static pointer op_device(scheme *sc, enum scheme_opcodes op);
#endif
#if SCHEMIX_USE_KALLSYMS
static pointer op_ctype(scheme *sc, enum scheme_opcodes op);
static pointer op_array(scheme *sc, enum scheme_opcodes op);
#endif
static pointer op_closure(scheme *sc, enum scheme_opcodes op);
static pointer op_quote(scheme *sc, enum scheme_opcodes op);
static pointer op_define(scheme *sc, enum scheme_opcodes op);
static pointer op_debug(scheme *sc, enum scheme_opcodes op);
static pointer op_set(scheme *sc, enum scheme_opcodes op);
static pointer op_begin(scheme *sc, enum scheme_opcodes op);
static pointer op_if(scheme *sc, enum scheme_opcodes op);
static pointer op_let(scheme *sc, enum scheme_opcodes op);
static pointer op_begin(scheme *sc, enum scheme_opcodes op);
static pointer op_let(scheme *sc, enum scheme_opcodes op);
static pointer op_delay(scheme *sc, enum scheme_opcodes op);
static pointer op_logic(scheme *sc, enum scheme_opcodes op);
static pointer op_macro(scheme *sc, enum scheme_opcodes op);
static pointer op_callcc(scheme *sc, enum scheme_opcodes op);
static pointer op_math(scheme *sc, enum scheme_opcodes op);
static pointer op_list(scheme *sc, enum scheme_opcodes op);
static pointer op_char(scheme *sc, enum scheme_opcodes op);
static pointer op_string(scheme *sc, enum scheme_opcodes op);
static pointer op_vector(scheme *sc, enum scheme_opcodes op);
static pointer op_type(scheme *sc, enum scheme_opcodes op);
static pointer op_compare(scheme *sc, enum scheme_opcodes op);
static pointer op_print(scheme *sc, enum scheme_opcodes op);
static pointer op_quit(scheme *sc, enum scheme_opcodes op);
static pointer op_gc(scheme *sc, enum scheme_opcodes op);
static pointer op_port(scheme *sc, enum scheme_opcodes op);
static pointer op_env(scheme *sc, enum scheme_opcodes op);
static pointer op_read(scheme *sc, enum scheme_opcodes op);

static void Eval_Cycle(scheme *sc, enum scheme_opcodes op);
static void assign_syntax(scheme *sc, char *name);
static int syntaxnum(pointer p);
static void assign_proc(scheme *sc, enum scheme_opcodes, char *name);
static int scheme_init(scheme *sc);
static void scheme_deinit(scheme *sc);
static void init_extras(scheme *sc);
static scheme *make_device(scheme *sc, char *name, pointer bindings);
static int scheme_load_string(scheme *sc, char *cmd);
static pointer _cons(scheme *sc, pointer a, pointer b, int immutable);
static pointer mk_integer(scheme *sc, long num);
static pointer mk_symbol(scheme *sc, const char *name);
static pointer gensym(scheme *sc);
static pointer mk_string(scheme *sc, const char *str);
static pointer mk_counted_string(scheme *sc, const char *str, int len);
static pointer mk_character(scheme *sc, int c);
static void putstr(scheme *sc, const char *s);
static void define(scheme *sc, pointer envir, pointer symbol, pointer value);
static pointer eval(scheme *sc, pointer x);
static int device_open( struct inode *inode,
                        struct file *file );
static int device_release( struct inode *inode,
                           struct file *file );
static ssize_t device_read( struct file *file,
                            char *buffer,
                            size_t length,
                            loff_t *offset );
static ssize_t device_write( struct file *file,
                             const char *buffer,
                             size_t length,
                             loff_t *offset );
static int is_string(pointer p);
static char *string_value(pointer p);
static int is_number(pointer p);
static num nvalue(pointer p);
static int is_integer(pointer p);
static int is_character(pointer p);
static long charvalue(pointer p);
static int is_vector(pointer p);
static int is_port(pointer p);
static int is_pair(pointer p);
static int is_symbol(pointer p);
static char *symname(pointer p);
static int is_syntax(pointer p);
static int is_proc(pointer p);
static int is_closure(pointer p);
static int is_macro(pointer p);
static pointer closure_code(pointer p);
static pointer closure_env(pointer p);
static int is_continuation(pointer p);
static int is_promise(pointer p);
static int is_environment(pointer p);
static int is_immutable(pointer p);
static void setimmutable(pointer p);


/* Op-codes */
typedef pointer (*dispatch_func)(scheme *, enum scheme_opcodes);

typedef struct {
	dispatch_func func;
	char *name;
	int min_arity;
	int max_arity;
	char *arg_tests_encoding;
	char *help;
} op_code_info;

typedef int (*test_predicate)(pointer);

/* Correspond carefully with following defines! */
static struct {
	test_predicate fct;
	const char *kind;
} tests[]={
	{0,0}, /* unused */                        /* 0 */
	{is_any, 0},                               /* 1 */
	{is_string, "string"},                     /* 2 */
	{is_symbol, "symbol"},                     /* 3 */
	{is_port, "port"},                         /* 4 */
	{0,"input port"},                          /* 5 */
	{0,"output_port"},                         /* 6 */
	{is_environment, "environment"},           /* 7 */
	{is_pair, "pair"},                         /* 8 */
	{0, "pair or '()"},                        /* 9 */
	{is_character, "character"},               /* 10 */
	{is_vector, "vector"},                     /* 11 */
	{is_number, "number"},                     /* 12 */
	{is_num_integer, "integer"},               /* 13 */
	{is_nonneg, "non-negative integer"},       /* 14 */
	{is_device, "device"},                     /* 15 */
	{is_memory_reference, "memory reference"}, /* 16 */
	{is_array, "array"}                        /* 17 */
};

/* Warning: these are in octal - be careful! */
#define TST_NONE 0
#define TST_ANY "\001"
#define TST_STRING "\002"
#define TST_SYMBOL "\003"
#define TST_PORT "\004"
#define TST_INPORT "\005"
#define TST_OUTPORT "\006"
#define TST_ENVIRONMENT "\007"
#define TST_PAIR "\010"
#define TST_LIST "\011"
#define TST_CHAR "\012"
#define TST_VECTOR "\013"
#define TST_NUMBER "\014"
#define TST_INTEGER "\015"
#define TST_NATURAL "\016"
#define TST_DEVICE "\017"
#define TST_MEMORY_REFERENCE "\020"
#define TST_ARRAY "\021"

#define INF_ARG 0xffff

static op_code_info dispatch_table[]= {
#define _OP_DEF(A,B,C,D,E,OP,HELP) {A,B,C,D,E,HELP},
#include "schemix-opcodes.h"
	{ 0, 0, 0, 0, 0, 0 }
};

#endif
