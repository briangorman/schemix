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

#include "schemix.h"

static void *malloc( scheme *sc, long size )
{
	static long total=0;
	void *toret = vmalloc(size);
	number_of_mallocs++;
	total += size;
	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "Schemix: %s malloc %ld bytes (%ld total)\n",
		       toret ? "success on" : "DID NOT SUCCESSFULLY",
		       size,
             total );
	return toret;
}

static void free( scheme *sc, void *ptr )
{
	number_of_mallocs--;
	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "Schemix freeing memory.  Mallocs=%ld\n",
		       number_of_mallocs);
	vfree(ptr);
}

static pointer copy(scheme *dest, scheme *src, pointer obj)
{
	memory_reference *ref;

	if(obj == NULL)
		return NULL;
	else if( obj == src->NIL )
		return dest->NIL;

	switch( type(obj) )
	{
	case T_STRING:
		return obj;
	case T_NUMBER:
		return obj;
	case T_SYMBOL:
		return mk_symbol(dest, symname(obj));
	case T_PROC:
		return obj;
	case T_PAIR:
		/* FIXME: Do this without recursion?
		   don't want to smash the stack! */
		return cons(dest,
			    copy(dest,src,car(obj)),
			    copy(dest,src,cdr(obj)));
	case T_CLOSURE:
		return mk_closure(dest,
				  copy(dest,src,closure_code(obj)),
				  copy(dest,src,closure_env(obj)));
#if SCHEMIX_USE_KALLSYMS
	case T_KERNEL_PROCEDURE:
		return obj;
#endif
	case T_CONTINUATION:
		return obj;
	case T_CHARACTER:
		return obj;
	case T_PORT:
		return obj;
	case T_VECTOR:
		return obj;
	case T_MACRO:
		return obj;
	case T_PROMISE:
		return obj;
	case T_ENVIRONMENT:
		return mk_environment(dest,
				      copy(dest,src,car(obj)),
				      copy(dest,src,cdr(obj)));
	case T_MEMORY_REFERENCE:
		ref = malloc( dest, sizeof(memory_reference) );
		ref->type = mem_ref(obj)->type;
		ref->ptr = mem_ref(obj)->ptr;
		ref->length = mem_ref(obj)->length;
		ref->dim = copy(dest,src,mem_ref(obj)->dim);
		return mk_memory_reference( dest, ref );
	default:
		return obj;
	}
}

static pointer help(scheme *sc, pointer obj)
{
	switch( type(obj) )
	{
	case T_STRING:
		return mk_string(sc, "String");
	case T_NUMBER:
		return mk_string(sc, "Number");
	case T_SYMBOL:
		return mk_string(sc, "Symbol");
	case T_PROC:
		return mk_string(sc, dispatch_table[procnum(obj)].help );
	case T_PAIR:
		return mk_string(sc, "Pair");
	case T_CLOSURE:
		return mk_string(sc, "Closure");
#if SCHEMIX_USE_KALLSYMS
	case T_KERNEL_PROCEDURE:
		return mk_string(sc, "Kernel procedure");
#endif
	case T_CONTINUATION:
		return mk_string(sc, "Continuation");
	case T_CHARACTER:
		return mk_string(sc, "Character");
	case T_PORT:
		return mk_string(sc, "Port");
	case T_VECTOR:
		return mk_string(sc, "Vector");
	case T_MACRO:
		return mk_string(sc, "Macro");
	case T_PROMISE:
		return mk_string(sc, "Promise");
	default:
		return mk_string(sc, "No help available!");
	}
}

static num num_add(num a, num b)
{
	num ret;
	ret.num_type = num_long;
	ret.value.long_value = a.value.long_value + b.value.long_value;
	return ret;
}

static num num_mul(num a, num b)
{
	num ret;
	ret.num_type = num_long;
	ret.value.long_value = a.value.long_value * b.value.long_value;
	return ret;
}

static num num_div(num a, num b)
{
	num ret;
	ret.num_type = num_long;
	ret.value.long_value = a.value.long_value / b.value.long_value;
	return ret;
}

static num num_sub(num a, num b)
{
	num ret;
	ret.num_type = num_long;
	ret.value.long_value = a.value.long_value - b.value.long_value;
	return ret;
}

static num num_rem(num a, num b)
{
	num ret;
	long e1, e2, res;
	ret.num_type = num_long;
	e1=num_long_value(a);
	e2=num_long_value(b);
	res=e1%e2;
	if(res*e1<0) {   /* remainder should have same sign as first operand */
		if(e2<0)
			e2=-e2;
		if(res>0) {
			res-=e2;
		} else {
			res+=e2;
		}
	}
	ret.value.long_value=res;
	return ret;
}

static num num_mod(num a, num b)
{
	num ret;
	long e1, e2, res;
	ret.num_type = num_long;
	e1=num_long_value(a);
	e2=num_long_value(b);
	res=e1%e2;
	if(res*e2<0) {    /* modulo should have same sign as second operand */
		if(e2<0)
			e2=-e2;
		if(res>0) {
			res-=e2;
		} else {
			res+=e2;
		}
	}
	ret.value.long_value=res;
	return ret;
}

static int num_eq(num a, num b)
{
	return (a.value.long_value==b.value.long_value);
}


static int num_gt(num a, num b)
{
	return (a.value.long_value>b.value.long_value);
}

static int num_ge(num a, num b)
{
	return !num_lt(a,b);
}

static int num_lt(num a, num b)
{
	return (a.value.long_value<b.value.long_value);
}

static int num_le(num a, num b)
{
	return !num_gt(a,b);
}

static long binary_decode(const char *s)
{
	long x=0;

	while(*s!=0 && (*s=='1' || *s=='0')) {
		x<<=1;
		x+=*s-'0';
		s++;
	}

	return x;
}

/* allocate new cell segment */
static int alloc_cellseg(scheme *sc, int n)
{
	pointer newp;
	pointer last;
	pointer p;
	char *cp;
	long i;
	int k;
	unsigned int adj=ADJ;

	if(adj<sizeof(struct cell)) {
		adj=sizeof(struct cell);
	}

	for (k = 0; k < n; k++) {
		if (sc->last_cell_seg >= CELL_NSEGMENT - 1)
			return k;
		cp = (char*)malloc( sc,
				    CELL_SEGSIZE * sizeof(struct cell)+adj );
		if (cp == 0)
			return k;
		i = ++sc->last_cell_seg ;
		sc->alloc_seg[i] = cp;
		/* adjust in TYPE_BITS-bit boundary */
		if(((int)cp)%adj!=0) {
			cp=(char*)(adj*((long)cp/adj+1));
		}
		/* insert new segment in address order */
		newp=(pointer)cp;
		sc->cell_seg[i] = newp;
		while (i > 0 && sc->cell_seg[i - 1] > sc->cell_seg[i]) {
			p = sc->cell_seg[i];
			sc->cell_seg[i] = sc->cell_seg[i - 1];
			sc->cell_seg[--i] = p;
		}
		sc->fcells += CELL_SEGSIZE;
		last = newp + CELL_SEGSIZE - 1;
		for (p = newp; p <= last; p++) {
			typeflag(p) = 0;
			cdr(p) = p + 1;
			car(p) = sc->NIL;
		}
		/* insert new cells in address order on free list */
		if (sc->free_cell == sc->NIL || p < sc->free_cell) {
			cdr(last) = sc->free_cell;
			sc->free_cell = newp;
		} else {
			p = sc->free_cell;
			while (cdr(p) != sc->NIL && newp > cdr(p))
				p = cdr(p);
			cdr(last) = cdr(p);
			cdr(p) = newp;
		}
	}
	return n;
}

static pointer get_cell(scheme *sc, pointer a, pointer b)
{
	pointer x = sc->NIL;

	if (sc->free_cell != sc->NIL) {
		x = sc->free_cell;
		sc->free_cell = cdr(x);
		--sc->fcells;
		return (x);
	}
	return _get_cell (sc, a, b);
}


/* get new cell - the gc must not reclaim a or b. */
static pointer _get_cell(scheme *sc, pointer a, pointer b)
{
	pointer x;

	if(sc->no_memory) {
		return sc->sink;
	}

	if (sc->free_cell == sc->NIL) {
		gc(sc,a, b);
		if (sc->fcells < sc->last_cell_seg*8
		    || sc->free_cell == sc->NIL) {
			/* if only a few recovered,
			   get more to avoid fruitless gc's */
			if (!alloc_cellseg(sc,1) && sc->free_cell == sc->NIL) {
				sc->no_memory=1;
				return sc->sink;
			}
		}
	}
	x = sc->free_cell;
	sc->free_cell = cdr(x);
	--sc->fcells;
	return (x);
}

static pointer get_consecutive_cells(scheme *sc, int n)
{
	pointer x;

	if(sc->no_memory) {
		return sc->sink;
	}

	/* Are there any cells available? */
	x=find_consecutive_cells(sc,n);
	if (x == sc->NIL) {
		/* If not, try gc'ing some */
		gc(sc, sc->NIL, sc->NIL);
		x=find_consecutive_cells(sc,n);
		if (x == sc->NIL) {
			/* If there still aren't, try getting more heap */
			if (!alloc_cellseg(sc,1)) {
				sc->no_memory=1;
				return sc->sink;
			}
		}
		x=find_consecutive_cells(sc,n);
		if (x == sc->NIL) {
			/* If all fail, report failure */
			sc->no_memory=1;
			return sc->sink;
		}
	}
	return (x);
}

static int count_consecutive_cells(pointer x, int needed)
{
	int n=1;
	while(cdr(x)==x+1) {
		x=cdr(x);
		n++;
		if(n>needed) return n;
	}
	return n;
}

static pointer find_consecutive_cells(scheme *sc, int n)
{
	pointer *pp;
	pointer x;
	int cnt;

	pp=&sc->free_cell;
	while(*pp!=sc->NIL) {
		cnt=count_consecutive_cells(*pp,n);
		if(cnt>=n) {
			x=*pp;
			*pp=cdr(*pp+n-1);
			sc->fcells -= n;
			return x;
		}
		pp=&cdr(*pp+cnt-1);
	}
	return sc->NIL;
}

/* get new cons cell */
static pointer _cons(scheme *sc, pointer a, pointer b, int immutable)
{
	pointer x = get_cell(sc,a, b);

	typeflag(x) = T_PAIR;
	if(immutable) {
		setimmutable(x);
	}
	car(x) = a;
	cdr(x) = b;
	return (x);
}

/* ========== oblist implementation  ========== */

static pointer oblist_initial_value(scheme *sc)
{
	return sc->NIL;
}

static pointer oblist_find_by_name(scheme *sc, const char *name)
{
	pointer x;
	char    *s;

	for (x = sc->oblist; is_pair(x); x = cdr(x)) {
		s = symname(car(x));
		if(strcmp(name, s) == 0) {
			return car(x);
		}
	}
	if( x != sc->NIL && (sc == NULL || sc->DEBUG) )
		printk(KERN_ALERT "Schemix:  List ends with non-nil element in oblist_find_by_name\n");
	return sc->NIL;
}

/* returns the new symbol */
static pointer oblist_add_by_name(scheme *sc, const char *name)
{
	pointer x;

	x = immutable_cons(sc, mk_string(sc, name), sc->NIL);
	typeflag(x) = T_SYMBOL;
	setimmutable(car(x));
	sc->oblist = immutable_cons(sc, x, sc->oblist);
	return x;
}

static pointer oblist_all_symbols(scheme *sc)
{
	return sc->oblist;
}

static pointer mk_port(scheme *sc, port *p)
{
	pointer x = get_cell(sc, sc->NIL, sc->NIL);

	typeflag(x) = T_PORT|T_ATOM;
	x->_object._port=p;
	return (x);
}

static pointer mk_character(scheme *sc, int c)
{
	pointer x = get_cell(sc,sc->NIL, sc->NIL);

	typeflag(x) = (T_CHARACTER | T_ATOM);
	long_value_unchecked(x)= c;
	set_integer(x);
	return (x);
}

/* get number atom (integer) */
static pointer mk_integer(scheme *sc, long num)
{
	pointer x = get_cell(sc,sc->NIL, sc->NIL);

	typeflag(x) = (T_NUMBER | T_ATOM);
	long_value_unchecked(x)= num;
	set_integer(x);
	return (x);
}

/* FIXME: this should take type,address as its args */
static pointer mk_memory_reference(scheme *sc, memory_reference *ref)
{
	pointer x;

	for(x=sc->mem_ref_list; is_pair(x); x=cdr(x))
	{
		if(mem_ref(car(x))->ptr == ref->ptr)
		{
			/* It is in the cache, so we can free ref
			   and return the cached value */
			free( sc, ref );
			return( car(x) );
		}
	}
	if( x != sc->NIL && (sc == NULL || sc->DEBUG) )
		printk(KERN_ALERT "Schemix:  List ends with non-nil element in mk_memory_reference\n");
	/* It's not in the cache.  Make a new object. */
	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "Schemix Creating a new memory reference\n");
	x = get_cell(sc, sc->NIL, sc->NIL);
	typeflag(x) = (T_MEMORY_REFERENCE | T_ATOM);
	mem_ref(x) = ref;
	/* Add the new reference to the cache. */
	sc->mem_ref_list = cons(sc, x, sc->mem_ref_list);
	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "Schemix created a new memory reference\n");
	return(x);
}

static pointer mk_number(scheme *sc, num n)
{
	switch( n.num_type )
	{
	case num_long:
		return mk_integer(sc,n.value.long_value);
	default: /* this will never happen */
		return sc->NIL;
	}
}

/* allocate name to string area */
static char *store_string(scheme *sc,
			  int len_str, const char *str, char fill)
{
	char *q;

	q=(char*)malloc( sc, len_str+1 );
	if(q==0) {
		sc->no_memory=1;
		return sc->strbuff;
	}
	if(str!=0) {
		strcpy(q, str);
	} else {
		memset(q, fill, len_str);
		q[len_str]=0;
	}
	return (q);
}

/* get new string */
static pointer mk_string(scheme *sc, const char *str)
{
	return mk_counted_string(sc,str,strlen(str));
}

static pointer mk_counted_string(scheme *sc, const char *str, int len)
{
	pointer x = get_cell(sc, sc->NIL, sc->NIL);

	strvalue(x) = store_string(sc,len,str,0);
	typeflag(x) = (T_STRING | T_ATOM);
	strlength(x) = len;
	return (x);
}

static pointer mk_empty_string(scheme *sc, int len, char fill)
{
	pointer x = get_cell(sc, sc->NIL, sc->NIL);

	strvalue(x) = store_string(sc,len,0,fill);
	typeflag(x) = (T_STRING | T_ATOM);
	strlength(x) = len;
	return (x);
}

static pointer mk_vector(scheme *sc, int len)
{
	pointer x=get_consecutive_cells(sc,len/2+len%2+1);
	typeflag(x) = (T_VECTOR | T_ATOM);
	long_value_unchecked(x)=len;
	set_integer(x);
	fill_vector(x,sc->NIL);
	return x;
}

static void fill_vector(pointer vec, pointer obj)
{
	int i;
	int num=long_value_unchecked(vec)/2+long_value_unchecked(vec)%2;
	for(i=0; i<num; i++) {
		typeflag(vec+1+i) = T_PAIR;
		setimmutable(vec+1+i);
		car(vec+1+i)=obj;
		cdr(vec+1+i)=obj;
	}
}

static pointer vector_elem(pointer vec, int ielem)
{
	int n=ielem/2;
	if(ielem%2==0) {
		return car(vec+1+n);
	} else {
		return cdr(vec+1+n);
	}
}

static pointer set_vector_elem(pointer vec, int ielem, pointer a)
{
	int n=ielem/2;
	if(ielem%2==0) {
		return car(vec+1+n)=a;
	} else {
		return cdr(vec+1+n)=a;
	}
}

/* get new symbol */
static pointer mk_symbol(scheme *sc, const char *name)
{
	pointer x;

	/* first check oblist */
	x = oblist_find_by_name(sc, name);
	if (x != sc->NIL) {
		return (x);
	} else {
		x = oblist_add_by_name(sc, name);
		return (x);
	}
}

static pointer gensym(scheme *sc)
{
	pointer x;
	char name[40];

	for(; sc->gensym_cnt<LONG_MAX; sc->gensym_cnt++) {
		sprintf(name,"gensym-%ld",sc->gensym_cnt);

		/* first check oblist */
		x = oblist_find_by_name(sc, name);

		if (x != sc->NIL) {
			continue;
		} else {
			x = oblist_add_by_name(sc, name);
			return (x);
		}
	}

	return sc->NIL;
}

/* make symbol or number atom from string */
static pointer mk_atom(scheme *sc, char *q)
{
	char c, *p;
	long integer_value=0;
#if SCHEMIX_USE_KALLSYMS
	char *module, *symbol;
	if((p=strstr(q,"::"))!=0) {
		*p=0;
		module = q;
		symbol = p+2;
		return mk_memory_reference(sc,
					   kernel_lookup(sc, module, symbol));
	}
#endif

	q = strlwr(q);

	/* FIXME: This is a mess! */
	p = q;
	c = *p++;
	if ((c == '+') || (c == '-')) {
		c = *p++;
		if (!isdigit(c)) {
			return (mk_symbol(sc, q));
		}
	} else if (!isdigit(c)) {
		return (mk_symbol(sc, q));
	}

	for ( ; (c = *p) != 0; ++p) {
		if (!isdigit(c)) {
			return (mk_symbol(sc, q));
		}
	}
	sscanf( q, "%ld", &integer_value );
	return mk_integer(sc, integer_value);
}

/* make constant */
static pointer mk_sharp_const(scheme *sc, char *name)
{
	long    x;
	char    tmp[256];

	if (!strcmp(name, "t"))
		return (sc->T);
	else if (!strcmp(name, "f"))
		return (sc->F);
	else if (*name == 'o') {/* #o (octal) */
		sprintf(tmp, "0%s", name+1);
		sscanf(tmp, "%lo", &x);
		return (mk_integer(sc, x));
	} else if (*name == 'd') {    /* #d (decimal) */
		sscanf(name+1, "%ld", &x);
		return (mk_integer(sc, x));
	} else if (*name == 'x') {    /* #x (hex) */
		sprintf(tmp, "0x%s", name+1);
		sscanf(tmp, "%lx", &x);
		return (mk_integer(sc, x));
	} else if (*name == 'b') {    /* #b (binary) */
		x = binary_decode(name+1);
		return (mk_integer(sc, x));
	} else if (*name == '\\') { /* #\w (character) */
		int c=0;
		if(strcmp(name+1,"space")==0) {
			c=' ';
		} else if(strcmp(name+1,"newline")==0) {
			c='\n';
		} else if(strcmp(name+1,"return")==0) {
			c='\r';
		} else if(strcmp(name+1,"tab")==0) {
			c='\t';
		} else if(name[1]=='x' && name[2]!=0) {
			int c1=0;
			if(sscanf(name+2,"%x",&c1)==1 && c1<256) {
				c=c1;
			} else {
				return sc->NIL;
			}
		} else if(name[2]==0) {
			c=name[1];
		} else {
			return sc->NIL;
		}
		return mk_character(sc,c);
	} else
		return (sc->NIL);
}

/* ========== garbage collector ========== */

/*--
 *  We use algorithm E (Knuth, The Art of Computer Programming Vol.1,
 *  sec. 2.3.5), the Schorr-Deutsch-Waite link-inversion algorithm,
 *  for marking.
 */
static void mark(pointer a)
{
	pointer t, q, p;

	t = (pointer) 0;
	p = a;
 E2:  setmark(p);
	if(is_vector(p)) {
		int i;
		int num=long_value_unchecked(p)/2+long_value_unchecked(p)%2;
		for(i=0; i<num; i++) {
			/* Vector cells will be treated like ordinary cells */
			mark(p+1+i);
		}
	}
	if (is_atom(p))
		goto E6;
	/* E4: down car */
	q = car(p);
	if (q && !is_mark(q)) {
		setatom(p);  /* a note that we have moved car */
		car(p) = t;
		t = p;
		p = q;
		goto E2;
	}
 E5:  q = cdr(p); /* down cdr */
	if (q && !is_mark(q)) {
		cdr(p) = t;
		t = p;
		p = q;
		goto E2;
	}
 E6:   /* up.  Undo the link switching from steps E4 and E5. */
	if (!t)
		return;
	q = t;
	if (is_atom(q)) {
		clratom(q);
		t = car(q);
		car(q) = p;
		p = q;
		goto E5;
	} else {
		t = cdr(q);
		cdr(q) = p;
		p = q;
		goto E6;
	}
}

/* garbage collection. parameter a, b is marked. */
static void gc(scheme *sc, pointer a, pointer b)
{
	pointer p;
	int i;

	if(sc->gc_verbose) {
		putstr(sc, "gc...");
	}

	/* mark system globals */
	mark(sc->oblist);
	mark(sc->global_env);
	mark(sc->mem_ref_list);

	/* mark current registers */
	mark(sc->args);
	mark(sc->envir);
	mark(sc->code);
	dump_stack_mark(sc);
	mark(sc->value);
	mark(sc->inport);
	mark(sc->outport);
	mark(sc->loadport);

	/* mark variables a, b */
	mark(a);
	mark(b);

	/* garbage collect */
	clrmark(sc->NIL);
	sc->fcells = 0;
	sc->free_cell = sc->NIL;
	/* free-list is kept sorted by address so as to maintain consecutive
	   ranges, if possible, for use with vectors. Here we scan the cells
	   (which are also kept sorted by address) downwards to build the
	   free-list in sorted order.
	*/
	for (i = sc->last_cell_seg; i >= 0; i--) {
		p = sc->cell_seg[i] + CELL_SEGSIZE;
		while (--p >= sc->cell_seg[i]) {
			if (is_mark(p)) {
				clrmark(p);
			} else {
				/* reclaim cell */
				if (typeflag(p) != 0) {
					finalize_cell(sc, p);
					typeflag(p) = 0;
					++sc->fcells;
					car(p) = sc->NIL;
				}
				cdr(p) = sc->free_cell;
				sc->free_cell = p;
			}
		}
	}

	if (sc->gc_verbose) {
		char msg[80];
		sprintf(msg,"done: %ld cells were recovered.\n", sc->fcells);
		putstr(sc,msg);
	}
}

static void finalize_cell(scheme *sc, pointer a)
{
	if(is_string(a)) {
		free( sc, strvalue(a) );
	} else if(is_port(a)) {
		free( sc, a->_object._port );
	} else if(is_memory_reference(a)) {
		free( sc, mem_ref(a) );
	}
}
/* ========== Routines for Reading ========== */

static void port_close(scheme *sc, pointer p, int flag)
{
	port *pt=p->_object._port;
	pt->kind&=~flag;
	if((pt->kind & (port_input|port_output))==0) {
		pt->kind=port_free;
	}
}

static int inchar(scheme *sc)
{
	port *pt = sc->inport->_object._port;
   if( *pt->rep.string.curr == 0
       || pt->rep.string.curr == pt->rep.string.past_the_end )
		return EOF;
	else
		return *pt->rep.string.curr++;
}

/* back character to input buffer */
static void backchar(scheme *sc, int c)
{
	port *pt;
	if(c==EOF) return;
	pt=sc->inport->_object._port;
	if(pt->rep.string.curr!=pt->rep.string.start) {
		--pt->rep.string.curr;
	}
}

static void putstr(scheme *sc, const char *s)
{
	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "%s", s);
	sc->output_buffer_end += snprintf( sc->output_buffer_end,
					   ((sc->output_buffer_start)
					    + OUTPUT_BUFFER_LENGTH
					    - (sc->output_buffer_end)),
					   "%s", s );
}

static void putchars(scheme *sc, const char *s, int len)
{
	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "%.*s", len, s);
	sc->output_buffer_end += snprintf( sc->output_buffer_end,
					   ((sc->output_buffer_start)
					    + OUTPUT_BUFFER_LENGTH
					    - (sc->output_buffer_end)),
					   "%.*s", len, s );
}

static void putcharacter(scheme *sc, int c)
{
	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "%c",c);
	sc->output_buffer_end += snprintf( sc->output_buffer_end,
					   ((sc->output_buffer_start)
					    + OUTPUT_BUFFER_LENGTH
					    - (sc->output_buffer_end)),
					   "%c", c );
}

/* read characters up to delimiter, but cater to character constants */
static char   *readstr_upto(scheme *sc, char *delim)
{
	char   *p = sc->strbuff;

	while (!is_one_of(delim, (*p++ = inchar(sc))));
	if(p==sc->strbuff+2 && p[-2]=='\\') {
		*p=0;
	} else {
		backchar(sc,p[-1]);
		*--p = '\0';
	}
	return sc->strbuff;
}

/* read string expression "xxx...xxx" */
static pointer readstrexp(scheme *sc)
{
	char *p = sc->strbuff;
	int c;
	int c1=0;
	enum { st_ok, st_bsl, st_x1, st_x2} state=st_ok;

	for (;;) {
		c=inchar(sc);
		if(p-sc->strbuff>sizeof(sc->strbuff)-1) {
			return sc->F;
		}
		if(c==EOF) {
			return sc->F;
		}
		switch(state) {
		case st_ok:
			switch(c) {
			case '\\':
				state=st_bsl;
				break;
			case '"':
				*p=0;
				return mk_counted_string(sc,
							 sc->strbuff,
							 p - sc->strbuff);
			default:
				*p++=c;
				break;
			}
			break;
		case st_bsl:
			switch(c) {
			case 'x':
			case 'X':
				state=st_x1;
				c1=0;
				break;
			case 'n':
				*p++='\n';
				state=st_ok;
				break;
			case 't':
				*p++='\t';
				state=st_ok;
				break;
			case 'r':
				*p++='\r';
				state=st_ok;
				break;
			case '"':
				*p++='"';
				state=st_ok;
				break;
			default:
				*p++=c;
				state=st_ok;
				break;
			}
			break;
		case st_x1:
		case st_x2:
			c = toupper(c);
			if(c>='0' && c<='F') {
				if(c<='9') {
					c1=(c1<<4)+c-'0';
				} else {
					c1=(c1<<4)+c-'A'+10;
				}
				if(state==st_x1) {
					state=st_x2;
				} else {
					*p++=c1;
					state=st_ok;
				}
			} else {
				return sc->F;
			}
			break;
		}
	}
}

/* check c is in chars */
static int is_one_of(char *s, int c)
{
	if(c==EOF) return 1;
	while (*s)
		if (*s++ == c)
			return (1);
	return (0);
}

static char *strlwr(char *s)
{
	char *p=s;
	while(*s) {
		*s=tolower(*s);
		s++;
	}
	return p;
}

/* skip white characters */
static void skipspace(scheme *sc)
{
	int c=0;
	while ((c=inchar(sc)) && isspace(c))
		;
	if(c!=EOF) {
		backchar(sc,c);
	}
}

/* get token */
static int token(scheme *sc)
{
	int c;
	skipspace(sc);
   c = inchar(sc);
   /*printk(KERN_ALERT "Schemix: tokenizing on character %d\n", c);*/
	switch (c) {
	case EOF:
		return (TOK_EOF);
	case '(':
		return (TOK_LPAREN);
	case ')':
		return (TOK_RPAREN);
	case '.':
		c=inchar(sc);
		if(isspace(c)) {
			return (TOK_DOT);
		} else {
			backchar(sc,c);
			backchar(sc,'.');
			return TOK_ATOM;
		}
	case '\'':
		return (TOK_QUOTE);
	case ';':
		return (TOK_COMMENT);
	case '"':
		return (TOK_DQUOTE);
	case BACKQUOTE:
		return (TOK_BQUOTE);
	case ',':
		if ((c=inchar(sc)) == '@')
			return (TOK_ATMARK);
		else {
			backchar(sc,c);
			return (TOK_COMMA);
		}
	case '#':
		c=inchar(sc);
		if (c == '(') {
			return (TOK_VEC);
		} else if(c == '!') {
			return TOK_COMMENT;
		} else {
			backchar(sc,c);
			if(is_one_of(" tfodxb\\",c)) {
				return TOK_SHARP_CONST;
			} else {
				return (TOK_SHARP);
			}
		}
	default:
		backchar(sc,c);
		return (TOK_ATOM);
	}
}

/* ========== Routines for Printing ========== */
#define   ok_abbrev(x)   (is_pair(x) && cdr(x) == sc->NIL)

static void printslashstring(scheme *sc, char *p, int len)
{
	int i;
	unsigned char *s=(unsigned char*)p;
	putcharacter(sc,'"');
	for ( i=0; i<len; i++) {
		if(*s==0xff || *s=='"' || *s<' ' || *s=='\\') {
			putcharacter(sc,'\\');
			switch(*s) {
			case '"':
				putcharacter(sc,'"');
				break;
			case '\n':
				putcharacter(sc,'n');
				break;
			case '\t':
				putcharacter(sc,'t');
				break;
			case '\r':
				putcharacter(sc,'r');
				break;
			case '\\':
				putcharacter(sc,'\\');
				break;
			default: {
				int d=*s/16;
				putcharacter(sc,'x');
				if(d<10) {
					putcharacter(sc,d+'0');
				} else {
					putcharacter(sc,d-10+'A');
				}
				d=*s%16;
				if(d<10) {
					putcharacter(sc,d+'0');
				} else {
					putcharacter(sc,d-10+'A');
				}
			}
			}
		} else {
			putcharacter(sc,*s);
		}
		s++;
	}
	putcharacter(sc,'"');
}


/* print atoms */
static void printatom(scheme *sc, pointer l, int f)
{
	char *p;
	int len;
	atom2str(sc,l,f,&p,&len);
	putchars(sc,p,len);
}


/* Uses internal buffer unless string pointer is already available */
static void atom2str(scheme *sc, pointer l, int f, char **pp, int *plen)
{
	char *p;

	if (l == sc->NIL) {
		p = "()";
	} else if (l == sc->T) {
		p = "#t";
	} else if (l == sc->F) {
		p = "#f";
	} else if (l == sc->EOF_OBJ) {
		p = "#<EOF>";
	} else if (is_port(l)) {
		p = sc->strbuff;
		strcpy(p, "#<PORT>");
	} else if (is_number(l)) {
		p = sc->strbuff;
		if(is_integer(l)) {
			sprintf(p, "%ld", long_value_unchecked(l));
		}
	} else if (is_string(l)) {
		if (!f) {
			p = strvalue(l);
		} else { /* Hack, uses the fact that printing is needed */
			*pp=sc->strbuff;
			*plen=0;
			printslashstring(sc, strvalue(l), strlength(l));
			return;
		}
	} else if (is_character(l)) {
		int c=charvalue(l);
		p = sc->strbuff;
		if (!f) {
			p[0]=c;
			p[1]=0;
		} else {
			switch(c) {
			case ' ':
				sprintf(p,"#\\space"); break;
			case '\n':
				sprintf(p,"#\\newline"); break;
			case '\r':
				sprintf(p,"#\\return"); break;
			case '\t':
				sprintf(p,"#\\tab"); break;
			default:
				if(c<32) {
					sprintf(p,"#\\x%x",c); break;
				}
				sprintf(p,"#\\%c",c); break;
			}
		}
	} else if (is_symbol(l)) {
		p = symname(l);
	} else if (is_proc(l)) {
		p = sc->strbuff;
		sprintf(p, "#<%s PROCEDURE %ld>", procname(l), procnum(l));
#if SCHEMIX_USE_KALLSYMS
	} else if (is_kernel_proc(l)) {
		p = sc->strbuff;
		sprintf(p, "#<KERNEL-PROCEDURE #x%p>", mem_ref(cdr(l))->ptr);
	} else if (is_memory_reference(l)) {
		p = sc->strbuff;
		sprintf(p, "#<MEMORY-REFERENCE #x%p (%ld bytes, %s%s)>",
			mem_ref(l)->ptr,
			mem_ref(l)->length,
			structures_table[mem_ref(l)->type].name,
			(mem_ref(l)->dim == NULL
			 || mem_ref(l)->dim == sc->NIL) ? "" : " array" );
#endif
	} else if (is_device(l)) {
		p = "#<DEVICE>";
	} else if (is_macro(l)) {
		p = "#<MACRO>";
	} else if (is_closure(l)) {
		p = "#<CLOSURE>";
	} else if (is_promise(l)) {
		p = "#<PROMISE>";
	} else if (is_continuation(l)) {
		p = "#<CONTINUATION>";
	} else {
		p = "#<ERROR>";
	}
	*pp=p;
	*plen=strlen(p);
}
/* ========== Routines for Evaluation Cycle ========== */

/* make closure. c is code. e is environment */
static pointer mk_closure(scheme *sc, pointer c, pointer e)
{
	pointer x = get_cell(sc, c, e);

	typeflag(x) = T_CLOSURE;
	car(x) = c;
	cdr(x) = e;
	return (x);
}

/* The next few functions were taken from modutils/examples. */

#if SCHEMIX_USE_KALLSYMS
extern const char __start___kallsyms[];
extern const char __stop___kallsyms[];

static struct module *local_module_list;

static void get_module_list(void)
{
	const struct kallsyms_header	*ka_hdr;
	const struct kallsyms_section	*ka_sec;
	const struct kallsyms_symbol	*ka_sym;
	const char			*ka_str;
	int i;
	const char *p;

	if (__start___kallsyms >= __stop___kallsyms)
		return;
	ka_hdr = (struct kallsyms_header *)__start___kallsyms;
	ka_sec = (struct kallsyms_section *)
		((char *)(ka_hdr) + ka_hdr->section_off);
	ka_sym = (struct kallsyms_symbol *)
		((char *)(ka_hdr) + ka_hdr->symbol_off);
	ka_str =
		((char *)(ka_hdr) + ka_hdr->string_off);

	for (i = 0;
	     i < ka_hdr->symbols;
	     kallsyms_next_sym(ka_hdr, ka_sym), ++i) {
		p = ka_str + ka_sym->name_off;
		if (strcmp(p, "module_list") == 0) {
			if (ka_sym->symbol_addr)
				local_module_list = *((struct module **)(ka_sym->symbol_addr));
			break;
		}
	}
}

/* Find a module by its name */
static struct module *kernel_module( char *name )
{
	struct module *m;
	get_module_list();

	if( strcmp( name, "kernel" ) == 0 )
		for( m = local_module_list; m; m = m->next )
			if( *(m->name) == 0 )
				return m;

	for( m = local_module_list; m; m = m->next )
		if( strcmp( name, m->name ) == 0 )
			return m;

	return NULL;
}

/* Find a symbol in a module */
static memory_reference *kernel_lookup( scheme *sc, char *module, char *name )
{
	const struct kallsyms_header	*ka_hdr = NULL;
	const struct kallsyms_section	*ka_sec;
	const struct kallsyms_symbol	*ka_sym = NULL;
	const char *ka_str = NULL;
	const struct module *m;
	int i = 0, l;
	const char *p, *pt_R;
	char *p2;
	unsigned long sym_start, sym_end;
	memory_reference *ref;

	m = kernel_module( module );
	if( !m )
		return NULL;

	if (!mod_member_present(m, kallsyms_start) ||
	    !mod_member_present(m, kallsyms_end) ||
	    m->kallsyms_start >= m->kallsyms_end)
		return NULL;

	ka_hdr = (struct kallsyms_header *)m->kallsyms_start;
	ka_sym = (struct kallsyms_symbol *)((char *)(ka_hdr) + ka_hdr->symbol_off);
	ka_str = ((char *)(ka_hdr) + ka_hdr->string_off);
	for (i = 0; i < ka_hdr->symbols; ++i, kallsyms_next_sym(ka_hdr, ka_sym))
	{
		p = ka_str + ka_sym->name_off;
		if (strcmp(p, name) == 0)
			break;
		/* Unversioned requests match versioned names */
		if (!(pt_R = strstr(p, "_R")))
			continue;
		l = strlen(pt_R);
		if (l < 10)
			continue;	/* Not _R.*xxxxxxxx */
		(void)simple_strtoul(pt_R+l-8, &p2, 16);
		if (*p2)
			continue;	/* Not _R.*xxxxxxxx */
		if (strncmp(p, name, pt_R-p) == 0)
			break;	/* Match with version */
	}
	if (i == ka_hdr->symbols)
		return NULL;

	ka_sec = (const struct kallsyms_section *)((char *)ka_hdr
						   + ka_hdr->section_off
						   + ka_sym->section_off);
	sym_start = ka_sym->symbol_addr;
	if (i < ka_hdr->symbols-1) {
		const struct kallsyms_symbol *ka_symn = ka_sym;
		kallsyms_next_sym(ka_hdr, ka_symn);
		sym_end = ka_symn->symbol_addr;
	}
	else
		sym_end = ka_sec->start + ka_sec->size;

	ref = malloc( sc, sizeof(memory_reference) );
	ref->type = 0; /* type is unknown */
	ref->ptr = (void*)sym_start;
	ref->length = sym_end - sym_start;
	ref->dim = NULL; /* Unknown dimensions */

	return ref;
}

#define ARGS_NONE     0x0
#define ARGS_INT      0x1
#define ARGS_STR      0x2
#define ARGS_INT_INT  0x11
#define ARGS_INT_STR  0x12
#define ARGS_STR_INT  0x21
#define ARGS_STR_STR  0x22

static long arg_hash(scheme *sc, pointer args)
{
	long type = ARGS_NONE;
	while(is_pair(args)) {
		type = type << 8;
		if( (is_symbol(car(args))
		     && strcmp(symname(car(args)), "int") == 0)
		    || is_integer(car(args)))
			type += ARGS_INT;
		else if( (is_symbol(car(args))
			  && strcmp(symname(car(args)), "char*") == 0)
			 || is_string(car(args)))
			type += ARGS_STR;
		args = cdr(args);
	}
	return type;
}

/* make a kernel procedure.                                */
/* these are created by expressions of the form            */
/* (kernel-lambda (arg1 ... argn) module::name)            */
static pointer mk_kernel_procedure(scheme *sc, pointer args, pointer proc)
{
	pointer x = get_cell(sc, args, proc);
	typeflag(x) = T_KERNEL_PROCEDURE;
	car(x) = mk_integer( sc, arg_hash( sc, args ) );
	cdr(x) = proc;
	return (x);
}

/* call a kernel procedure */
static void apply_kernel_procedure(scheme *sc, pointer proc, pointer args)
{
	void* f = mem_ref(cdr(proc))->ptr;
	int arg_types = arg_hash( sc, args );
	pointer arg1=car(args), arg2=cadr(args);

	/* validate that given args follow the required type signature */
	if( arg_types != long_value_unchecked(car(proc)) )
	{
		printk( KERN_ALERT "Schemix: args don't match type signature\n" );
		return;
	}

	/* FIXME: autogenerate this dispatch table at compile time */

	switch( arg_types ) {
	case ARGS_NONE:
		((void(*)(void))f)();
		break;
	case ARGS_INT:
		((void(*)(int))f)(long_value_unchecked(arg1));
		break;
	case ARGS_STR:
		((void(*)(char*))f)(strvalue(arg1));
		break;
	case ARGS_INT_INT:
		((void(*)(int,int))f)(long_value_unchecked(arg1),
				      long_value_unchecked(arg2));
		break;
	case ARGS_INT_STR:
		((void(*)(int,char*))f)(long_value_unchecked(arg1),
					strvalue(arg2));
		break;
	case ARGS_STR_INT:
		((void(*)(char*,int))f)(strvalue(arg1),
					long_value_unchecked(arg2));
		break;
	case ARGS_STR_STR:
		((void(*)(char*,char*))f)(strvalue(arg1), strvalue(arg2));
		break;
	default:
		break;
	}
}

static int flatten_array_ref(scheme *sc, pointer x, pointer dim)
{
	int product = 1;
	int sum = 0;
	while( is_pair(x) && dim != sc->NIL )
	{
		sum += long_value_unchecked(car(x)) * product;
		product *= long_value_unchecked(car(dim));
		dim = cdr(dim);
		x = cdr(x);
	}
	if( x != sc->NIL && (sc == NULL || sc->DEBUG) )
		printk(KERN_ALERT "Schemix:  List ends with non-nil element in flatten_array_ref\n");
	if( x != dim )
		return -1; /* too many/few coordinates */
	return sum;
}

static pointer mk_structure(scheme *sc, pointer name)
{
	int i;
	pointer x;
	memory_reference *ref;
	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "Schemix making a structure of type %s\n",
		       symname(name));
	/* Lookup name in structures_table */
	for( i = 0;
	     i < structures_table_length
		     && strcmp(structures_table[i].name, symname(name)) != 0;
	     i++ )
	{
		if( sc == NULL || sc->DEBUG )
			printk(KERN_ALERT "Schemix found structure type %s\n",
			       structures_table[i].name);
	}
	if( i == structures_table_length ) /* Can't find structure name */
		return sc->F;
	/* Allocate cell and enough memory */
	ref = malloc( sc, sizeof(memory_reference) );
	ref->type = i;
	ref->ptr = malloc( sc, structures_table[i].size );
	ref->length = structures_table[i].size;
	ref->dim = sc->NIL; /* singleton - not array */
	x = mk_memory_reference( sc, ref );
	/* Zero the allocated memory */
	memset( mem_ref(x)->ptr, 0, mem_ref(x)->length );
	/* Return the handle */
	return x;
}
#endif  /* #if SCHEMIX_USE_KALLSYMS */

#if SCHEMIX_USE_DEVFS
static pointer mk_device(scheme *sc, pointer name, pointer bindings)
{
	pointer x = get_cell(sc, name, bindings);

	typeflag(x) = T_DEVICE;
	x->_object._scheme = make_device(sc, symname(name), bindings);
	return (x);
}
#endif

/* make continuation. */
static pointer mk_continuation(scheme *sc, pointer d)
{
	pointer x = get_cell(sc, sc->NIL, d);

	typeflag(x) = T_CONTINUATION;
	cont_dump(x) = d;
	return (x);
}

static pointer list_star(scheme *sc, pointer d)
{
	pointer p, q;
	if(cdr(d)==sc->NIL) {
		return car(d);
	}
	p=cons(sc,car(d),cdr(d));
	q=p;
	while(cdr(cdr(p))!=sc->NIL) {
		d=cons(sc,car(p),cdr(p));
		if(cdr(cdr(p))!=sc->NIL) {
			p=cdr(d);
		}
	}
	cdr(p)=car(cdr(p));
	return q;
}

/* reverse list -- produce new list */
static pointer reverse(scheme *sc, pointer a)
{
	/* a must be checked by gc */
	pointer p = sc->NIL;

	for ( ; is_pair(a); a = cdr(a)) {
		p = cons(sc, car(a), p);
	}
	return (p);
}

/* reverse list --- in-place */
static pointer reverse_in_place(scheme *sc, pointer term, pointer list)
{
	pointer p = list, result = term, q;

	while (is_pair(p)) {
		q = cdr(p);
		cdr(p) = result;
		result = p;
		p = q;
	}
	return (result);
}

/* append list -- produce new list */
static pointer append(scheme *sc, pointer a, pointer b)
{
	pointer p = b, q;

	if (a != sc->NIL) {
		a = reverse(sc, a);
		while (is_pair(a)) {
			q = cdr(a);
			cdr(a) = p;
			p = a;
			a = q;
		}
	}
	return (p);
}

/* equivalence of atoms */
static int eqv(pointer a, pointer b)
{
	if (is_string(a)) {
		if (is_string(b))
			return (strvalue(a) == strvalue(b));
		else
			return (0);
	} else if (is_number(a)) {
		if (is_number(b))
			return num_eq(nvalue(a),nvalue(b));
		else
			return (0);
	} else if (is_character(a)) {
		if (is_character(b))
			return charvalue(a)==charvalue(b);
		else
			return (0);
	} else if (is_port(a)) {
		if (is_port(b))
			return a==b;
		else
			return (0);
	} else if (is_proc(a)) {
		if (is_proc(b))
			return procnum(a)==procnum(b);
		else
			return (0);
	} else {
		return (a == b);
	}
}

/* ========== Environment implementation  ========== */

static int hash_fn(const char *key, int table_size)
{
	unsigned int hashed = 0;
	const char *c;
	int bits_per_int = sizeof(unsigned int)*8;

	for (c = key; *c; c++) {
		/* letters have about 5 bits in them */
		hashed = (hashed<<5) | (hashed>>(bits_per_int-5));
		hashed ^= *c;
	}
	return hashed % table_size;
}

/*
 * In this implementation, each frame of the environment may be
 * a hash table: a vector of alists hashed by variable name.
 * In practice, we use a vector only for the initial frame;
 * subsequent frames are too small and transient for the lookup
 * speed to out-weigh the cost of making a new vector.
 */

static pointer mk_environment(scheme *sc, pointer x, pointer y)
{
	pointer z = immutable_cons(sc, x, y);
	setenvironment(z);
	return z;
}

static void new_frame_in_env(scheme *sc, pointer old_env)
{
	pointer new_frame;

	/* The interaction-environment has about 300 variables in it. */
	if (old_env == sc->NIL) {
		new_frame = mk_vector(sc, 461);
	} else {
		new_frame = sc->NIL;
	}

	sc->envir = mk_environment(sc, new_frame, old_env);
}

static void new_slot_spec_in_env(scheme *sc, pointer env,
                                 pointer variable, pointer value)
{
	pointer slot = immutable_cons(sc, variable, value);

	if (is_vector(car(env))) {
		int location = hash_fn(symname(variable),
				       long_value_unchecked(car(env)));
		set_vector_elem(car(env),
				location,
				immutable_cons(sc,
					       slot,
					       vector_elem(car(env),
							   location)));
	} else {
		car(env) = immutable_cons(sc, slot, car(env));
	}
}

static pointer find_slot_in_env(scheme *sc, pointer env, pointer hdl, int all)
{
	pointer x,y=sc->NIL;
	int location;

	if( is_memory_reference(hdl) )
		return hdl;

	for (x = env; is_environment(x); x = cdr(x)) {
		if (is_vector(car(x))) {
			location = hash_fn(symname(hdl),
					   long_value_unchecked(car(x)));
			y = vector_elem(car(x), location);
		} else {
			y = car(x);
		}
		for ( ; is_pair(y); y = cdr(y)) {
			if (caar(y) == hdl) {
				break;
			}
		}
		if (is_pair(y)) {
			break;
		}
		else if( y != sc->NIL && (sc == NULL || sc->DEBUG) )
			printk(KERN_ALERT "Schemix:  List ends with non-nil element in find_slot_in_env\n");
		if(!all) {
			return sc->NIL;
		}
	}
	if (is_environment(x)) {
		return car(y);
	}
	else if( x != sc->NIL && (sc == NULL || sc->DEBUG) )
		printk(KERN_ALERT "Schemix:  Environment ends with non-nil element in find_slot_in_env\n");
	return sc->NIL;
}

static void new_slot_in_env(scheme *sc, pointer variable, pointer value)
{
	new_slot_spec_in_env(sc, sc->envir, variable, value);
}

#if SCHEMIX_USE_KALLSYMS
static int set_memory(scheme *sc, pointer slot, pointer value,
		      int offset, char is_array_ref)
{
	if( mem_ref(slot)->dim == sc->NIL || is_array_ref )
	{
		/* FIXME: This should flag an error on incompatable types */
		switch( mem_ref(slot)->type )
		{
		case STRUCT_TYPE_CHAR:
			if( is_character(value) )
				__put_user( (char)charvalue(value),
					    ((char*)(mem_ref(slot)->ptr)
					     + offset) );
			return 1;
		case STRUCT_TYPE_SHORT:
			if( is_integer(value) )
				*((short*)(mem_ref(slot)->ptr) + offset)
					= (short)long_value_unchecked(value);
			return 1;
		case STRUCT_TYPE_INT:
			if( is_integer(value) )
				*((int*)(mem_ref(slot)->ptr) + offset)
					= (int)long_value_unchecked(value);
			return 1;
		case STRUCT_TYPE_LONG:
			if( is_integer(value) )
				*((long*)(mem_ref(slot)->ptr) + offset)
					= (long)long_value_unchecked(value);
			return 1;
		case STRUCT_TYPE_FLOAT:
			if( is_integer(value) )
				*((float*)(mem_ref(slot)->ptr) + offset)
					= (float)long_value_unchecked(value);
			return 1;
		case STRUCT_TYPE_DOUBLE:
			if( is_integer(value) )
				*((double*)(mem_ref(slot)->ptr) + offset)
					= (double)long_value_unchecked(value);
			return 1;
		case STRUCT_TYPE_UNSIGNED_CHAR:
			if( is_character(value) )
				*((unsigned char*)(mem_ref(slot)->ptr)+offset)
					= (unsigned char)charvalue(value);
			return 1;
		case STRUCT_TYPE_UNSIGNED_SHORT:
			if( is_integer(value) )
				*((unsigned short*)(mem_ref(slot)->ptr)+offset)
					= (unsigned short)long_value_unchecked(value);
			return 1;
		case STRUCT_TYPE_UNSIGNED_INT:
			if( is_integer(value) )
				*((unsigned int*)(mem_ref(slot)->ptr)+offset)
					= (unsigned int)long_value_unchecked(value);
			return 1;
		case STRUCT_TYPE_UNSIGNED_LONG:
			if( is_integer(value) )
				*((unsigned long*)(mem_ref(slot)->ptr)+offset)
					= (unsigned long)long_value_unchecked(value);
			return 1;
		}
	}
	return 0; /* Not successful */
}
#endif

static int set_slot_in_env(scheme *sc, pointer slot, pointer value)
{
#if SCHEMIX_USE_KALLSYMS
	if( is_memory_reference(slot) )
		return set_memory(sc, slot, value, 0, 0);
	else /* Ordinary Scheme slot in environment */
#endif
		cdr(slot) = value;
	return 1;
}

#if SCHEMIX_USE_KALLSYMS
static pointer box_char(scheme *sc, void* ptr, int offset)
{
	return mk_character(sc, (int)(*((char*)ptr + offset)));
}

static pointer box_short(scheme *sc, void* ptr, int offset)
{
	return mk_integer(sc, (long)(*((short*)ptr + offset)));
}

static pointer box_int(scheme *sc, void* ptr, int offset)
{
	return mk_integer(sc, (long)(*((int*)ptr + offset)));
}

static pointer box_long(scheme *sc, void* ptr, int offset)
{
	return mk_integer(sc, (long)(*((long*)ptr + offset)));
}

static pointer box_float(scheme *sc, void* ptr, int offset)
{
	return mk_integer(sc, (long)(*((float*)ptr + offset)));
}

static pointer box_double(scheme *sc, void* ptr, int offset)
{
	return mk_integer(sc, (long)(*((double*)ptr + offset)));
}

static pointer box_unsigned_char(scheme *sc, void* ptr, int offset)
{
	return mk_character(sc, (int)(*((unsigned char*)ptr + offset)));
}

static pointer box_unsigned_short(scheme *sc, void* ptr, int offset)
{
	return mk_integer(sc, (long)(*((unsigned short*)ptr + offset)));
}

static pointer box_unsigned_int(scheme *sc, void* ptr, int offset)
{
	return mk_integer(sc, (long)(*((unsigned int*)ptr + offset)));
}

static pointer box_unsigned_long(scheme *sc, void* ptr, int offset)
{
	return mk_integer(sc, (long)(*((unsigned long*)ptr + offset)));
}

static pointer box_value(scheme *sc, pointer slot, int offset,
			 char is_array_ref)
{
	if( mem_ref(slot)->dim == NULL ) /* unknown dimensions */
		return slot;
	else if( mem_ref(slot)->dim == sc->NIL || is_array_ref )
		switch( mem_ref(slot)->type )
		{
			/* Box C types into Scheme objects */
		case STRUCT_TYPE_CHAR:
			return box_char(sc,mem_ref(slot)->ptr,offset);
		case STRUCT_TYPE_SHORT:
			return box_short(sc,mem_ref(slot)->ptr,offset);
		case STRUCT_TYPE_INT:
			return box_int(sc,mem_ref(slot)->ptr,offset);
		case STRUCT_TYPE_LONG:
			return box_long(sc,mem_ref(slot)->ptr,offset);
		case STRUCT_TYPE_FLOAT:
			return box_float(sc,mem_ref(slot)->ptr,offset);
		case STRUCT_TYPE_DOUBLE:
			return box_double(sc,mem_ref(slot)->ptr,offset);
		case STRUCT_TYPE_UNSIGNED_CHAR:
			return box_unsigned_char(sc,mem_ref(slot)->ptr,offset);
		case STRUCT_TYPE_UNSIGNED_SHORT:
			return box_unsigned_short(sc,
						  mem_ref(slot)->ptr,
						  offset);
		case STRUCT_TYPE_UNSIGNED_INT:
			return box_unsigned_int(sc,mem_ref(slot)->ptr,offset);
		case STRUCT_TYPE_UNSIGNED_LONG:
			return box_unsigned_long(sc,mem_ref(slot)->ptr,offset);
		default: return slot; /* unknown types are self-evaluating */
		}
	else if( cdr(mem_ref(slot)->dim) == sc->NIL /* (char n) -> string */
		 && mem_ref(slot)->type == STRUCT_TYPE_CHAR )
		return mk_string(sc, (const char*)mem_ref(slot)->ptr);
	else /* we don't box arrays - use array-ref and array-set! */
		return slot;
}
#endif /* SCHEMIX_USE_KALLSYMS */

static pointer slot_value_in_env(scheme *sc, pointer slot)
{
#if SCHEMIX_USE_KALLSYMS
	if( is_memory_reference(slot) ) /* Memory reference (C-style) */
		return box_value(sc, slot, 0, 0);
	else /* Ordinary Scheme slot in environment */
#endif
		return cdr(slot);
}

/* ========== Evaluation Cycle ========== */


static pointer _Error_1(scheme *sc, const char *s, pointer a)
{
	if(a!=0) {
		sc->args = cons(sc, (a), sc->NIL);
	} else {
		sc->args = sc->NIL;
	}
	sc->args = cons(sc, mk_string(sc, (s)), sc->args);
	setimmutable(car(sc->args));
	sc->op = (int)OP_ERR0;
	return sc->T;
}
#define Error_1(sc,s, a) return _Error_1(sc,s,a)
#define Error_0(sc,s)    return _Error_1(sc,s,0)

/* Too small to turn into function */
# define  BEGIN     do {
# define  END  } while (0)
#define s_goto(sc,a) BEGIN                                  \
    sc->op = (int)(a);                                      \
    return sc->T; END

#define s_return(sc,a) return _s_return(sc,a)

#ifndef USE_SCHEME_STACK

/* this structure holds all the interpreter's registers */
struct dump_stack_frame {
	enum scheme_opcodes op;
	pointer args;
	pointer envir;
	pointer code;
};

#define STACK_GROWTH 3

static void s_save(scheme *sc, enum scheme_opcodes op,
		   pointer args, pointer code)
{
	int i=0, saved_size=0;
	char *source, *dest;
	int nframes = (int)sc->dump;
	struct dump_stack_frame *next_frame;

	/* enough room for the next frame? */
	if (nframes >= sc->dump_size) {
		saved_size = sizeof(struct dump_stack_frame) * sc->dump_size;
		sc->dump_size += STACK_GROWTH;
		/* alas there is no realloc */
		source = (char*)sc->dump_base;
		dest = (char*)malloc( sc,
				      sizeof(struct dump_stack_frame)
				      * sc->dump_size );
		for( i=0; i<saved_size; i++ )
			*(dest + i) = *(source + i);
		free( sc, source );
		sc->dump_base = (void*)dest;
	}
	next_frame = (struct dump_stack_frame *)sc->dump_base + nframes;
	next_frame->op = op;
	next_frame->args = args;
	next_frame->envir = sc->envir;
	next_frame->code = code;
	sc->dump = (pointer)(nframes+1);
}

static pointer _s_return(scheme *sc, pointer a)
{
	int nframes = (int)sc->dump;
	struct dump_stack_frame *frame;

	sc->value = (a);
	if (nframes <= 0) {
		return sc->NIL;
	}
	nframes--;
	frame = (struct dump_stack_frame *)sc->dump_base + nframes;
	sc->op = frame->op;
	sc->args = frame->args;
	sc->envir = frame->envir;
	sc->code = frame->code;
	sc->dump = (pointer)nframes;
	return sc->T;
}

static void dump_stack_reset(scheme *sc)
{
	/* sc->dump is the number of frames on the stack */
	sc->dump = (pointer)0;
}

static void dump_stack_initialize(scheme *sc)
{
	sc->dump_size = 0;
	sc->dump_base = NULL;
	dump_stack_reset(sc);
}

static void dump_stack_free(scheme *sc)
{
	free( sc, sc->dump_base );
	sc->dump_base = NULL;
	sc->dump = (pointer)0;
	sc->dump_size = 0;
}

static void dump_stack_mark(scheme *sc)
{
	int nframes = (int)sc->dump;
	int i;
	for(i=0; i<nframes; i++) {
		struct dump_stack_frame *frame;
		frame = (struct dump_stack_frame *)sc->dump_base + i;
		mark(frame->args);
		mark(frame->envir);
		mark(frame->code);
	}
}

#else

static void dump_stack_reset(scheme *sc)
{
	sc->dump = sc->NIL;
}

static void dump_stack_initialize(scheme *sc)
{
	dump_stack_reset(sc);
}

static void dump_stack_free(scheme *sc)
{
	sc->dump = sc->NIL;
}

static pointer _s_return(scheme *sc, pointer a)
{
	sc->value = (a);
	if(sc->dump==sc->NIL) return sc->NIL;
	sc->op = long_value_unchecked(car(sc->dump));
	sc->args = cadr(sc->dump);
	sc->envir = caddr(sc->dump);
	sc->code = cadddr(sc->dump);
	sc->dump = cddddr(sc->dump);
	return sc->T;
}

static void s_save(scheme *sc, enum scheme_opcodes op,
		   pointer args, pointer code)
{
	sc->dump = cons(sc, sc->envir, cons(sc, (code), sc->dump));
	sc->dump = cons(sc, (args), sc->dump);
	sc->dump = cons(sc, mk_integer(sc, (long)(op)), sc->dump);
}

static void dump_stack_mark(scheme *sc)
{
	mark(sc->dump);
}
#endif

static int list_length(scheme *sc, pointer a)
{
	int v=0;
	pointer x;
	for (x = a, v = 0; is_pair(x); x = cdr(x)) {
		++v;
	}
	if(x==sc->NIL) {
		return v;
	}
	return -1;
}

static pointer op_toplevel(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_T0LVL: /* top level */
		dump_stack_reset(sc);
		sc->envir = sc->global_env;
		sc->inport = sc->loadport;
		s_save(sc,OP_T0LVL, sc->NIL, sc->NIL);
		s_save(sc,OP_VALUEPRINT, sc->NIL, sc->NIL);
		s_save(sc,OP_T1LVL, sc->NIL, sc->NIL);
		s_goto(sc,OP_READ_INTERNAL);

	case OP_REP: /* one-shot top level */
		dump_stack_reset(sc);
		sc->envir = sc->global_env;
		sc->inport = sc->loadport;
		s_save(sc,OP_QUIT, sc->NIL, sc->NIL);
      /* We skip whitespace here to minimise the number of
         times the kernel has to call our device_write hook */
		s_save(sc,OP_SKIP_WHITESPACE, sc->NIL, sc->NIL);
		s_save(sc,OP_VALUEPRINT, sc->NIL, sc->NIL);
		s_save(sc,OP_T1LVL, sc->NIL, sc->NIL);
		s_goto(sc,OP_READ_INTERNAL);

	case OP_T1LVL: /* rest of top level */
		sc->code = sc->value;
		s_goto(sc,OP_EVAL);

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_read(scheme *sc, enum scheme_opcodes op)
{
   pointer x;
	switch (op) {
   case OP_SKIP_WHITESPACE:
      skipspace(sc);
      s_return(sc,sc->value);

	case OP_READ_INTERNAL:       /* internal read */
		sc->tok = token(sc);
		if(sc->tok==TOK_EOF) {
			if(sc->inport==sc->loadport) {
				sc->args=sc->NIL;
				s_goto(sc,OP_QUIT);
			} else {
				s_return(sc,sc->EOF_OBJ);
			}
		}
		s_goto(sc,OP_RDSEXPR);

	case OP_READ:
		if(!is_pair(sc->args)) {
			s_goto(sc,OP_READ_INTERNAL);
		}
		if(!is_inport(car(sc->args))) {
			Error_1(sc,"read: not an input port:",car(sc->args));
		}
		if(car(sc->args)==sc->inport) {
			s_goto(sc,OP_READ_INTERNAL);
		}
		x=sc->inport;
		sc->inport=car(sc->args);
		x=cons(sc,x,sc->NIL);
		s_save(sc,OP_SET_INPORT, x, sc->NIL);
		s_goto(sc,OP_READ_INTERNAL);

	case OP_READ_CHAR: /* read-char */
	case OP_PEEK_CHAR: /* peek-char */ {
		int c;
		if(is_pair(sc->args)) {
			if(car(sc->args)!=sc->inport) {
				x=sc->inport;
				x=cons(sc,x,sc->NIL);
				s_save(sc,OP_SET_INPORT, x, sc->NIL);
				sc->inport=car(sc->args);
			}
		}
		c=inchar(sc);
		if(c==EOF) {
			s_return(sc,sc->EOF_OBJ);
		}
		if(sc->op==OP_PEEK_CHAR) {
			backchar(sc,c);
		}
		s_return(sc,mk_character(sc,c));
	}

	case OP_CHAR_READY: /* char-ready? */ {
		pointer p=sc->inport;
		int res;
		if(is_pair(sc->args)) {
			p=car(sc->args);
		}
		res=p->_object._port->kind&port_string;
		s_retbool(res);
	}

	case OP_SET_INPORT: /* set-input-port */
		sc->inport=car(sc->args);
		s_return(sc,sc->value);

	case OP_SET_OUTPORT: /* set-output-port */
		sc->outport=car(sc->args);
		s_return(sc,sc->value);

	case OP_RDSEXPR:
		/*printk(KERN_ALERT "Schemix: reading something starting with token %d\n", sc->tok);*/
		switch (sc->tok) {
		case TOK_EOF:
			if(sc->inport==sc->loadport) {
				sc->args=sc->NIL;
				s_goto(sc,OP_QUIT);
			} else {
				s_return(sc,sc->EOF_OBJ);
			}
		case TOK_COMMENT: {
			int c;
			while ((c=inchar(sc)) != '\n' && c!=EOF)
				;
			sc->tok = token(sc);
			s_goto(sc,OP_RDSEXPR);
		}
		case TOK_VEC:
			s_save(sc,OP_RDVEC,sc->NIL,sc->NIL);
			/* fall through */
		case TOK_LPAREN:
			sc->tok = token(sc);
			if (sc->tok == TOK_RPAREN) {
				s_return(sc,sc->NIL);
			} else if (sc->tok == TOK_DOT) {
				Error_0(sc,
					"syntax error: illegal '.'");
			} else {
				sc->open_parens++;
				s_save(sc,OP_RDLIST, sc->NIL, sc->NIL);
				s_goto(sc,OP_RDSEXPR);
			}
		case TOK_QUOTE:
			s_save(sc,OP_RDQUOTE, sc->NIL, sc->NIL);
			sc->tok = token(sc);
			s_goto(sc,OP_RDSEXPR);
		case TOK_BQUOTE:
			sc->tok = token(sc);
			if(sc->tok==TOK_VEC) {
				s_save(sc,OP_RDQQUOTEVEC, sc->NIL, sc->NIL);
				sc->tok=TOK_LPAREN;
				s_goto(sc,OP_RDSEXPR);
			} else {
				s_save(sc,OP_RDQQUOTE, sc->NIL, sc->NIL);
			}
			s_goto(sc,OP_RDSEXPR);
		case TOK_COMMA:
			s_save(sc,OP_RDUNQUOTE, sc->NIL, sc->NIL);
			sc->tok = token(sc);
			s_goto(sc,OP_RDSEXPR);
		case TOK_ATMARK:
			s_save(sc,OP_RDUQTSP, sc->NIL, sc->NIL);
			sc->tok = token(sc);
			s_goto(sc,OP_RDSEXPR);
		case TOK_ATOM:
			s_return(sc,
				 mk_atom(sc, readstr_upto(sc, "();\t\n\r \"")));
		case TOK_DQUOTE:
			x=readstrexp(sc);
			if(x==sc->F) {
				Error_0(sc,"Error reading string");
			}
			setimmutable(x);
			s_return(sc,x);
		case TOK_SHARP: {
			pointer f=find_slot_in_env(sc,
						   sc->envir,
						   sc->SHARP_HOOK,
						   1);
			if(f==sc->NIL) {
				Error_0(sc,"undefined sharp expression");
			} else {
				sc->code=cons(sc,
					      slot_value_in_env(sc,f),
					      sc->NIL);
				s_goto(sc,OP_EVAL);
			}
		}
		case TOK_SHARP_CONST:
			if((x = mk_sharp_const(sc,
					       readstr_upto(sc, "();\t\n\r ")))
			    == sc->NIL) {
				Error_0(sc,"undefined sharp expression");
			} else {
				s_return(sc,x);
			}
		default:
			Error_0(sc,"syntax error: illegal token");
		}
		break;

	case OP_RDLIST: {
		sc->args = cons(sc, sc->value, sc->args);
		sc->tok = token(sc);
		if (sc->tok == TOK_COMMENT) {
			int c;
			while ((c=inchar(sc)) != '\n' && c!=EOF)
				;
			sc->tok = token(sc);
		}
		if (sc->tok == TOK_RPAREN) {
			int c = inchar(sc);
			if (c != '\n') backchar(sc,c);
			sc->open_parens--;
			s_return(sc,reverse_in_place(sc, sc->NIL, sc->args));
		} else if (sc->tok == TOK_DOT) {
			s_save(sc,OP_RDDOT, sc->args, sc->NIL);
			sc->tok = token(sc);
			s_goto(sc,OP_RDSEXPR);
		} else {
			s_save(sc,OP_RDLIST, sc->args, sc->NIL);;
			s_goto(sc,OP_RDSEXPR);
		}
	}

	case OP_RDDOT:
		if (token(sc) != TOK_RPAREN) {
			Error_0(sc,"syntax error: illegal dot expression");
		} else {
			sc->open_parens--;
			s_return(sc,reverse_in_place(sc, sc->value, sc->args));
		}

	case OP_RDQUOTE:
		s_return(sc,cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL)));

	case OP_RDQQUOTE:
		s_return(sc,
			 cons(sc, sc->QQUOTE, cons(sc, sc->value, sc->NIL)));

	case OP_RDQQUOTEVEC:
		s_return(sc,cons(sc, mk_symbol(sc,"apply"),
				 cons(sc, mk_symbol(sc,"vector"),
				      cons(sc,cons(sc, sc->QQUOTE,
						   cons(sc,sc->value,sc->NIL)),
					   sc->NIL))));

	case OP_RDUNQUOTE:
		s_return(sc,
			 cons(sc, sc->UNQUOTE, cons(sc, sc->value, sc->NIL)));

	case OP_RDUQTSP:
		s_return(sc,
			 cons(sc,
			      sc->UNQUOTESP,
			      cons(sc, sc->value, sc->NIL)));

	case OP_RDVEC:
		/*sc->code=cons(sc,mk_proc(sc,OP_VECTOR),sc->value);
		  s_goto(sc,OP_EVAL); Cannot be quoted*/
		/*x=cons(sc,mk_proc(sc,OP_VECTOR),sc->value);
		  s_return(sc,x); Cannot be part of pairs*/
		/*sc->code=mk_proc(sc,OP_VECTOR);
		  sc->args=sc->value;
		  s_goto(sc,OP_APPLY);*/
		sc->args=sc->value;
		s_goto(sc,OP_VECTOR);

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_gensym(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_GENSYM:
		s_return(sc, gensym(sc));
	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_print(scheme *sc, enum scheme_opcodes op)
{
   pointer x;
	switch (op) {
	case OP_P0LIST:
		if(is_vector(sc->args)) {
			putstr(sc,"#(");
			sc->args=cons(sc,sc->args,mk_integer(sc,0));
			s_goto(sc,OP_PVECFROM);
		} else if(is_environment(sc->args)) {
			putstr(sc,"#<ENVIRONMENT>");
			s_return(sc,sc->T);
		} else if (!is_pair(sc->args)) {
			printatom(sc, sc->args, sc->print_flag);
			s_return(sc,sc->T);
		} else if (car(sc->args) == sc->QUOTE
			   && ok_abbrev(cdr(sc->args))) {
			putstr(sc, "'");
			sc->args = cadr(sc->args);
			s_goto(sc,OP_P0LIST);
		} else if (car(sc->args) == sc->QQUOTE
			   && ok_abbrev(cdr(sc->args))) {
			putstr(sc, "`");
			sc->args = cadr(sc->args);
			s_goto(sc,OP_P0LIST);
		} else if (car(sc->args) == sc->UNQUOTE
			   && ok_abbrev(cdr(sc->args))) {
			putstr(sc, ",");
			sc->args = cadr(sc->args);
			s_goto(sc,OP_P0LIST);
		} else if (car(sc->args) == sc->UNQUOTESP
			   && ok_abbrev(cdr(sc->args))) {
			putstr(sc, ",@");
			sc->args = cadr(sc->args);
			s_goto(sc,OP_P0LIST);
		} else {
			putstr(sc, "(");
			s_save(sc,OP_P1LIST, cdr(sc->args), sc->NIL);
			sc->args = car(sc->args);
			s_goto(sc,OP_P0LIST);
		}

	case OP_P1LIST:
		if (is_pair(sc->args)) {
			s_save(sc,OP_P1LIST, cdr(sc->args), sc->NIL);
			putstr(sc, " ");
			sc->args = car(sc->args);
			s_goto(sc,OP_P0LIST);
		} else if(is_vector(sc->args)) {
			s_save(sc,OP_P1LIST,sc->NIL,sc->NIL);
			putstr(sc, " . ");
			s_goto(sc,OP_P0LIST);
		} else {
			if (sc->args != sc->NIL) {
				putstr(sc, " . ");
				printatom(sc, sc->args, sc->print_flag);
			}
			putstr(sc, ")");
			s_return(sc,sc->T);
		}
	case OP_PVECFROM: {
		int i=long_value_unchecked(cdr(sc->args));
		pointer vec=car(sc->args);
		int len=long_value_unchecked(vec);
		if(i==len) {
			putstr(sc,")");
			s_return(sc,sc->T);
		} else {
			pointer elem=vector_elem(vec,i);
			long_value_unchecked(cdr(sc->args))=i+1;
			s_save(sc,OP_PVECFROM, sc->args, sc->NIL);
			sc->args=elem;
			putstr(sc," ");
			s_goto(sc,OP_P0LIST);
		}
	}

	case OP_VALUEPRINT: /* print evaluation result */
		s_save(sc,OP_NEWLINE, sc->NIL, sc->NIL);
		sc->print_flag = 1;
		sc->args = sc->value;
		s_goto(sc,OP_P0LIST);

	case OP_WRITE:      /* write */
	case OP_DISPLAY:    /* display */
	case OP_WRITE_CHAR: /* write-char */
		if(is_pair(cdr(sc->args))) {
			if(cadr(sc->args)!=sc->outport) {
				x=cons(sc,sc->outport,sc->NIL);
				s_save(sc,OP_SET_OUTPORT, x, sc->NIL);
				sc->outport=cadr(sc->args);
			}
		}
		sc->args = car(sc->args);
		if(op==OP_WRITE) {
			sc->print_flag = 1;
		} else {
			sc->print_flag = 0;
		}
		s_goto(sc,OP_P0LIST);

	case OP_NEWLINE:    /* newline */
		if(is_pair(sc->args)) {
			if(car(sc->args)!=sc->outport) {
				x=cons(sc,sc->outport,sc->NIL);
				s_save(sc,OP_SET_OUTPORT, x, sc->NIL);
				sc->outport=car(sc->args);
			}
		}
		putstr(sc, "\n");
		s_return(sc,sc->T);

	case OP_ERR0:  /* error */
		sc->retcode=-1;
		if (!is_string(car(sc->args))) {
			sc->args=cons(sc,mk_string(sc," -- "),sc->args);
			setimmutable(car(sc->args));
		}
		putstr(sc, "Error: ");
		putstr(sc, strvalue(car(sc->args)));
		sc->args = cdr(sc->args);
		s_goto(sc,OP_ERR1);

	case OP_ERR1:  /* error */
		putstr(sc, " ");
		if (sc->args != sc->NIL) {
			s_save(sc,OP_ERR1, cdr(sc->args), sc->NIL);
			sc->args = car(sc->args);
			sc->print_flag = 1;
			s_goto(sc,OP_P0LIST);
		} else {
			putstr(sc, "\n");
			return sc->NIL;
		}

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_eval(scheme *sc, enum scheme_opcodes op)
{
   pointer x;
	switch (op) {
	case OP_EVAL:       /* main part of evaluation */
		if (is_symbol(sc->code) || is_memory_reference(sc->code) ) {
			/* symbol (Scheme or C style) */
			x=find_slot_in_env(sc,sc->envir,sc->code,1);
			if (x != sc->NIL) {
				s_return(sc,slot_value_in_env(sc,x));
			} else {
				Error_1(sc,"eval: unbound variable:",sc->code);
			}
		} else if (is_pair(sc->code)) {
			if (is_syntax(x = car(sc->code))) {     /* SYNTAX */
				sc->code = cdr(sc->code);
				s_goto(sc,syntaxnum(x));
			} else {/* first eval top element and eval arguments */
				s_save(sc,OP_E0ARGS, sc->NIL, sc->code);
				sc->code = car(sc->code);
				s_goto(sc,OP_EVAL);
			}
		} else {
			s_return(sc,sc->code);
		}

	case OP_E0ARGS:     /* eval arguments */
		if (is_macro(sc->value)) {    /* macro expansion */
			s_save(sc,OP_DOMACRO, sc->NIL, sc->NIL);
			sc->args = cons(sc,sc->code, sc->NIL);
			sc->code = sc->value;
			s_goto(sc,OP_APPLY);
		} else {
			sc->code = cdr(sc->code);
			s_goto(sc,OP_E1ARGS);
		}

	case OP_E1ARGS:     /* eval arguments */
		sc->args = cons(sc, sc->value, sc->args);
		if (is_pair(sc->code)) { /* continue */
			s_save(sc,OP_E1ARGS, sc->args, cdr(sc->code));
			sc->code = car(sc->code);
			sc->args = sc->NIL;
			s_goto(sc,OP_EVAL);
		} else {  /* end */
			sc->args = reverse_in_place(sc, sc->NIL, sc->args);
			sc->code = car(sc->args);
			sc->args = cdr(sc->args);
			s_goto(sc,OP_APPLY);
		}

	case OP_PEVAL: /* eval */
		if(cdr(sc->args)!=sc->NIL) {
			sc->envir=cadr(sc->args);
		}
		sc->code = car(sc->args);
		s_goto(sc,OP_EVAL);

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_apply(scheme *sc, enum scheme_opcodes op)
{
   pointer x, y;
	switch (op) {
	case OP_APPLY:      /* apply 'code' to 'args' */
		if (is_proc(sc->code)) {
			s_goto(sc,procnum(sc->code));   /* PROCEDURE */
		}
#if SCHEMIX_USE_KALLSYMS
		else if (is_kernel_proc(sc->code)) { /* KERNEL-PROCEDURE */
			apply_kernel_procedure(sc, sc->code, sc->args);
			s_return(sc, sc->NIL);
		}
#endif
		else if (is_closure(sc->code) || is_macro(sc->code)
			 || is_promise(sc->code)) { /* CLOSURE */
			/* Should not accept promise */
			/* make environment */
			new_frame_in_env(sc, closure_env(sc->code));
			for (x = car(closure_code(sc->code)), y = sc->args;
			     is_pair(x); x = cdr(x), y = cdr(y)) {
				if (y == sc->NIL) {
					Error_0(sc,"not enough arguments");
				} else {
					new_slot_in_env(sc, car(x), car(y));
				}
			}
			if (x == sc->NIL) {
				/*--
				 * if (y != sc->NIL) {
				 *   Error_0(sc,"too many arguments");
				 * }
				 */
			} else if (is_symbol(x))
				new_slot_in_env(sc, x, y);
			else {
				Error_1(sc,
					"syntax error in closure: non-symbol:",
					x);
			}
			sc->code = cdr(closure_code(sc->code));
			sc->args = sc->NIL;
			s_goto(sc,OP_BEGIN);
		} else if (is_continuation(sc->code)) { /* CONTINUATION */
			sc->dump = cont_dump(sc->code);
			s_return(sc, car(sc->args));
		} else {
			Error_1(sc,"illegal function",sc->code);
		}

	case OP_PAPPLY:     /* apply */
		sc->code = car(sc->args);
		sc->args = list_star(sc,cdr(sc->args));
		s_goto(sc,OP_APPLY);

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_lambda(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_LAMBDA:     /* lambda */
		s_return(sc,mk_closure(sc, sc->code, sc->envir));
	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

#if SCHEMIX_USE_DEVFS
static pointer op_device(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_MAKE_DEVICE:   /* (make-device name (bindings)) */
		if( !is_symbol(car(sc->code)) )
			Error_1(sc,
				"First argument to make-device not a symbol",
				car(sc->code));
		else if( !is_pair(cadr(sc->code))
			 && (cadr(sc->code) != sc->NIL))
			Error_1(sc,"Second argument to make-device not a list",
				cadr(sc->code));
		else
			s_return(sc,mk_device(sc,
					      car(sc->code),
					      cadr(sc->code)));

	case OP_DEVICEP:     /* device? */
		s_retbool(is_device(car(sc->args)));

	case OP_DESTROY_DEVICE:
		if( car(sc->args)->_object._scheme ) {
			scheme_load_string( car(sc->args)->_object._scheme,
					    "(exit)\n" ); /* FIXME: Yuck! */
			car(sc->args)->_object._scheme = NULL;
		}
		s_return(sc,sc->T);
	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}
#endif

#if SCHEMIX_USE_KALLSYMS
static pointer op_ctype(scheme *sc, enum scheme_opcodes op)
{
   pointer x;
   unsigned long i, j;
	switch (op) {
	case OP_KERNEL_LAMBDA: /* (kernel-lambda (arg1 ... argn) name) */
		s_return(sc,mk_kernel_procedure(sc, car(sc->code),
						cadr(sc->code)));

	case OP_MAKE_STRUCTURE:
		x = mk_structure(sc, car(sc->args));
		if( x != sc->F )
			s_return(sc,mk_structure(sc, car(sc->args)));
		else
			Error_0(sc,"make-structure: cannot parse typespec!");

	case OP_ASSERT_TYPE:
		j = 1; /* j holds the total number of elements in array */
		/* check all elements of dimension list are numbers */
		if( is_pair(car(sc->args)) )
			for(x = cdar(sc->args); is_pair(x); x = cdr(x))
			{
				if(!is_number(car(x)))
					Error_1(sc,
						"assert-type: non-numeric dimension",
						car(sc->args));
				else
					j *= long_value_unchecked(car(x));
			}
		if( is_symbol(car(sc->args))
		    || (is_pair(car(sc->args)) && is_symbol(caar(sc->args))) )
		{
			x = (is_pair(car(sc->args))
			     ? caar(sc->args)
			     : car(sc->args));
			for( i = 0;
			     i < structures_table_length
				     && strcmp(structures_table[i].name,
					       symname(x)) != 0;
			     i++ )
			{
				if( sc == NULL || sc->DEBUG )
					printk(KERN_ALERT "Schemix found structure type %s\n",
					       structures_table[i].name);
			}
			/* Can't find structure name */
			if( i == structures_table_length )
				Error_1(sc,
					"assert-type: unknown type:",
					car(sc->args));
			else if( mem_ref(cadr(sc->args))->length
				 < j * structures_table[i].size )
				Error_0(sc,"assert-type: not enough space");
			else
			{
				mem_ref(cadr(sc->args))->type = i;
				mem_ref(cadr(sc->args))->dim
					= (is_pair(car(sc->args))
					   ? cdar(sc->args)
					   : sc->NIL);
			}
		}
		else
			Error_1(sc,
				"assert-type: cannot parse typespec:",
				car(sc->args));
		s_return(sc,sc->T);
	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}
#endif /* SCHEMIX_USE_KALLSYMS */

#if SCHEMIX_USE_KALLSYMS
static pointer op_array(scheme *sc, enum scheme_opcodes op)
{
   pointer x, y, z;
   unsigned long i;
	switch (op) {
	case OP_ARRAYP:
		s_retbool(is_array(car(sc->args)));

	case OP_ARRAY_RANK:
		s_return(sc,
			 mk_integer(sc,
				    list_length(sc,
						mem_ref(car(sc->args))->dim)));

	case OP_ARRAY_START:
		s_return(sc,sc->ZERO); /* All arrays are zero-based */

	case OP_ARRAY_END:
		i = long_value_unchecked(cadr(sc->args));
		if( i < 0 )
			Error_1(sc,
				"array-end: negative dimension:",
				cadr(sc->args));
		x = mem_ref(car(sc->args))->dim;
		while( is_pair(x) && i-- > 0 )
			x = cdr(x);
		if( !is_pair(x) )
			Error_1(sc,
				"array-end: dimension larger than array rank:",
				cadr(sc->args));
		s_return(sc,car(x));

	case OP_ARRAY_REF: /* FIXME: add bounds checking */
		x = car(sc->args);
		if( !is_array(x) )
			Error_1(sc,
				"array-ref: first argument is not an array:",
				x);
		i = flatten_array_ref(sc, cdr(sc->args), mem_ref(x)->dim);
		if( i < 0 )
			Error_1(sc,
				"array-ref: wrong number of array coordinates",
				cdr(sc->args));
		s_return(sc,box_value(sc, x, i, 1));

	case OP_ARRAY_SET: /* FIXME: add bounds checking */
		x = car(sc->args);
		if( !is_array(x) )
			Error_1(sc,
				"array-ref: first argument is not an array:",
				x);
		y = sc->args;
		while(is_pair(cddr(y))) /* we know there are >=3 args */
			y = cdr(y);
		z = cdr(y); /* z is the last cons cell */
		cdr(y) = sc->NIL; /* remove last element for flatten_array */
		i = flatten_array_ref(sc, cdr(sc->args), mem_ref(x)->dim);
		if( i < 0 )
			Error_1(sc,
				"array-ref: wrong number of array coordinates",
				cdr(sc->args));
		cdr(y) = z; /* put back the last element */
		set_memory(sc, x, car(z), i, 1);
		s_return(sc, sc->NIL);
	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}
#endif /* SCHEMIX_USE_KALLSYMS */

static pointer op_closure(scheme *sc, enum scheme_opcodes op)
{
   pointer x, y;
	switch (op) {
	case OP_MKCLOSURE: /* make-closure */
		x=car(sc->args);
		if(car(x)==sc->LAMBDA) {
			x=cdr(x);
		}
		if(cdr(sc->args)==sc->NIL) {
			y=sc->envir;
		} else {
			y=cadr(sc->args);
		}
		s_return(sc,mk_closure(sc, x, y));

	case OP_GET_CLOSURE:     /* get-closure-code */   /* a.k */
		sc->args = car(sc->args);
		if (sc->args == sc->NIL) {
			s_return(sc,sc->F);
		} else if (is_closure(sc->args)) {
			s_return(sc, cons(sc,
					  sc->LAMBDA,
					  closure_code(sc->value)));
		} else if (is_macro(sc->args)) {
			s_return(sc,cons(sc,
					 sc->LAMBDA,
					 closure_code(sc->value)));
		} else {
			s_return(sc,sc->F);
		}

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_quote(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_QUOTE:      /* quote */
		s_return(sc, car(sc->code));
	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_define(scheme *sc, enum scheme_opcodes op)
{
   pointer x;
	switch (op) {
	case OP_DEF0:  /* define */
		if (is_pair(car(sc->code))) {
			x = caar(sc->code);
			sc->code = cons(sc,
					sc->LAMBDA,
					cons(sc,
					     cdar(sc->code),
					     cdr(sc->code)));
		} else {
			x = car(sc->code);
			sc->code = cadr(sc->code);
		}
		if (!is_symbol(x)) {
			Error_0(sc,"variable is not a symbol");
		}
		s_save(sc,OP_DEF1, sc->NIL, x);
		s_goto(sc,OP_EVAL);

	case OP_DEF1:  /* define */
		x=find_slot_in_env(sc,sc->envir,sc->code,0);
		if (x != sc->NIL) {
			if( !set_slot_in_env(sc, x, sc->value) )
				Error_1(sc,"define: cannot set argument", x);
		} else {
			new_slot_in_env(sc, sc->code, sc->value);
		}
		s_return(sc,sc->code);


	case OP_DEFP:  /* defined? */
		x=sc->envir;
		if(cdr(sc->args)!=sc->NIL) {
			x=cadr(sc->args);
		}
		s_retbool(find_slot_in_env(sc,x,car(sc->args),1)!=sc->NIL);

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_debug(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_HELP:
		s_return(sc, help(sc, car(sc->args)));

	case OP_PRINTK_DEBUGGING:
		if( car(sc->args) == sc->F )
			sc->DEBUG = 0;
		else
			sc->DEBUG = 1;
		s_return(sc, sc->T);

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_set(scheme *sc, enum scheme_opcodes op)
{
   pointer x;
	switch (op) {
	case OP_SET0:       /* set! */
		s_save(sc,OP_SET1, sc->NIL, car(sc->code));
		sc->code = cadr(sc->code);
		s_goto(sc,OP_EVAL);

	case OP_SET1:       /* set! */
		x=find_slot_in_env(sc,sc->envir,sc->code,1);
		if (x != sc->NIL) {
			if( !set_slot_in_env(sc, x, sc->value) )
				Error_1(sc,"define: cannot set argument", x);
			s_return(sc,sc->value);
		} else {
			Error_1(sc,"set!: unbound variable:", sc->code);
		}
	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_begin(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_BEGIN:      /* begin */
		if (!is_pair(sc->code)) {
			s_return(sc,sc->code);
		}
		if (cdr(sc->code) != sc->NIL) {
			s_save(sc,OP_BEGIN, sc->NIL, cdr(sc->code));
		}
		sc->code = car(sc->code);
		s_goto(sc,OP_EVAL);
	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_if(scheme *sc, enum scheme_opcodes op)
{
   pointer x, y;
	switch (op) {
	case OP_IF0:        /* if */
		s_save(sc,OP_IF1, sc->NIL, cdr(sc->code));
		sc->code = car(sc->code);
		s_goto(sc,OP_EVAL);

	case OP_IF1:        /* if */
		if (is_true(sc->value))
			sc->code = car(sc->code);
		else
			sc->code = cadr(sc->code);  /* (if #f 1) ==> () because
						     * car(NIL) = NIL */
		s_goto(sc,OP_EVAL);

	case OP_COND0:      /* cond */
		if (!is_pair(sc->code)) {
			Error_0(sc,"syntax error in cond");
		}
		s_save(sc,OP_COND1, sc->NIL, sc->code);
		sc->code = caar(sc->code);
		s_goto(sc,OP_EVAL);

	case OP_COND1:      /* cond */
		if (is_true(sc->value)) {
			if ((sc->code = cdar(sc->code)) == sc->NIL) {
				s_return(sc,sc->value);
			}
			if(car(sc->code)==sc->FEED_TO) {
				if(!is_pair(cdr(sc->code))) {
					Error_0(sc,"syntax error in cond");
				}
				x=cons(sc,
				       sc->QUOTE,
				       cons(sc, sc->value, sc->NIL));
				sc->code=cons(sc,
					      cadr(sc->code),
					      cons(sc,x,sc->NIL));
				s_goto(sc,OP_EVAL);
			}
			s_goto(sc,OP_BEGIN);
		} else {
			if ((sc->code = cdr(sc->code)) == sc->NIL) {
				s_return(sc,sc->NIL);
			} else {
				s_save(sc,OP_COND1, sc->NIL, sc->code);
				sc->code = caar(sc->code);
				s_goto(sc,OP_EVAL);
			}
		}

	case OP_CASE0:      /* case */
		s_save(sc,OP_CASE1, sc->NIL, cdr(sc->code));
		sc->code = car(sc->code);
		s_goto(sc,OP_EVAL);

	case OP_CASE1:      /* case */
		for (x = sc->code; is_pair(x); x = cdr(x)) {
			if (!is_pair(y = caar(x))) {
				break;
			}
			for ( ; is_pair(y); y = cdr(y)) {
				if (eqv(car(y), sc->value)) {
					break;
				}
			}
			if (is_pair(y)) {
				break;
			}
		}
		if (is_pair(x)) {
			if (is_pair(caar(x))) {
				sc->code = cdar(x);
				s_goto(sc,OP_BEGIN);
			} else {/* else */
				s_save(sc,OP_CASE2, sc->NIL, cdar(x));
				sc->code = caar(x);
				s_goto(sc,OP_EVAL);
			}
		} else {
			s_return(sc,sc->NIL);
		}

	case OP_CASE2:      /* case */
		if (is_true(sc->value)) {
			s_goto(sc,OP_BEGIN);
		} else {
			s_return(sc,sc->NIL);
		}

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_let(scheme *sc, enum scheme_opcodes op)
{
   pointer x, y;
	switch (op) {
	case OP_LET0:       /* let */
		sc->args = sc->NIL;
		sc->value = sc->code;
		sc->code = is_symbol(car(sc->code))
			? cadr(sc->code)
			: car(sc->code);
		s_goto(sc,OP_LET1);

	case OP_LET1:       /* let (calculate parameters) */
		sc->args = cons(sc, sc->value, sc->args);
		if (is_pair(sc->code)) { /* continue */
			s_save(sc,OP_LET1, sc->args, cdr(sc->code));
			sc->code = cadar(sc->code);
			sc->args = sc->NIL;
			s_goto(sc,OP_EVAL);
		} else {  /* end */
			sc->args = reverse_in_place(sc, sc->NIL, sc->args);
			sc->code = car(sc->args);
			sc->args = cdr(sc->args);
			s_goto(sc,OP_LET2);
		}

	case OP_LET2:       /* let */
		new_frame_in_env(sc, sc->envir);
		for (x = is_symbol(car(sc->code))
			     ? cadr(sc->code)
			     : car(sc->code),
			     y = sc->args;
		     is_pair(y); x = cdr(x), y = cdr(y)) {
			new_slot_in_env(sc, caar(x), car(y));
		}
		if (is_symbol(car(sc->code))) {    /* named let */
			for (x = cadr(sc->code), sc->args = sc->NIL;
			     is_pair(x);
			     x = cdr(x)) {
				sc->args = cons(sc, caar(x), sc->args);
			}
			x = mk_closure(sc,
				       cons(sc,
					    reverse_in_place(sc,
							     sc->NIL,
							     sc->args),
					    cddr(sc->code)),
				       sc->envir);
			new_slot_in_env(sc, car(sc->code), x);
			sc->code = cddr(sc->code);
			sc->args = sc->NIL;
		} else {
			sc->code = cdr(sc->code);
			sc->args = sc->NIL;
		}
		s_goto(sc,OP_BEGIN);

	case OP_LET0AST:    /* let* */
		if (car(sc->code) == sc->NIL) {
			new_frame_in_env(sc, sc->envir);
			sc->code = cdr(sc->code);
			s_goto(sc,OP_BEGIN);
		}
		s_save(sc,OP_LET1AST, cdr(sc->code), car(sc->code));
		sc->code = cadaar(sc->code);
		s_goto(sc,OP_EVAL);

	case OP_LET1AST:    /* let* (make new frame) */
		new_frame_in_env(sc, sc->envir);
		s_goto(sc,OP_LET2AST);

	case OP_LET2AST:    /* let* (calculate parameters) */
		new_slot_in_env(sc, caar(sc->code), sc->value);
		sc->code = cdr(sc->code);
		if (is_pair(sc->code)) { /* continue */
			s_save(sc,OP_LET2AST, sc->args, sc->code);
			sc->code = cadar(sc->code);
			sc->args = sc->NIL;
			s_goto(sc,OP_EVAL);
		} else {  /* end */
			sc->code = sc->args;
			sc->args = sc->NIL;
			s_goto(sc,OP_BEGIN);
		}

	case OP_LET0REC:    /* letrec */
		new_frame_in_env(sc, sc->envir);
		sc->args = sc->NIL;
		sc->value = sc->code;
		sc->code = car(sc->code);
		s_goto(sc,OP_LET1REC);

	case OP_LET1REC:    /* letrec (calculate parameters) */
		sc->args = cons(sc, sc->value, sc->args);
		if (is_pair(sc->code)) { /* continue */
			s_save(sc,OP_LET1REC, sc->args, cdr(sc->code));
			sc->code = cadar(sc->code);
			sc->args = sc->NIL;
			s_goto(sc,OP_EVAL);
		} else {  /* end */
			sc->args = reverse_in_place(sc, sc->NIL, sc->args);
			sc->code = car(sc->args);
			sc->args = cdr(sc->args);
			s_goto(sc,OP_LET2REC);
		}

	case OP_LET2REC:    /* letrec */
		for (x = car(sc->code), y = sc->args;
		     is_pair(y);
		     x = cdr(x), y = cdr(y)) {
			new_slot_in_env(sc, caar(x), car(y));
		}
		sc->code = cdr(sc->code);
		sc->args = sc->NIL;
		s_goto(sc,OP_BEGIN);

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_delay(scheme *sc, enum scheme_opcodes op)
{
	pointer x;
	switch (op) {
	case OP_DELAY:      /* delay */
		x = mk_closure(sc, cons(sc, sc->NIL, sc->code), sc->envir);
		typeflag(x)=T_PROMISE;
		s_return(sc,x);

	case OP_FORCE:      /* force */
		sc->code = car(sc->args);
		if (is_promise(sc->code)) {
			/* Should change type to closure here */
			s_save(sc, OP_SAVE_FORCED, sc->NIL, sc->code);
			sc->args = sc->NIL;
			s_goto(sc,OP_APPLY);
		} else {
			s_return(sc,sc->code);
		}

	case OP_SAVE_FORCED:     /* Save forced value replacing promise */
		memcpy(sc->code,sc->value,sizeof(struct cell));
		s_return(sc,sc->value);

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_logic(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_NOT:        /* not */
		s_retbool(is_false(car(sc->args)));

	case OP_AND0:       /* and */
		if (sc->code == sc->NIL) {
			s_return(sc,sc->T);
		}
		s_save(sc,OP_AND1, sc->NIL, cdr(sc->code));
		sc->code = car(sc->code);
		s_goto(sc,OP_EVAL);

	case OP_AND1:       /* and */
		if (is_false(sc->value)) {
			s_return(sc,sc->value);
		} else if (sc->code == sc->NIL) {
			s_return(sc,sc->value);
		} else {
			s_save(sc,OP_AND1, sc->NIL, cdr(sc->code));
			sc->code = car(sc->code);
			s_goto(sc,OP_EVAL);
		}

	case OP_OR0:        /* or */
		if (sc->code == sc->NIL) {
			s_return(sc,sc->F);
		}
		s_save(sc,OP_OR1, sc->NIL, cdr(sc->code));
		sc->code = car(sc->code);
		s_goto(sc,OP_EVAL);

	case OP_OR1:        /* or */
		if (is_true(sc->value)) {
			s_return(sc,sc->value);
		} else if (sc->code == sc->NIL) {
			s_return(sc,sc->value);
		} else {
			s_save(sc,OP_OR1, sc->NIL, cdr(sc->code));
			sc->code = car(sc->code);
			s_goto(sc,OP_EVAL);
		}

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_macro(scheme *sc, enum scheme_opcodes op)
{
	pointer x;
	switch (op) {
	case OP_DOMACRO:    /* do macro */
		sc->code = sc->value;
		s_goto(sc,OP_EVAL);

	case OP_MACRO0:     /* macro */
		if (is_pair(car(sc->code))) {
			x = caar(sc->code);
			sc->code = cons(sc,
					sc->LAMBDA,
					cons(sc,
					     cdar(sc->code),
					     cdr(sc->code)));
		} else {
			x = car(sc->code);
			sc->code = cadr(sc->code);
		}
		if (!is_symbol(x)) {
			Error_0(sc,"variable is not a symbol");
		}
		s_save(sc,OP_MACRO1, sc->NIL, x);
		s_goto(sc,OP_EVAL);

	case OP_MACRO1:     /* macro */
		typeflag(sc->value) = T_MACRO;
		x = find_slot_in_env(sc, sc->envir, sc->code, 0);
		if (x != sc->NIL) {
			set_slot_in_env(sc, x, sc->value);
		} else {
			new_slot_in_env(sc, sc->code, sc->value);
		}
		s_return(sc,sc->code);

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_callcc(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_CONTINUATION:    /* call-with-current-continuation */
		sc->code = car(sc->args);
		sc->args = cons(sc, mk_continuation(sc, sc->dump), sc->NIL);
		s_goto(sc,OP_APPLY);

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_math(scheme *sc, enum scheme_opcodes op)
{
	pointer x;
	num v;

	switch (op) {
	case OP_ADD:        /* + */
		v=num_zero;
		for (x = sc->args; is_pair(x); x = cdr(x)) {
			v=num_add(v,nvalue(car(x)));
		}
		s_return(sc,mk_number(sc, v));

	case OP_MUL:        /* * */
		v=num_one;
		for (x = sc->args; is_pair(x); x = cdr(x)) {
			v=num_mul(v,nvalue(car(x)));
		}
		s_return(sc,mk_number(sc, v));

	case OP_SUB:        /* - */
		if(cdr(sc->args)==sc->NIL) {
			x=sc->args;
			v=num_zero;
		} else {
			x = cdr(sc->args);
			v = nvalue(car(sc->args));
		}
		for (; is_pair(x); x = cdr(x)) {
			v=num_sub(v,nvalue(car(x)));
		}
		s_return(sc,mk_number(sc, v));

	case OP_DIV:        /* / */
		if(cdr(sc->args)==sc->NIL) {
			x=sc->args;
			v=num_one;
		} else {
			x = cdr(sc->args);
			v = nvalue(car(sc->args));
		}
		for (; is_pair(x); x = cdr(x)) {
			if (long_value_unchecked(car(x)) != 0)
				v=num_div(v,nvalue(car(x)));
			else {
				Error_0(sc,"/: division by zero");
			}
		}
		s_return(sc,mk_number(sc, v));

	case OP_REM:        /* remainder */
		v = nvalue(car(sc->args));
		if (long_value_unchecked(cadr(sc->args)) != 0)
			v=num_rem(v,nvalue(cadr(sc->args)));
		else {
			Error_0(sc,"remainder: division by zero");
		}
		s_return(sc,mk_number(sc, v));

	case OP_MOD:        /* modulo */
		v = nvalue(car(sc->args));
		if (long_value_unchecked(cadr(sc->args)) != 0)
			v=num_mod(v,nvalue(cadr(sc->args)));
		else {
			Error_0(sc,"modulo: division by zero");
		}
		s_return(sc,mk_number(sc, v));

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_list(scheme *sc, enum scheme_opcodes op)
{
   pointer x, y;
   long v;
	switch (op) {
	case OP_CAR:        /* car */
		s_return(sc,caar(sc->args));

	case OP_CDR:        /* cdr */
		s_return(sc,cdar(sc->args));

	case OP_CONS:       /* cons */
		cdr(sc->args) = cadr(sc->args);
		s_return(sc,sc->args);

	case OP_SETCAR:     /* set-car! */
		if(!is_immutable(car(sc->args))) {
			caar(sc->args) = cadr(sc->args);
			s_return(sc,car(sc->args));
		} else {
			Error_0(sc,"set-car!: unable to alter immutable pair");
		}

	case OP_SETCDR:     /* set-cdr! */
		if(!is_immutable(car(sc->args))) {
			cdar(sc->args) = cadr(sc->args);
			s_return(sc,car(sc->args));
		} else {
			Error_0(sc,"set-cdr!: unable to alter immutable pair");
		}

	case OP_REVERSE:    /* reverse */
		s_return(sc,reverse(sc, car(sc->args)));

	case OP_LIST_STAR: /* list* */
		s_return(sc,list_star(sc,sc->args));

	case OP_APPEND:     /* append */
		if(sc->args==sc->NIL) {
			s_return(sc,sc->NIL);
		}
		x=car(sc->args);
		if(cdr(sc->args)==sc->NIL) {
			s_return(sc,sc->args);
		}
		for (y = cdr(sc->args); is_pair(y); y = cdr(y)) {
			x=append(sc,x,car(y));
		}
		s_return(sc,x);

	case OP_LIST_LENGTH:     /* length */   /* a.k */
		v=list_length(sc,car(sc->args));
		if(v<0) {
			Error_1(sc,"length: not a list:",car(sc->args));
		}
		s_return(sc,mk_integer(sc, v));

	case OP_ASSQ:       /* assq */     /* a.k */
		x = car(sc->args);
		for (y = cadr(sc->args); is_pair(y); y = cdr(y)) {
			if (!is_pair(car(y))) {
				Error_0(sc,"non pair element in assq");
			}
			if (x == caar(y))
				break;
		}
		if (is_pair(y)) {
			s_return(sc,car(y));
		} else {
			s_return(sc,sc->F);
		}

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_char(scheme *sc, enum scheme_opcodes op)
{
   char c;
	switch (op) {
	case OP_CHAR2INT: { /* char->integer */
		c=(char)long_value_unchecked(car(sc->args));
		s_return(sc,mk_integer(sc,(unsigned char)c));
	}

	case OP_INT2CHAR: { /* integer->char */
		c=long_value_unchecked(car(sc->args));
		s_return(sc,mk_character(sc,c));
	}

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_string(scheme *sc, enum scheme_opcodes op)
{
   pointer x;
	switch (op) {
	case OP_MKSTRING: { /* make-string */
		int fill=' ';
		int len;

		len=long_value_unchecked(car(sc->args));

		if(cdr(sc->args)!=sc->NIL) {
			fill=charvalue(cadr(sc->args));
		}
		s_return(sc,mk_empty_string(sc,len,(char)fill));
	}

	case OP_STRLEN:  /* string-length */
		s_return(sc,mk_integer(sc,strlength(car(sc->args))));

	case OP_STRREF: { /* string-ref */
		char *str;
		int index;

		str=strvalue(car(sc->args));

		index=long_value_unchecked(cadr(sc->args));

		if(index>=strlength(car(sc->args))) {
			Error_1(sc,
				"string-ref: out of bounds:",
				cadr(sc->args));
		}

		s_return(sc,mk_character(sc,((unsigned char*)str)[index]));
	}

	case OP_STRSET: { /* string-set! */
		char *str;
		int index;
		int c;

		if(is_immutable(car(sc->args))) {
			Error_1(sc,
				"string-set!: cannot set immutable string",
				car(sc->args));
		}
		str=strvalue(car(sc->args));

		index=long_value_unchecked(cadr(sc->args));
		if(index>=strlength(car(sc->args))) {
			Error_1(sc,
				"string-set!: out of bounds:",
				cadr(sc->args));
		}

		c=charvalue(caddr(sc->args));

		str[index]=(char)c;
		s_return(sc,car(sc->args));
	}

	case OP_STRAPPEND: { /* string-append */
		int len = 0;
		pointer newstr;
		char *pos;

		/* compute needed length for new string */
		for (x = sc->args; is_pair(x); x = cdr(x)) {
			len += strlength(car(x));
		}
		newstr = mk_empty_string(sc, len, ' ');
		/* store contents of argument strings into the new string */
		for (pos = strvalue(newstr), x = sc->args; is_pair(x);
		     pos += strlength(car(x)), x = cdr(x)) {
			memcpy(pos, strvalue(car(x)), strlength(car(x)));
		}
		s_return(sc, newstr);
	}

	case OP_SUBSTR: { /* substring */
		char *str;
		int index0;
		int index1;
		int len;

		str=strvalue(car(sc->args));

		index0=long_value_unchecked(cadr(sc->args));

		if(index0>strlength(car(sc->args))) {
			Error_1(sc,
				"substring: start out of bounds:",
				cadr(sc->args));
		}

		if(cddr(sc->args)!=sc->NIL) {
			index1=long_value_unchecked(caddr(sc->args));
			if(index1>strlength(car(sc->args)) || index1<index0) {
				Error_1(sc,
					"substring: end out of bounds:",
					caddr(sc->args));
			}
		} else {
			index1=strlength(car(sc->args));
		}

		len=index1-index0;
		x=mk_empty_string(sc,len,' ');
		memcpy(strvalue(x),str+index0,len);
		strvalue(x)[len]=0;

		s_return(sc,x);
	}

	case OP_STR2SYM:  /* string->symbol */
		s_return(sc,mk_symbol(sc,strvalue(car(sc->args))));

	case OP_STR2ATOM: /* string->atom */ {
		char *s=strvalue(car(sc->args));
		if(*s=='#') {
			s_return(sc, mk_sharp_const(sc, s+1));
		} else {
			s_return(sc, mk_atom(sc, s));
		}
	}

	case OP_SYM2STR: /* symbol->string */
		x=mk_string(sc,symname(car(sc->args)));
		setimmutable(x);
		s_return(sc,x);

	case OP_ATOM2STR: /* atom->string */
		x=car(sc->args);
		if(is_number(x)
		   || is_character(x)
		   || is_string(x)
		   || is_symbol(x)) {
			char *p;
			int len;
			atom2str(sc,x,0,&p,&len);
			s_return(sc,mk_counted_string(sc,p,len));
		} else {
			Error_1(sc, "atom->string: not an atom:", x);
		}

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_vector(scheme *sc, enum scheme_opcodes op)
{
   pointer x;
	switch (op) {
	case OP_VECTOR: {   /* vector */
		int i;
		pointer vec;
		int len=list_length(sc,sc->args);
		if(len<0) {
			Error_1(sc,"vector: not a proper list:",sc->args);
		}
		vec=mk_vector(sc,len);
		for (x = sc->args, i = 0; is_pair(x); x = cdr(x), i++) {
			set_vector_elem(vec,i,car(x));
		}
		s_return(sc,vec);
	}

	case OP_MKVECTOR: { /* make-vector */
		pointer fill=sc->NIL;
		int len;
		pointer vec;

		len=long_value_unchecked(car(sc->args));

		if(cdr(sc->args)!=sc->NIL) {
			fill=cadr(sc->args);
		}
		vec=mk_vector(sc,len);
		if(fill!=sc->NIL) {
			fill_vector(vec,fill);
		}
		s_return(sc,vec);
	}

	case OP_VECLEN:  /* vector-length */
		s_return(sc,
			 mk_integer(sc,long_value_unchecked(car(sc->args))));

	case OP_VECREF: { /* vector-ref */
		int index;

		index=long_value_unchecked(cadr(sc->args));

		if(index>=long_value_unchecked(car(sc->args))) {
			Error_1(sc,
				"vector-ref: out of bounds:",
				cadr(sc->args));
		}

		s_return(sc,vector_elem(car(sc->args),index));
	}

	case OP_VECSET: {   /* vector-set! */
		int index;

		if(is_immutable(car(sc->args))) {
			Error_1(sc,
				"vector-set!: cannot set immutable vector",
				car(sc->args));
		}

		index=long_value_unchecked(cadr(sc->args));
		if(index>=long_value_unchecked(car(sc->args))) {
			Error_1(sc,
				"vector-set!: out of bounds:",
				cadr(sc->args));
		}

		set_vector_elem(car(sc->args),index,caddr(sc->args));
		s_return(sc,car(sc->args));
	}

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_type(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_BOOLP:       /* boolean? */
		s_retbool(car(sc->args) == sc->F || car(sc->args) == sc->T);
	case OP_EOFOBJP:       /* boolean? */
		s_retbool(car(sc->args) == sc->EOF_OBJ);
	case OP_NULLP:       /* null? */
		s_retbool(car(sc->args) == sc->NIL);
	case OP_SYMBOLP:     /* symbol? */
		s_retbool(is_symbol(car(sc->args)));
	case OP_NUMBERP:     /* number? */
		s_retbool(is_number(car(sc->args)));
	case OP_STRINGP:     /* string? */
		s_retbool(is_string(car(sc->args)));
	case OP_INTEGERP:     /* integer? */
		s_retbool(is_integer(car(sc->args)));
	case OP_REALP:     /* real? */
		s_retbool(is_number(car(sc->args))); /* All numbers are real */
	case OP_CHARP:     /* char? */
		s_retbool(is_character(car(sc->args)));
	case OP_PORTP:     /* port? */
		s_retbool(is_port(car(sc->args)));
	case OP_INPORTP:     /* input-port? */
		s_retbool(is_inport(car(sc->args)));
	case OP_OUTPORTP:     /* output-port? */
		s_retbool(is_outport(car(sc->args)));
	case OP_PROCP:       /* procedure? */
		/*--
		 * continuation should be procedure by the example
		 * (call-with-current-continuation procedure?) ==> #t
		 * in R^3 report sec. 6.9
		 */
		s_retbool(is_proc(car(sc->args)) || is_closure(car(sc->args))
			  || is_continuation(car(sc->args)) );
	case OP_CLOSUREP:        /* closure? */
		/*
		 * Note, macro object is also a closure.
		 * Therefore, (closure? <#MACRO>) ==> #t
		 */
		s_retbool(is_closure(car(sc->args)));
	case OP_MACROP:          /* macro? */
		s_retbool(is_macro(car(sc->args)));
	case OP_PAIRP:       /* pair? */
		s_retbool(is_pair(car(sc->args)));
	case OP_LISTP: {     /* list? */
		pointer slow, fast;
		slow = fast = car(sc->args);
		while (1) {
			if (!is_pair(fast)) s_retbool(fast == sc->NIL);
			fast = cdr(fast);
			if (!is_pair(fast)) s_retbool(fast == sc->NIL);
			fast = cdr(fast);
			slow = cdr(slow);
			if (fast == slow) {
				/* the fast pointer has looped back around and
				   caught up with the slow pointer, hence the
				   structure is circular, not of finite length,
				   and therefore not a list */
				s_retbool(0);
			}
		}
	}
	case OP_ENVP:        /* environment? */
		s_retbool(is_environment(car(sc->args)));
	case OP_VECTORP:     /* vector? */
		s_retbool(is_vector(car(sc->args)));

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_compare(scheme *sc, enum scheme_opcodes op)
{
	pointer x;
	num v;
	int (*comp_func)(num,num)=0;

	switch (op) {
	case OP_NUMEQ:      /* = */
	case OP_LESS:       /* < */
	case OP_GRE:        /* > */
	case OP_LEQ:        /* <= */
	case OP_GEQ:        /* >= */
		switch(op) {
		case OP_NUMEQ: comp_func=num_eq; break;
		case OP_LESS:  comp_func=num_lt; break;
		case OP_GRE:   comp_func=num_gt; break;
		case OP_LEQ:   comp_func=num_le; break;
		case OP_GEQ:   comp_func=num_ge; break;
		default:                         break;
		}
		x=sc->args;
		v=nvalue(car(x));
		x=cdr(x);

		for (; is_pair(x); x = cdr(x)) {
			if(!comp_func(v,nvalue(car(x)))) {
				s_retbool(0);
			}
			v=nvalue(car(x));
		}
		s_retbool(1);
	case OP_EQ:         /* eq? */
		s_retbool(car(sc->args) == cadr(sc->args));
	case OP_EQV:        /* eqv? */
		s_retbool(eqv(car(sc->args), cadr(sc->args)));

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_quit(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_QUIT:       /* quit */
		if(is_pair(sc->args)) {
			sc->retcode=long_value_unchecked(car(sc->args));
		}
		return(sc->NIL);

	case OP_EXIT:       /* exit */
		scheme_deinit(sc);
		return NULL;

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_gc(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_GC:         /* gc */
		gc(sc, sc->NIL, sc->NIL);
		s_return(sc,sc->T);

	case OP_GCVERB:          /* gc-verbose */
	{    int  was = sc->gc_verbose;

	sc->gc_verbose = (car(sc->args) != sc->F);
	s_retbool(was);
	}

	case OP_NEWSEGMENT: /* new-segment */
		if (!is_pair(sc->args) || !is_number(car(sc->args))) {
			Error_0(sc,"new-segment: argument must be a number");
		}
		alloc_cellseg(sc, (int) long_value_unchecked(car(sc->args)));
		s_return(sc,sc->T);

	case OP_OBLIST: /* oblist */
		s_return(sc, oblist_all_symbols(sc));

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_port(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_CURR_INPORT: /* current-input-port */
		s_return(sc,sc->inport);

	case OP_CURR_OUTPORT: /* current-output-port */
		s_return(sc,sc->outport);

	case OP_CLOSE_INPORT: /* close-input-port */
		port_close(sc,car(sc->args),port_input);
		s_return(sc,sc->T);

	case OP_CLOSE_OUTPORT: /* close-output-port */
		port_close(sc,car(sc->args),port_output);
		s_return(sc,sc->T);

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static pointer op_env(scheme *sc, enum scheme_opcodes op)
{
	switch (op) {
	case OP_INT_ENV: /* interaction-environment */
		s_return(sc,sc->global_env);

	case OP_CURR_ENV: /* current-environment */
		s_return(sc,sc->envir);

	default:
		sprintf(sc->strbuff, "%d: illegal operator", sc->op);
		Error_0(sc,sc->strbuff);
	}
	return sc->T;
}

static const char *procname(pointer x)
{
	int n=procnum(x);
	const char *name=dispatch_table[n].name;
	if(name==0) {
		name="ILLEGAL!";
	}
	return name;
}

/* kernel of this interpreter */
static void Eval_Cycle(scheme *sc, enum scheme_opcodes op)
{
	int count=0;
	int old_op;
	pointer retcode = sc->NIL;

	sc->op = op;
	for (;;) {
		op_code_info *pcd=dispatch_table+sc->op;
		if (pcd->name!=0) { /* if built-in function, check arguments */
			char msg[512];
			int ok=1;
			int n=list_length(sc,sc->args);

			/* Check number of arguments */
			if(n<pcd->min_arity) {
				ok=0;
				sprintf(msg,"%s: needs%s %d argument(s)",
					pcd->name,
					pcd->min_arity==pcd->max_arity
					? ""
					: " at least",
					pcd->min_arity);
			}
			if(ok && n>pcd->max_arity) {
				ok=0;
				sprintf(msg,"%s: needs%s %d argument(s)",
					pcd->name,
					pcd->min_arity==pcd->max_arity
					? ""
					: " at most",
					pcd->max_arity);
			}
			if(ok) {
				if(pcd->arg_tests_encoding!=0) {
					int i=0;
					int j;
					const char *t=pcd->arg_tests_encoding;
					pointer arglist=sc->args;
					do {
						pointer arg=car(arglist);
						j=(int)t[0];
						if(j==TST_INPORT[0]) {
							if(!is_inport(arg))
								break;
						} else if(j==TST_OUTPORT[0]) {
							if(!is_outport(arg))
								break;
						} else if(j==TST_LIST[0]) {
							if(arg!=sc->NIL
							   && !is_pair(arg))
								break;
						} else {
							if(!tests[j].fct(arg))
								break;
						}
						/* last test is replicated
						   as necessary */
						if(t[1]!=0) {
							t++;
						}
						arglist=cdr(arglist);
						i++;
					} while(i<n);
					if(i<n) {
						ok=0;
						sprintf(msg,
							"%s: argument %d must be: %s.  Arguments were: ",
							pcd->name,
							i+1,
							tests[j].kind);
					}
				}
			}
			if(!ok) {
				if(_Error_1(sc,msg,sc->args)==sc->NIL) {
					return;
				}
				pcd=dispatch_table+sc->op;
			}
		}
		old_op=sc->op;
		retcode = pcd->func(sc, sc->op);
		if( retcode == NULL || retcode == sc->NIL) {
			return;
		}
		if(sc->no_memory) {
			printk( KERN_ALERT "Schemix:  Out of memory!\n" );
			return;
		}
		count++;
	}
}

/* ========== Initialization of internal keywords ========== */

static void assign_syntax(scheme *sc, char *name)
{
	pointer x;

	x = oblist_add_by_name(sc, name);
	typeflag(x) |= T_SYNTAX;
}

static void assign_proc(scheme *sc, enum scheme_opcodes op, char *name)
{
	pointer x, y;

	x = mk_symbol(sc, name);
	y = mk_proc(sc,op);
	new_slot_in_env(sc, x, y);
}

static pointer mk_proc(scheme *sc, enum scheme_opcodes op)
{
	pointer y;

	y = get_cell(sc, sc->NIL, sc->NIL);
	typeflag(y) = (T_PROC | T_ATOM);
	long_value_unchecked(y) = (long) op;
	set_integer(y);
	return y;
}

/* Hard-coded for the given keywords. Remember to rewrite if more are added! */
static int syntaxnum(pointer p)
{
	const char *s=strvalue(car(p));
	switch(strlength(car(p))) {
	case 2:
		if(s[0]=='i') return OP_IF0;        /* if */
		else return OP_OR0;                 /* or */
	case 3:
		if(s[0]=='a') return OP_AND0;      /* and */
		else return OP_LET0;               /* let */
	case 4:
		switch(s[3]) {
		case 'e': return OP_CASE0;         /* case */
		case 'd': return OP_COND0;         /* cond */
		case '*': return OP_LET0AST;       /* let* */
		default: return OP_SET0;           /* set! */
		}
	case 5:
		switch(s[2]) {
		case 'g': return OP_BEGIN;         /* begin */
		case 'l': return OP_DELAY;         /* delay */
		case 'c': return OP_MACRO0;        /* macro */
		default: return OP_QUOTE;          /* quote */
		}
	case 6:
		switch(s[2]) {
		case 'm': return OP_LAMBDA;        /* lambda */
		case 'f': return OP_DEF0;          /* define */
		default: return OP_LET0REC;        /* letrec */
		}
#if SCHEMIX_USE_DEVFS
	case 11:
		return OP_MAKE_DEVICE;             /* make-device */
#endif
	default:
		return 0;
	}
}

static int scheme_init(scheme *sc)
{
	int i, n=sizeof(dispatch_table)/sizeof(dispatch_table[0]);
	pointer x;

	num_zero.num_type=num_long;
	num_zero.value.long_value=0;
	num_one.num_type=num_long;
	num_one.value.long_value=1;

	sc->gensym_cnt=0;
	sc->last_cell_seg = -1;
	sc->sink = &sc->_sink;
	sc->NIL = &sc->_NIL;
	sc->T = &sc->_HASHT;
	sc->F = &sc->_HASHF;
	sc->EOF_OBJ=&sc->_EOF_OBJ;
	sc->free_cell = &sc->_NIL;
	sc->fcells = 0;
	sc->no_memory=0;
	sc->inport=sc->NIL;
	sc->outport=sc->NIL;

	if (alloc_cellseg(sc,FIRST_CELLSEGS) != FIRST_CELLSEGS) {
		sc->no_memory=1;
		return 0;
	}

	sc->gc_verbose = 0;
	dump_stack_initialize(sc);
	sc->code = sc->NIL;

	/* init sc->NIL */
	typeflag(sc->NIL) = (T_ATOM | MARK);
	car(sc->NIL) = cdr(sc->NIL) = sc->NIL;
	/* init T */
	typeflag(sc->T) = (T_ATOM | MARK);
	car(sc->T) = cdr(sc->T) = sc->T;
	/* init F */
	typeflag(sc->F) = (T_ATOM | MARK);
	car(sc->F) = cdr(sc->F) = sc->F;

	sc->oblist = oblist_initial_value(sc);
	sc->mem_ref_list = sc->NIL;

	/* init global_env */
	new_frame_in_env(sc, sc->NIL);
	sc->global_env = sc->envir;

	/* init else */
	x = mk_symbol(sc,"else");
	new_slot_in_env(sc, x, sc->T);

	assign_syntax(sc, "lambda");
	assign_syntax(sc, "make-device");
	assign_syntax(sc, "quote");
	assign_syntax(sc, "define");
	assign_syntax(sc, "if");
	assign_syntax(sc, "begin");
	assign_syntax(sc, "set!");
	assign_syntax(sc, "let");
	assign_syntax(sc, "let*");
	assign_syntax(sc, "letrec");
	assign_syntax(sc, "cond");
	assign_syntax(sc, "delay");
	assign_syntax(sc, "and");
	assign_syntax(sc, "or");
	assign_syntax(sc, "cons-stream");
	assign_syntax(sc, "macro");
	assign_syntax(sc, "case");

	for(i=0; i<n; i++) {
		if(dispatch_table[i].name!=0) {
			assign_proc(sc, i, dispatch_table[i].name);
		}
	}

	/* Initialization of global pointers to special symbols */
	sc->LAMBDA = mk_symbol(sc, "lambda");
	sc->MAKE_DEVICE = mk_symbol(sc, "make-device");
	sc->ON_READ = mk_symbol(sc, "*on-read*");
	sc->ON_WRITE = mk_symbol(sc, "*on-write*");
	sc->ON_SEEK = mk_symbol(sc, "*on-seek*");
	sc->QUOTE = mk_symbol(sc, "quote");
	sc->QQUOTE = mk_symbol(sc, "quasiquote");
	sc->UNQUOTE = mk_symbol(sc, "unquote");
	sc->UNQUOTESP = mk_symbol(sc, "unquote-splicing");
	sc->FEED_TO = mk_symbol(sc, "=>");
	sc->SHARP_HOOK = mk_symbol(sc, "*sharp-hook*");

	sc->Device_Open = 0;
	sc->DEBUG = 0;
	sc->ZERO = mk_number(sc,num_zero);

	sc->output_buffer_pos = sc->output_buffer_start;
	sc->output_buffer_end = sc->output_buffer_start;
	sc->loadport = mk_port( sc, &sc->lport );

/* include the parts of the interpreter that are written in scheme
   and have been "compiled" to C using schemix-make-init */
	init_extras( sc );
	return !sc->no_memory;
}

static void init_extras(scheme *sc)
{
#include "schemix-pre-init.c"
#include "schemix-init.c"
}

static void scheme_deinit(scheme *sc)
{
	int i;
	scheme *lst;

	if( root_sc == sc )
		root_sc = NULL;

   if( list_of_schemes == sc )
      list_of_schemes = list_of_schemes->next;
	lst = list_of_schemes;
	while( lst ) {
		if( lst->next == sc )
			lst->next = lst->next->next;
		lst = lst->next;
	}

#if SCHEMIX_USE_DEVFS
	devfs_unregister( sc->devfs_handle );
#else
	unregister_chrdev( sc->major, "schemix" );
#endif

	sc->oblist=sc->NIL;
	sc->mem_ref_list=sc->NIL;
	sc->global_env=sc->NIL;
	dump_stack_free(sc);
	sc->envir=sc->NIL;
	sc->code=sc->NIL;
	sc->args=sc->NIL;
	sc->value=sc->NIL;
	if(is_port(sc->inport)) {
		typeflag(sc->inport) = T_ATOM;
	}
	sc->inport=sc->NIL;
	sc->outport=sc->NIL;
	if(is_port(sc->loadport)) {
		typeflag(sc->loadport) = T_ATOM;
	}
	sc->loadport=sc->NIL;
	sc->gc_verbose=0;
	gc(sc,sc->NIL,sc->NIL);

	for(i=0; i<=sc->last_cell_seg; i++) {
		free( sc, sc->alloc_seg[i] );
	}
}

/* This reads, evaluates and prints *one* expression
   and then returns the number of chars it has consumed. */
static int scheme_load_string(scheme *sc, char *cmd)
{
	if( sc == NULL || sc->DEBUG )
      printk(KERN_ALERT "Schemix:  Evaluating string: %s\n", cmd);
	dump_stack_reset(sc);
   sc->envir = sc->global_env;
   sc->retcode=0;
	sc->lport.kind=port_input|port_string;
	sc->lport.rep.string.start = cmd;
	sc->lport.rep.string.past_the_end = cmd + strlen(cmd);
	sc->lport.rep.string.curr = cmd;
	sc->retcode=0;
	sc->inport=sc->loadport;
	Eval_Cycle(sc, OP_REP); /* Read, Eval, Print (but don't loop!) */
	typeflag(sc->loadport)=T_ATOM;
	if( (sc == NULL || sc->DEBUG)
         && (sc->lport.rep.string.curr != sc->lport.rep.string.past_the_end) )
      printk(KERN_ALERT "Schemix:  Did not read entire string...\n");
   return (sc->lport.rep.string.curr - cmd);
}

static void define(scheme *sc, pointer envir, pointer symbol, pointer value)
{
     pointer x = find_slot_in_env(sc, envir, symbol, 0);
     pointer ev_value = eval( sc, value );
     if (x != sc->NIL) {
          set_slot_in_env(sc, x, ev_value);
     } else {
          new_slot_spec_in_env(sc, envir, symbol, ev_value);
     }
}

static pointer apply(scheme *sc, pointer func, pointer args)
{
	dump_stack_reset(sc);
	sc->envir = sc->global_env;
	sc->args = args;
	sc->code = func;
	sc->retcode = 0;
	Eval_Cycle(sc, OP_APPLY);
	return sc->value;
}

static pointer eval(scheme *sc, pointer x)
{
	dump_stack_reset(sc);
	sc->envir = sc->global_env;
	sc->args = sc->NIL;
	sc->code = x;
	sc->retcode = 0;
	Eval_Cycle(sc, OP_EVAL);
	return sc->value;
}

/* Linux file operations */
static struct file_operations file_ops;

/* bindings is a list like ((x a) (y b) (z c)). */
static scheme* make_device(scheme *sc, char *name, pointer bindings)
{
	scheme *new_scheme;

	new_scheme = (scheme*)malloc( sc, sizeof(scheme) );
	if(!scheme_init(new_scheme))
	{
		printk(KERN_ALERT "Could not initialize scheme interpreter\n");
		return NULL;
	}
	else
	{
		new_scheme->next = list_of_schemes;
		list_of_schemes = new_scheme;
	}

	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "Schemix registering device '%s'...\n",
		       name);

#if SCHEMIX_USE_DEVFS
	new_scheme->devfs_handle = devfs_register( NULL,
						   name,
						   DEVFS_FL_AUTO_DEVNUM, 0, 0,
						   S_IFCHR | S_IRUSR | S_IWUSR
						   | S_IRGRP | S_IWGRP,
						   &file_ops,
						   (void*)new_scheme );
#else
	new_scheme->major = register_chrdev(0, "schemix", &file_ops);
	if( new_scheme->major < 0 )
		printk(KERN_ALERT "Schemix: can't register device!\n");
	else
		printk(KERN_ALERT "Schemix device has major number %d\n",
		       new_scheme->major);
#endif

	/* Establish bindings */
	if(bindings) {
		if( sc == NULL || sc->DEBUG )
			printk(KERN_ALERT "Establishing Scheme bindings\n");
		while( is_pair(bindings) ) {
			if( sc == NULL || sc->DEBUG )
				printk(KERN_ALERT "Binding symbol '%s'\n",
				       symname(caar(bindings)));
			new_slot_in_env(new_scheme,
					mk_symbol(new_scheme,
						  symname(caar(bindings))),
					eval(new_scheme,
					     copy(new_scheme,
						  sc,
						  cadar(bindings))));
			bindings = cdr(bindings);
		}
	}

	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "established bindings for new Scheme\n");

	return new_scheme;
}

static int schemix_init(void)
{
#if DEBUG_MEMORY
	struct cell debugcell;
#endif
	printk(KERN_ALERT "Schemix driver loading...\n");
	number_of_mallocs=0;
	if( !(root_sc = make_device(NULL,"schemix",NULL)) )
	{
		printk(KERN_ALERT "Could not initialize scheme interpreter\n");
		return -1;
	}
	printk(KERN_ALERT "Schemix driver loaded.\n");

	/* Linux device file ops stuff */
	file_ops.open = device_open;
	file_ops.release = device_release;
	file_ops.read = device_read;
	file_ops.write = device_write;

	/* FIXME: Yuck! */
	scheme_load_string( root_sc,
			    "(display \"This is Schemix, an in-kernel Scheme system.  Copyright 2003 by William Bland.\\n\")\n" );

#if DEBUG_MEMORY
	printk(KERN_ALERT "sizeof(cell) = %d, sizeof(scheme) = %d\n",
	       sizeof(struct cell), sizeof(scheme));
	printk(KERN_ALERT "sizeof(cell.string) = %d, sizeof(cell.num) = %d\n",
	       sizeof(debugcell._object._string),
	       sizeof(debugcell._object._number));
	printk(KERN_ALERT "sizeof(cell.scheme) = %d, sizeof(port) = %d\n",
	       sizeof(debugcell._object._scheme),
	       sizeof(debugcell._object._port));
	printk(KERN_ALERT "sizeof(cell.cons) = %d\n",
	       sizeof(debugcell._object._cons));
#endif

	return 0;
}

static void schemix_exit(void)
{
	scheme *sc;

	while( list_of_schemes )
	{
		sc = list_of_schemes;
		list_of_schemes = list_of_schemes->next;
		scheme_deinit(sc);
	}
	printk(KERN_ALERT "Schemix driver unloaded\n");
}

static int device_open( struct inode *inode,
                        struct file *file )
{
#if SCHEMIX_USE_DEVFS
	scheme *sc = (scheme*)file->private_data;
#else
	scheme *sc = root_sc;
#endif
	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "Schemix device open\n");
	if( sc->Device_Open )
		return -EBUSY;
	sc->Device_Open = 1;
   dump_stack_reset(sc);
   sc->envir = sc->global_env;
   sc->retcode=0;

	return 0;
}

static int device_release( struct inode *inode,
                           struct file *file )
{
#if SCHEMIX_USE_DEVFS
	scheme *sc = (scheme*)file->private_data;
#else
	scheme *sc = root_sc;
#endif
	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "Schemix device closing\n");
   if( sc ) { /* Make sure the device does actually still exist! */
      sc->Device_Open = 0;
   }
	return 0;
}

static ssize_t device_read( struct file *file,
                            char *buffer,
                            size_t length,
                            loff_t *offset )
{
#if SCHEMIX_USE_DEVFS
	scheme *sc = (scheme*)file->private_data;
#else
	scheme *sc = root_sc;
#endif
	pointer hook = find_slot_in_env(sc,sc->global_env,sc->ON_READ,1);

	if( sc == NULL || sc->DEBUG )
		printk(KERN_ALERT "User reading from Schemix device\n");
	/* Evaluate the user hook (if one is defined) */
	if( hook != sc->NIL )
	{
		hook = slot_value_in_env(sc, hook);
		/* maybe we should pass to the hook the string
		   that's about to be printed? */
		if( apply( sc, hook, sc->NIL ) == sc->F )
			return 0; /* hook returned #f */
	}

	/* write as much as we can into buffer.
	   if we get up to output_device_buffer_end then
	   we reset */
	unsigned int n;
	for( n=0;
	     n<length && sc->output_buffer_pos < sc->output_buffer_end;
	     n++ )
		put_user( *(sc->output_buffer_pos++), buffer++ );
	if( sc->output_buffer_pos == sc->output_buffer_end ) {
		sc->output_buffer_pos = sc->output_buffer_start;
		sc->output_buffer_end = sc->output_buffer_start;
   }
	return n;
}

static ssize_t device_write( struct file *file,
                             const char *buffer,
                             size_t length,
                             loff_t *offset )
{
#if SCHEMIX_USE_DEVFS
	scheme *sc = (scheme*)file->private_data;
#else
	scheme *sc = root_sc;
#endif
	pointer hook = find_slot_in_env(sc,sc->global_env,sc->ON_WRITE,1);
	unsigned int n;
	char *i = sc->input_buffer;
	char c;

	/* Evaluate the user hook (if one is defined).
	   Otherwise, read and eval. */

	for( n=0; n<length && n<INPUT_BUFFER_LENGTH; n++ )
	{
		get_user( c, buffer++ );
		*(i++) = c;
	}
	*i = '\0';

	if( hook != sc->NIL )
	{
		hook = slot_value_in_env(sc, hook);
		apply( sc, hook, mk_string( sc, sc->input_buffer ) );
      return n;
	}
	else
		return scheme_load_string( sc, sc->input_buffer );
}

module_init(schemix_init);
module_exit(schemix_exit);
MODULE_LICENSE("GPL");
