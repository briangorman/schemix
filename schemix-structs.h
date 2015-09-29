/* schemix-structs.h                                                     */
/* Information that can be used by Schemix to deduce structure layout    */
/* In the future, this file should be auto-generated from kernel source. */

struct structs_table_entry
{
	char *name;
	void *location;    /* For working out offsets within structs */
	unsigned long size;
	unsigned int type; /* An index in structures_table */
};

#define STRUCT_TYPE_UNKNOWN 0
#define STRUCT_TYPE_CHAR 1
#define STRUCT_TYPE_SHORT 2
#define STRUCT_TYPE_INT 3
#define STRUCT_TYPE_LONG 4
#define STRUCT_TYPE_FLOAT 5
#define STRUCT_TYPE_DOUBLE 6
#define STRUCT_TYPE_LONG_DOUBLE 7
#define STRUCT_TYPE_UNSIGNED_CHAR 8
#define STRUCT_TYPE_UNSIGNED_SHORT 9
#define STRUCT_TYPE_UNSIGNED_INT 10
#define STRUCT_TYPE_UNSIGNED_LONG 11
#define STRUCT_TYPE_BYTE 12

static struct structs_table_entry structures_table[] = {
	{"unknown type", 0, 0, STRUCT_TYPE_UNKNOWN},
	{"char", 0, sizeof(char), STRUCT_TYPE_CHAR},
	{"short", 0, sizeof(int), STRUCT_TYPE_SHORT},
	{"int", 0, sizeof(int), STRUCT_TYPE_INT},
	{"long", 0, sizeof(long), STRUCT_TYPE_LONG},
	{"float", 0, sizeof(float), STRUCT_TYPE_FLOAT},
	{"double", 0, sizeof(double), STRUCT_TYPE_DOUBLE},
	{"long-double", 0, sizeof(long double), STRUCT_TYPE_LONG_DOUBLE},
	{"unsigned-char", 0, sizeof(unsigned char), STRUCT_TYPE_UNSIGNED_CHAR},
	{"unsigned-short", 0, sizeof(unsigned char),
	 STRUCT_TYPE_UNSIGNED_SHORT},
	{"unsigned-int", 0, sizeof(unsigned int), STRUCT_TYPE_UNSIGNED_INT},
	{"unsigned-long", 0, sizeof(unsigned long), STRUCT_TYPE_UNSIGNED_LONG},
	{"byte", 0, sizeof(char), STRUCT_TYPE_BYTE}
};

#define structures_table_length (sizeof(structures_table)/sizeof(struct structs_table_entry))
