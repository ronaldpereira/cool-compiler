#include "stringtab.h"

#define FRAME_OFFSET 3

#define MAXINT 100000000
#define WORD_SIZE 4
#define LOG_WORD_SIZE 2

#define CLASSNAMETAB "class_nameTab"
#define CLASSOBJTAB "class_objTab"
#define INTTAG "_int_tag"
#define BOOLTAG "_bool_tag"
#define STRINGTAG "_string_tag"
#define HEAP_START "heap_start"

#define DISPTAB_SUFFIX "_dispTab"
#define METHOD_SEP "."
#define CLASSINIT_SUFFIX "_init"
#define PROTOBJ_SUFFIX "_protObj"
#define OBJECTPROTOBJ "Object" PROTOBJ_SUFFIX
#define INTCONST_PREFIX "int_const"
#define STRCONST_PREFIX "str_const"
#define BOOLCONST_PREFIX "bool_const"

#define EMPTYSLOT 0
#define LABEL ":\n"

#define STRINGNAME (char *)"String"
#define INTNAME (char *)"Int"
#define BOOLNAME (char *)"Bool"
#define MAINNAME (char *)"Main"

#define DEFAULT_OBJFIELDS 3
#define TAG_OFFSET 0
#define SIZE_OFFSET 1
#define DISPTABLE_OFFSET 2

#define STRING_SLOTS 1
#define INT_SLOTS 1
#define BOOL_SLOTS 1

#define GLOBAL "\t.globl\t"
#define ALIGN "\t.align\t2\n"
#define WORD "\t.word\t"

#define ZERO "$zero"
#define ACC "$a0"
#define A1 "$a1"
#define SELF "$s0"
#define T1 "$t1"
#define T2 "$t2"
#define T3 "$t3"
#define SP "$sp"
#define FP "$fp"
#define RA "$ra"

#define JALR "\tjalr\t"
#define JAL "\tjal\t"
#define RET "\tjr\t" RA "\t"

#define SW "\tsw\t"
#define LW "\tlw\t"
#define LI "\tli\t"
#define LA "\tla\t"

#define MOVE "\tmove\t"
#define NEG "\tneg\t"
#define ADD "\tadd\t"
#define ADDI "\taddi\t"
#define ADDU "\taddu\t"
#define ADDIU "\taddiu\t"
#define DIV "\tdiv\t"
#define MUL "\tmul\t"
#define SUB "\tsub\t"
#define SLL "\tsll\t"
#define BEQZ "\tbeqz\t"
#define BRANCH "\tb\t"
#define BEQ "\tbeq\t"
#define BNE "\tbne\t"
#define BLEQ "\tble\t"
#define BLT "\tblt\t"
#define BGT "\tbgt\t"
