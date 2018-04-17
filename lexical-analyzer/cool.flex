/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */

%option noyywrap


%{
#include "cool-parse.h"
#include "stringtab.h"
#include "utilities.h"

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;
unsigned int comment = 0;
unsigned int string_buf_left;
bool string_error;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

int string_error_check(char *str) {
    unsigned int len = strlen(str);
    if (len < string_buf_left) {
        strncpy(string_buf_ptr, str, len);
        string_buf_ptr += len;
        string_buf_left -= len;
        return 0;
    } else {
        string_error = true;
        yylval.error_msg = "String constant too long";
        return -1;
    }
}

char *getNextyytext() {
  char *c = &yytext[1];
  if (*c == '\n') {
    curr_lineno++;
  }
  return c;
}

%}

NEWLINE         [\n]
NOTNEWLINE      [^\n]
NOTSTRING       [^\n\0\\\"]
WHITESPACE      [ \t\r\f\v]+
NULLCH          [\0]
BACKSLASH       [\\]
LINE_COMMENT    "--"
START_COMMENT   "(*"
END_COMMENT     "*)"
QUOTES          \"

%x COMMENT
%x STRING

%%

<INITIAL,COMMENT>{NEWLINE} {
    curr_lineno++;
}

{START_COMMENT} {
    comment++;
    BEGIN(COMMENT);
}

<COMMENT><<EOF>> {
    yylval.error_msg = "EOF in comment";
    BEGIN(INITIAL);
    return (ERROR);
}

<COMMENT>[*]/[^)] ;

<COMMENT>[(]/[^*] ;

<COMMENT>[^\n*(\\]* ;

<COMMENT>{BACKSLASH}(.|{NEWLINE}) {
    getNextyytext();
};

<COMMENT>{BACKSLASH} ;

<COMMENT>{START_COMMENT} {
    comment++;
}

<COMMENT>{END_COMMENT} {
    comment--;
    if (comment == 0) {
        BEGIN(INITIAL);
    }
}

<INITIAL>{END_COMMENT} {
    yylval.error_msg = "Unmatched *)";
    return (ERROR);
}

<INITIAL>{LINE_COMMENT}{NOTNEWLINE}* ;

<INITIAL>{QUOTES} {
    BEGIN(STRING);
    string_buf_ptr = string_buf;
    string_buf_left = MAX_STR_CONST;
    string_error = false;
}

<STRING><<EOF>> {
    yylval.error_msg = "EOF in string constant";
    BEGIN(INITIAL);
    return ERROR;
}

<STRING>{NOTSTRING}* {
    int rc = string_error_check(yytext);
    if (rc != 0) {
        return (ERROR);
    }
}

<STRING>{NULLCH} {
    yylval.error_msg = "String contains null character";
    string_error = true;
    return (ERROR);
}

<STRING>{BACKSLASH}{NULLCH} {
    yylval.error_msg = "String contains escaped null character";
    string_error = true;
    return (ERROR);
}

<STRING>{NEWLINE} {
    BEGIN(INITIAL);
    curr_lineno++;
    if (!string_error) {
        string_error = true;
        yylval.error_msg = "Unterminated string constant";
        return (ERROR);
    }
}

<STRING>{BACKSLASH}{1,2}(.|{NEWLINE}) {
    char *c = getNextyytext();
    int errorFlagC;

    switch (*c) {
        case 'n':
            errorFlagC = string_error_check("\n");
        break;

        case 'b':
            errorFlagC = string_error_check("\b");
        break;

        case 't':
            errorFlagC = string_error_check("\t");
        break;

        case 'f':
            errorFlagC = string_error_check("\f");
        break;

        case '0':
            yylval.error_msg = "String contains null character";
            string_error = true;
            errorFlagC = -1;
        break;

        case '\\':
            if (yytext[2] == '0') {
                yylval.error_msg = "String contains escaped null character";
                string_error = true;
                errorFlagC = -1;
            }
        break;

        default:
            errorFlagC = string_error_check(c);
    }
    if (errorFlagC != 0) {
        return (ERROR);
    }
}

<STRING>{BACKSLASH} ;

<STRING>{QUOTES} {
  BEGIN(INITIAL);
  if (!string_error) {
    yylval.symbol = stringtable.add_string(string_buf, string_buf_ptr - string_buf);
    return (STR_CONST);
  }
}

{WHITESPACE} ;

[0-9]+                            { yylval.symbol = stringtable.add_string(yytext); return (INT_CONST); }
[cC][lL][aA][sS][sS]              { return (CLASS); }
[eE][lL][sS][eE]                  { return (ELSE); }
[fF][iI]                          { return (FI); }
[iI][fF]                          { return (IF); }
[iI][nN]                          { return (IN); }
[iI][nN][hH][eE][rR][iI][tT][sS]  { return (INHERITS); }
[iI][sS][vV][oO][iI][dD]          { return (ISVOID); }
[lL][eE][tT]                      { return (LET); }
[lL][oO][oO][pP]                  { return (LOOP); }
[pP][oO][oO][lL]                  { return (POOL); }
[tT][hH][eE][nN]                  { return (THEN); }
[wW][hH][iI][lL][eE]              { return (WHILE); }
[cC][aA][sS][eE]                  { return (CASE); }
[eE][sS][aA][cC]                  { return (ESAC); }
[nN][eE][wW]                      { return (NEW); }
[oO][fF]                          { return (OF); }
[nN][oO][tT]                      { return (NOT); }
t[rR][uU][eE]                     { yylval.boolean = true; return (BOOL_CONST); }
f[aA][lL][sS][eE]                 { yylval.boolean = false; return (BOOL_CONST); }
[a-z][_a-zA-Z0-9]*                { yylval.symbol = stringtable.add_string(yytext); return (OBJECTID); }
[A-Z][_a-zA-Z0-9]*                { yylval.symbol = stringtable.add_string(yytext); return (TYPEID); }
"<="                              { return (LE); }
"=>"                              { return (DARROW); }
"<-"                              { return (ASSIGN); }
";"                               { return int(';'); }
","                               { return int(','); }
":"                               { return int(':'); }
"{"                               { return int('{'); }
"}"                               { return int('}'); }
"+"                               { return int('+'); }
"-"                               { return int('-'); }
"*"                               { return int('*'); }
"/"                               { return int('/'); }
"<"                               { return int('<'); }
"="                               { return int('='); }
"~"                               { return int('~'); }
"."                               { return int('.'); }
"@"                               { return int('@'); }
"("                               { return int('('); }
")"                               { return int(')'); }
<INITIAL>.                        { yylval.error_msg = yytext; return (ERROR); }

%%