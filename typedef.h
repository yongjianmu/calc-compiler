#ifndef __TYPEDEF_H__
#define __TYPEDEF_H__

#define str_add         "+"
#define str_sub         "-"
#define str_mul         "*"
#define str_div         "/"
#define str_mod         "%"
#define str_gt          ">"
#define str_ge          ">="
#define str_lt          "<"
#define str_le          "<="
#define str_eq          "=="
#define str_neq         "!="

enum Token{
    TOK_BOOL = 0,
    TOK_NUMBER,
    TOK_VAR,
    TOK_BR_IF,
    TOK_BR_WHILE,
    TOK_SET,
    TOK_SEQ,
    TOK_OP,
    TOK_LBRA,
    TOK_RBRA,
    TOK_EOF,
    TOK_UNDEFINED,
    TOKEN_MAX = 255
};

enum BinaryOP{
    op_add = 0,
    op_sub,
    op_mul,
    op_div,
    op_mod,
    op_gt,
    op_ge,
    op_lt,
    op_le,
    op_eq,
    op_ne,
    op_none,
    op_max = 255
};


#endif /*__TYPEDEF_H__*/
