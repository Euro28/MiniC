#pragma once
#include "token.h"

//to my understanding there is no generic enum to string command in c++.
const char* type_to_string(int type) {
  switch(type) {
    case INT_TOK: return "int";
    case VOID_TOK: return "void";
    case FLOAT_TOK: return "float";
    case BOOL_TOK: return "bool";
    case AND: return "&&";
    case OR: return "||";
    case PLUS: return "+";
    case MINUS: return "-";
    case ASTERIX: return "*";
    case DIV: return "/";
    case MOD: return "%";
    case NOT: return "!";
    case EQ: return "==";
    case NE: return "!=";
    case LE: return "<=";
    case LT: return "<";
    case GE: return ">=";
    case GT: return ">";
  }
}