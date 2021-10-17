#ifndef _TOKENS 
#define _TOKENS

#include <boost/spirit/include/lex_lexertl.hpp>
#include <string>

namespace lex = boost::spirit::lex;


template<typename Lexer>
struct Tokens :  lex::lexer<Lexer> {
    Tokens() {
        INT = "Int";
        BOOL = "Bool";
        STRING = "String";

        FUNC = "Func";
        VAR = "Var";
        IF = "If";
        WHILE = "While";

        LP = "\\(";
        RP = "\\)";
        LB = "\\{";
        RB = "\\}";

        OR = "\\|\\|";
        AND = "\\&\\&";
        EQ = "==";
        NE = "\\/=";
        LE = "<=";
        LESS = "<";
        GE = ">=";
        GREATER = ">";
        PLUS = "\\+";
        MINUS = "\\-";
        MULT = "\\*";
        DIV = "\\/";
        POW = "\\^";
        ASSIGN = "=";

        NOT = "\\!";

        INT_LITERAL = "([0-9]+)|(B[0-1]+)";
        BOOL_LITERAL = "True|False";
        STRING_LITERAL = "\\\"[^\\n\\\"]*\\\"";
        ID = "[a-z][0-9A-Za-z]*";


        this->self 
        = INT | BOOL | STRING
        | FUNC | VAR | IF | WHILE
        | LP | RP | LB | RB 
        | OR | AND | EQ | NE | LESS | LE | GREATER | GE | PLUS | MINUS | MULT | DIV | POW | ASSIGN
        | NOT 
        | INT_LITERAL | BOOL_LITERAL | STRING_LITERAL 
        | ID
        ;
    }

    // Types
    lex::token_def<lex::omit>   INT,
                                BOOL,
                                STRING;
    // Keywords
    lex::token_def<lex::omit>   FUNC,
                                VAR,
                                IF,
                                WHILE;
    // Brackets
    lex::token_def<lex::omit>   LP,
                                RP,
                                LB,
                                RB;

    // Binary operators
    lex::token_def<lex::omit>   OR,
                                AND,
                                EQ,
                                NE,
                                LE,
                                LESS,
                                GE,
                                GREATER,
                                PLUS,
                                MINUS,
                                MULT,
                                DIV,
                                POW,
                                ASSIGN;
    
    // Unary operators
    lex::token_def<lex::omit> NOT;

    // Literals
    lex::token_def<std::string> INT_LITERAL,
                                STRING_LITERAL,
                                BOOL_LITERAL;

    lex::token_def<std::string> ID;

};

#endif