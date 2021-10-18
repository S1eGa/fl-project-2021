#include <iostream>
#include <string>
#include <vector>

#include "boost/spirit/include/qi.hpp"
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/variant.hpp>

using namespace boost::spirit;
namespace phx   = boost::phoenix;

enum BinaryOperatorID {
    OR,
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
    POW
}; 

enum UnaryOperatorID { 
    NOT, 
    UNARY_MINUS
};

enum Types {
    INT,
    BOOL,
    STRING
};

typedef boost::variant<
    int,
    std::string
> Literal;

struct ID {
    Types type;
    std::string name;
};
BOOST_FUSION_ADAPT_STRUCT(ID, (Types, type),(std::string, name))

struct FunctionCall;
struct BinaryOperator;
struct UnaryOperator;


typedef boost::variant<
boost::recursive_wrapper<UnaryOperator>,
boost::recursive_wrapper<BinaryOperator>,
    ID,
    Literal,
    FunctionCall
> Expression;

struct ExpressionList {
    std::vector<Expression> args;
};
BOOST_FUSION_ADAPT_STRUCT(ExpressionList, (std::vector<Expression>, args))

struct FunctionCall {
    ID name;
    ExpressionList args;
};
BOOST_FUSION_ADAPT_STRUCT(FunctionCall, (ID, name), (ExpressionList, args))

struct UnaryOperator {
    UnaryOperatorID type;
    Expression right;

    UnaryOperator() = default;
    UnaryOperator(UnaryOperatorID type, Expression right) : type(type), right(right) {}
};
BOOST_FUSION_ADAPT_STRUCT(UnaryOperator, (UnaryOperatorID, type), (Expression, right))

struct BinaryOperator {
    BinaryOperatorID type;
    Expression left;
    Expression right;

    BinaryOperator() = default;
    BinaryOperator(Expression left, BinaryOperatorID type , Expression right) : type(type), left(left), right(right) {} 
};
BOOST_FUSION_ADAPT_STRUCT(BinaryOperator, (UnaryOperatorID, type), (Expression, left), (Expression, right))

struct DeclStatement;
struct AssignStatement;
struct SingleIfStatement;
struct IfElseStatement;
struct WhileStatement;

typedef boost::variant<
    boost::recursive_wrapper<SingleIfStatement>,
    boost::recursive_wrapper<IfElseStatement>
> IfStatement;

struct ReturnStatement {
    Expression return_value;
};
BOOST_FUSION_ADAPT_STRUCT(ReturnStatement, (Expression, return_value))

typedef boost::variant<
    Expression,
    AssignStatement,
    DeclStatement,
    boost::recursive_wrapper<IfStatement>,
    boost::recursive_wrapper<WhileStatement>,
    ReturnStatement
> Statement;

struct AssignStatement {
    ID variable;
    Expression right;
};
BOOST_FUSION_ADAPT_STRUCT(AssignStatement, (ID, variable), (Expression, right))

struct DeclStatement {
    ID variable;
    Expression right;
};
BOOST_FUSION_ADAPT_STRUCT(DeclStatement, (ID, variable), (Expression, right))

struct SingleIfStatement {
    Expression cond;
    Statement body;
};
BOOST_FUSION_ADAPT_STRUCT(SingleIfStatement, (Expression, cond), (Statement, body))

struct IfElseStatement {
    Expression cond;
    Statement body;
    Statement else_body;
};
BOOST_FUSION_ADAPT_STRUCT(IfElseStatement, (Expression, cond), (Statement, body), (Statement, else_body))


struct WhileStatement {
    Expression cond;
    Statement body;
};
BOOST_FUSION_ADAPT_STRUCT(WhileStatement, (Expression, cond), (Statement, body))

struct TypeParametersList {
    std::vector<ID> args;
};
BOOST_FUSION_ADAPT_STRUCT(TypeParametersList, (std::vector<ID>, args))

struct FuncDeclaration {
    ID name;
    TypeParametersList args;
    Statement body;
};
BOOST_FUSION_ADAPT_STRUCT(FuncDeclaration, (ID, name), (TypeParametersList, args), (Statement, body))

struct Language {
    std::vector<Expression> sequence;
};
BOOST_FUSION_ADAPT_STRUCT(Language, (std::vector<Expression>, sequence))


template<typename Iterator, typename Skipper>
struct Grammar: qi::grammar<Iterator, Skipper, Language()> {

    Grammar() : Grammar::base_type(start) {
        OR_OP.add
            ("||", BinaryOperatorID::OR);
        AND_OP.add
            ("&&", BinaryOperatorID::AND);
        NOT_OP.add
            ("!", UnaryOperatorID::NOT);
        COMPARE_OP.add
            ("==", BinaryOperatorID::EQ)
            ("/=", BinaryOperatorID::NE)
            (">=", BinaryOperatorID::GE)
            (">", BinaryOperatorID::GREATER)
            ("<=", BinaryOperatorID::LE)
            ("<", BinaryOperatorID::LESS);
        ADD_OP.add
            ("+", BinaryOperatorID::PLUS)
            ("-", BinaryOperatorID::PLUS);
        MULT_OP.add
            ("*", BinaryOperatorID::MULT)
            ("/", BinaryOperatorID::DIV);
        UNARY_MINUS_OP.add
            ("-", UnaryOperatorID::UNARY_MINUS);
        POW_OP.add
            ("^", BinaryOperatorID::POW);

        OR_level = AND_level [ _val = _1 ] >> -(OR_OP >> OR_level) [ _val = phx::construct<BinaryOperator>(_val, _1, _2) ];
        AND_level = NOT_level [ _val = _1 ] >> -(AND_OP >> AND_level) [ _val = phx::construct<BinaryOperator>(_val, _1, _2) ];
        
        NOT_level = COMPARE_level [ _val = _1 ] | (NOT_OP >> COMPARE_level) [ _val = phx::construct<UnaryOperator>(_1, _2) ];
        
        COMPARE_level = ADD_level [ _val = _1 ] >> -(COMPARE_OP >> ADD_level) [ _val = phx::construct<BinaryOperator>(_val, _1, _2) ];
        
        // Not left-associative!
        ADD_level = MULT_level [ _val = _1 ] >> -(ADD_OP >> MULT_level) [ _val = phx::construct<BinaryOperator>(_val, _1, _2) ];
        MULT_level = UNARY_MINUS_level [ _val = _1 ] >> -(MULT_OP >> UNARY_MINUS_level) [ _val = phx::construct<BinaryOperator>(_val, _1, _2) ];
        //
        
        UNARY_MINUS_level = POW_level [_val = _1] | (UNARY_MINUS_OP >> POW_level) [ _val = phx::construct<UnaryOperator>(_1, _2) ];
        
        POW_level = value [_val = _1] >> -(POW_OP >> value) [ _val = phx::construct<BinaryOperator>(_val, _1, _2) ];


        literal = qi::int_ | string_literal | bool_literal;
        string_literal = qi::lexeme['"' >> *(ascii::char_ - '"') >> '"'];
        
        value = id | literal/* | func_call*/;
        id = qi::lexeme[ascii::char_("a-z") >> *ascii::char_("0-9a-zA-Z")];

    }
    
    qi::symbols<char, BinaryOperatorID> OR_OP,
                                        AND_OP,
                                        COMPARE_OP, 
                                        ADD_OP,
                                        MULT_OP,
                                        POW_OP;
    qi::symbols<char, UnaryOperatorID>  UNARY_MINUS_OP,
                                        NOT_OP;
    
    qi::rule<Iterator, Skipper, Expression()> OR_level, AND_level, COMPARE_level, ADD_level, MULT_level, POW_level;
    qi::rule<Iterator, Skipper, Expression()> UNARY_MINUS_level, NOT_level;

    qi::rule<Iterator, Skipper, Language()> start;
    qi::rule<Iterator, Skipper, Expression()> expression;
    qi::rule<Iterator, Skipper, Expression()>  value;
    
    qi::rule<Iterator, Skipper, ID()> id;

    qi::rule<Iterator, Skipper, Literal()> literal;

    qi::rule<Iterator, Skipper, Statement()> statement;

    qi::rule<Iterator, Skipper, std::string()> string_literal;
    qi::rule<Iterator, Skipper, std::string()> bool_literal;

};



int main() {
    
    std::string str;
    std::getline(std::cin, str);
    
    Grammar<std::string::iterator, ascii::space_type> g;
    std ::string::iterator begin = str.begin();
    std ::string::iterator end = str.end();


    if(qi::phrase_parse(begin, end, g, ascii::space)) {
        std::cout << "Succeed!\n";
        std::cout << "Remain: " << std::string{begin, end} << std::endl;
    }

    return 0;
}