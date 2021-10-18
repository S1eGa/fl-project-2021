#include <iostream>
#include <string>
#include <vector>

#include "boost/spirit/include/qi.hpp"
#include <boost/variant.hpp>

using namespace boost::spirit;

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
    bool,
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
    ID,
    Literal,
    FunctionCall
> Value;

typedef boost::variant<
boost::recursive_wrapper<UnaryOperator>,
boost::recursive_wrapper<BinaryOperator>,
boost::recursive_wrapper<Value>
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
};
BOOST_FUSION_ADAPT_STRUCT(UnaryOperator, (UnaryOperatorID, type), (Expression, right))

struct BinaryOperator {
    BinaryOperatorID type;
    Expression left;
    Expression right;
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
    Value return_value;
};
BOOST_FUSION_ADAPT_STRUCT(ReturnStatement, (Value, return_value))

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
    std::vector<FuncDeclaration> sequence;
};
BOOST_FUSION_ADAPT_STRUCT(Language, (std::vector<FuncDeclaration>, sequence))


template<typename Iterator, typename Skipper>
struct Grammar: qi::grammar<Iterator, Skipper, Language()> {

    Grammar() : Grammar::base_type(start) {
        OR_OP.add
            ("||", BinaryOperatorID::OR);
        AND_OP.add
            ("&&", BinaryOperatorID::AND);
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
        POW_OP.add
            ("^", BinaryOperatorID::POW);

    }
    qi::symbols<char, BinaryOperatorID> OR_OP,
                                        AND_OP,
                                        COMPARE_OP, 
                                        ADD_OP,
                                        MULT_OP,
                                        POW_OP;
    
    qi::rule<Iterator, Skipper, Language()> start;
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