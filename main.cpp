#define BOOST_SPIRIT_USE_PHOENIX_V3

#include <iostream>
#include <fstream>
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

typedef boost::variant<
    int,
    std::string
> Literal;

struct ID {
    std::string name;
};
BOOST_FUSION_ADAPT_STRUCT(ID, (std::string, name))

enum TypeID {
    VOID,
    INT,
    BOOL,
    STRING
};

struct TypedID {
    TypeID type;
    ID name;
    TypedID() = default;
    TypedID(const TypeID type, ID name) : type(type), name(name) {}
};
BOOST_FUSION_ADAPT_STRUCT(TypedID, (TypeID, type)(ID, name))

struct UnaryOperator;
struct BinaryOperator;
struct FunctionCall;

typedef boost::variant<
    Literal,
    ID,
    boost::recursive_wrapper<UnaryOperator>,
    boost::recursive_wrapper<BinaryOperator>,
    boost::recursive_wrapper<FunctionCall>
> Expression;

struct ExpressionList {
    std::vector<Expression> args;
};
BOOST_FUSION_ADAPT_STRUCT(ExpressionList, (std::vector<Expression>, args))

struct FunctionCall {
    ID name;
    ExpressionList args;

    FunctionCall() = default;
    FunctionCall(ID name, const ExpressionList& args) : name(name), args(args) {}
};
BOOST_FUSION_ADAPT_STRUCT(FunctionCall, (ID, name), (ExpressionList, args))

struct UnaryOperator {
    Expression right;
    UnaryOperatorID type;

    UnaryOperator() = default;
    UnaryOperator(UnaryOperatorID type, Expression right) : right(right), type(type) {}
};
BOOST_FUSION_ADAPT_STRUCT(UnaryOperator, (Expression, right)(UnaryOperatorID, type))

struct BinaryOperator {
    Expression right;
    BinaryOperatorID type;
    Expression left;

    BinaryOperator() = default;
    BinaryOperator(Expression left, BinaryOperatorID type , Expression right) : right(right), type(type), left(left) {} 
};
BOOST_FUSION_ADAPT_STRUCT(BinaryOperator, (Expression, right)(BinaryOperatorID, type)(Expression, left))

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

struct ExpressionStatement {
    Expression inner;
};
BOOST_FUSION_ADAPT_STRUCT(ExpressionStatement, (Expression, inner))

typedef boost::variant<
    ExpressionStatement,
    AssignStatement,
    DeclStatement,
    IfStatement,
    boost::recursive_wrapper<WhileStatement>,
    ReturnStatement
> Statement;

struct AssignStatement {
    ID variable;
    Expression right;
};
BOOST_FUSION_ADAPT_STRUCT(AssignStatement, (ID, variable)(Expression, right))

struct DeclStatement {
    TypedID variable;
    Expression right;
};
BOOST_FUSION_ADAPT_STRUCT(DeclStatement, (TypedID, variable)(Expression, right))

struct SingleIfStatement {
    Expression cond;
    std::vector<Statement> body;
};
BOOST_FUSION_ADAPT_STRUCT(SingleIfStatement, (Expression, cond)(std::vector<Statement>, body))

struct IfElseStatement {
    Expression cond;
    std::vector<Statement> body;
    std::vector<Statement> else_body;
};
BOOST_FUSION_ADAPT_STRUCT(IfElseStatement, (Expression, cond)(std::vector<Statement>, body)(std::vector<Statement>, else_body))


struct WhileStatement {
    Expression cond;
    std::vector<Statement> body;
};
BOOST_FUSION_ADAPT_STRUCT(WhileStatement, (Expression, cond)(std::vector<Statement>, body))

struct TypeParametersList {
    std::vector<TypedID> args;
};
BOOST_FUSION_ADAPT_STRUCT(TypeParametersList, (std::vector<TypedID>, args))

struct FuncDeclaration {
    TypedID name;
    TypeParametersList args;
    std::vector<Statement> body;
    FuncDeclaration() = default;
    FuncDeclaration(ID name, const TypeParametersList& args, const TypeID type, const std::vector<Statement> &body) : name(type, name), args(args), body(body) {}
};
BOOST_FUSION_ADAPT_STRUCT(FuncDeclaration, (ID, name)(TypeParametersList, args)(Statement, body))

struct Language {
    std::vector<FuncDeclaration> sequence;
};
BOOST_FUSION_ADAPT_STRUCT(Language, (std::vector<FuncDeclaration>, sequence))

static int level = 0;


std::ostream& operator<<(std::ostream& os, const BinaryOperatorID op) {
    switch (op)
    {
        case BinaryOperatorID::AND:           return os << "&&";
        case BinaryOperatorID::OR:            return os << "||";
        case BinaryOperatorID::EQ:            return os << "==";
        case BinaryOperatorID::NE:            return os << "/=";
        case BinaryOperatorID::LE:            return os << "<";
        case BinaryOperatorID::LESS:          return os << "<=";
        case BinaryOperatorID::GE:            return os << ">";
        case BinaryOperatorID::GREATER:       return os << ">=";
        case BinaryOperatorID::PLUS:          return os << "+";
        case BinaryOperatorID::MINUS:         return os << "-";
        case BinaryOperatorID::MULT:          return os << "*";
        case BinaryOperatorID::DIV:           return os << "/";
        case BinaryOperatorID::POW:           return os << "^";
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, const UnaryOperatorID op) {
    switch (op)
    {
        case UnaryOperatorID::NOT:            return os << "!";
        case UnaryOperatorID::UNARY_MINUS:    return os << "-";
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, const TypeID type) {
    switch (type) {
        case TypeID::INT:                    return os << "Int";
        case TypeID::BOOL:                   return os << "Bool"; 
        case TypeID::STRING:                 return os << "String"; 
    }
    return os;
}

std::ostream& operator<<(std::ostream& os, const TypedID& typed_id);
std::ostream& operator<<(std::ostream& os, const ExpressionList& expr_list);
std::ostream& operator<<(std::ostream& os, const Expression& expr);

std::ostream& operator<<(std::ostream& os, const Statement& stmt) {
    os << std::string(level, '\t') << "Statement" << std::endl;
    ++level;
    
    struct visitor : boost::static_visitor<> {
        visitor(std::ostream& os) : os(os) {}
        std::ostream& os;

        void operator()(const IfStatement& s) const { 
            
            struct visitor_if : boost::static_visitor<> {
                visitor_if(std::ostream& os) : os(os) {}
                std::ostream& os;

                void operator()(const SingleIfStatement& e) const {
                    os << std::string(level, '\t') << "Single if statement" << std::endl;
                    ++level;

                    os << std::string(level, '\t') << "Condition" << std::endl;
                    ++level; 
                    os <<  e.cond;
                    --level;

                    os << std::string(level, '\t') << "Body" << std::endl;
                    ++level;
                    for (const Statement& statement: e.body) {
                        os << statement;
                    }
                    --level;

                    --level;
                }
                void operator()(const IfElseStatement& e) const { 
                    os << std::string(level, '\t') << "If-Else statement" << std::endl;
                    ++level;
                    
                    os << std::string(level, '\t') << "Condition" << std::endl;
                    ++level; 
                    os <<  e.cond;
                    --level;

                    os << std::string(level, '\t') << "Body" << std::endl;
                    ++level;
                    for (const Statement& statement: e.body) {
                        os << statement;
                    }
                    --level;

                    os << std::string(level, '\t') << "Else body" << std::endl;
                    ++level;
                    for (const Statement& statement: e.else_body) {
                        os << statement;
                    }
                    --level;

                    --level;
                }
            };

            boost::apply_visitor(visitor_if(os), s);

        }
        void operator()(WhileStatement const& s) const {
            os << std::string(level, '\t') << "While" << std::endl;
            
            ++level;
            os << std::string(level, '\t') << "Condition" << std::endl;
            
            ++level; 
            os <<  s.cond;
            --level;

            os << std::string(level, '\t') << "Body" << std::endl;

            ++level;
            for (const Statement& statement: s.body) {
                os << statement;
            }
            --level;

            --level;
        }
        
        void operator()(const DeclStatement& s) const { 
            os << std::string(level, '\t') << "Declaring variable" << std::endl;
            ++level;
            os << std::string(level, '\t') << "Variable" << std::endl;
            
            ++level;
            os << s.variable;
            --level;
            os << std::string(level, '\t') << "Assigned to" << std::endl;

            ++level;
            os << s.right;
            --level;
            --level;
        }

        void operator()(const AssignStatement& s) const { 
            
            os << std::string(level, '\t') << "Assigning to variable" << std::endl;
            ++level;
            os << std::string(level, '\t') << "Variable" << std::endl;
            
            ++level;
            os << s.variable;
            
            --level;
            os << std::string(level, '\t') << "Assigned to" << std::endl;

            ++level;
            os << s.right;
            --level;
            
            --level;
        }
        
        void operator()(const ReturnStatement& s) const {
            
            os << std::string(level, '\t') << "Return" << std::endl;
            ++level;
            os << s.return_value;
            --level;
            
        }

        void operator()(const ExpressionStatement& e) const {
            os << e.inner;
        }
        
    };
    
    boost::apply_visitor(visitor(os), stmt);
    --level;
    return os;
}


std::ostream& operator<<(std::ostream& os, const Expression& expr) {
    os << std::string(level, '\t') << "Expression" << std::endl;
    ++level;
    struct visitor_expr : boost::static_visitor<> {
        visitor_expr(std::ostream& os) : os(os) {}
        std::ostream& os;

        void operator()(const Literal& e) const {
            os << std::string(level, '\t') << "Literal" << std::endl;
            ++level;
            os << std::string(level, '\t') << e << std::endl; 
            --level;
        }
        void operator()(const ID& e) const { 
            os << std::string(level, '\t') << "Identifier" << std::endl;
            ++level;
            os << std::string(level, '\t') << e.name << std::endl; 
            --level;
        }
        void operator()(const UnaryOperator& e) const {
            os << std::string(level, '\t') << "Unary Operator" << e.type << std::endl;
            ++level;
            os << std::string(level, '\t') << "Right" << std::endl;
            ++level;
            os << e.right;
            --level;
            --level;
        }
        void operator()(const BinaryOperator& e) const {
            os << std::string(level, '\t') << "Binary Operator" << e.type << std::endl;
            ++level;
            os << std::string(level, '\t') << "Left" << std::endl;
            ++level;
            os << e.left;
            --level;

            os << std::string(level, '\t') << "Right" << std::endl;
            ++level;
            os << e.right;
            --level;
            --level;
        }
        void operator()(const FunctionCall& e) const {
            os << std::string(level, '\t') << "Function call" << std::endl; 
            ++level;
            os << std::string(level, '\t') << "Name" << std::endl;
            ++level;
            os << e.name;
            --level;

            os << std::string(level, '\t') << "Parameteres" << std::endl;
            ++level;
            os << e.args;
            --level;
            --level;
        }
    };
    boost::apply_visitor(visitor_expr(os), expr);
    --level;
    return os;
}


std::ostream& operator<<(std::ostream& os, const ExpressionList& expr_list) {
    os << std::string(level, '\t') << "Expression list" << std::endl;
    ++level;     
    
    ++level;
    if (expr_list.args.empty()) {
        os << std::string(level, '\t') << "Empty" << std::endl;
    }
    else {
        for (const Expression& expression: expr_list.args) {
            os << expression;
        }
    }
    --level;

    --level;
    return os;
}


std::ostream& operator<<(std::ostream& os, const TypedID& typed_id) {
    os << std::string(level, '\t') << "Typed Parameter" << std::endl;
    ++level;     
    os << std::string(level, '\t') << "Name" << std::endl;

    ++level;
    os << typed_id.name; 
    --level;

    os << std::string(level, '\t') << "Type" << std::endl;
    
    ++level;
    os << std::string(level, '\t') << typed_id.type << std::endl;
    --level;

    --level;
    return os;
}


std::ostream& operator<<(std::ostream& os, const TypeParametersList& args) {
    os << std::string(level, '\t') << "List of typed parameteres" << std::endl;
    ++level;
    if (args.args.empty()) {
        os << std::string(level, '\t') << "Empty" << std::endl;
    }   
    else {
        for (const TypedID& typed_id: args.args) {  
            os << typed_id;
        } 
    }
    --level;
    return os;
}

std::ostream& operator<<(std::ostream& os, const FuncDeclaration& func_decl) {
    os << std::string(level, '\t') << "Function declaration" << std::endl;
    ++level;
    
    os << func_decl.name;
    
    os << std::string(level, '\t') << "Arguments" << std::endl;

    ++level;   
    os << func_decl.args; 
    --level;

    os << std::string(level, '\t') << "Body" << std::endl;
    ++level;
    for(const Statement& statement: func_decl.body) {
        os << statement;
    }
    --level;

    --level;
    return os;
}

std::ostream& operator<<(std::ostream& os, const Language& lang) {
    os << "Language" << std::endl;
    ++level;
    for (const FuncDeclaration& func_decl : lang.sequence) {   
        os << func_decl << std::endl; 
    }
    --level;
    return os;
}




template<typename Iterator, typename Skipper>
struct Grammar: qi::grammar<Iterator, Skipper, Language()> {

    Grammar() : Grammar::base_type(start) {
        OR_OP.add
            ("||", BinaryOperatorID::OR);
        AND_OP.add
            ("&&", BinaryOperatorID::AND);
        NOT_OP.add
            ("!",  UnaryOperatorID::NOT);
        COMPARE_OP.add
            ("==", BinaryOperatorID::EQ)
            ("/=", BinaryOperatorID::NE)
            (">=", BinaryOperatorID::GE)
            (">",  BinaryOperatorID::GREATER)
            ("<=", BinaryOperatorID::LE)
            ("<",  BinaryOperatorID::LESS);
        ADD_OP.add
            ("+",  BinaryOperatorID::PLUS)
            ("-",  BinaryOperatorID::PLUS);
        MULT_OP.add
            ("*",  BinaryOperatorID::MULT)
            ("/",  BinaryOperatorID::DIV);
        UNARY_MINUS_OP.add
            ("-",  UnaryOperatorID::UNARY_MINUS);
        POW_OP.add
            ("^",  BinaryOperatorID::POW);

        TYPE_DECL.add
            ("Int", TypeID::INT)
            ("Bool", TypeID::BOOL)
            ("String", TypeID::STRING);
        

        OR_level = AND_level [ _val = _1 ] >> -(OR_OP >> OR_level) [ _val = phx::construct<BinaryOperator>(_val, _1, _2) ];
        AND_level = NOT_level [ _val = _1 ] >> -(AND_OP >> AND_level) [ _val = phx::construct<BinaryOperator>(_val, _1, _2) ];
        
        NOT_level = COMPARE_level [ _val = _1 ] | (NOT_OP >> COMPARE_level) [ _val = phx::construct<UnaryOperator>(_1, _2) ];
        
        COMPARE_level = ADD_level [ _val = _1 ] >> -(COMPARE_OP >> ADD_level) [ _val = phx::construct<BinaryOperator>(_val, _1, _2) ];
        
        ADD_level = MULT_level [ _val = _1 ] >> *(ADD_OP >> MULT_level) [ _val = phx::construct<BinaryOperator>(_val, _1, _2) ];
        MULT_level = UNARY_MINUS_level [ _val = _1 ] >> *(MULT_OP >> UNARY_MINUS_level) [ _val = phx::construct<BinaryOperator>(_val, _1, _2) ];

        UNARY_MINUS_level = POW_level [_val = _1] | (UNARY_MINUS_OP >> POW_level) [ _val = phx::construct<UnaryOperator>(_1, _2) ];
        
        POW_level = ('(' >> expression [_val = _1] >> ')' | value [_val = _1]) >> *(POW_OP >> POW_level) [ _val = phx::construct<BinaryOperator>(_val, _1, _2) ];


        
        start = qi::eps >> *func_decl;
        expression = OR_level | '(' >> expression >> ')' | value;
        value =  literal | func_call | id;
        id = id_literal;

        id_literal = qi::lexeme[qi::lower >> *qi::alnum]; 
        literal = string_literal | bool_literal | bin_literal | qi::int_;
        string_literal = qi::lexeme['"' >> *(ascii::char_ - '"') >> '"'];
        bool_literal = qi::string("True") | qi::string("False");
        bin_literal = qi::lexeme[ascii::char_('B') >> +(ascii::char_('0') | ascii::char_('1'))];

        expression_list = '(' >> -(expression % ',') >> ')';
        func_call = (id >> expression_list)[ _val = phx::construct<FunctionCall>(_1, _2)];

        single_if_statement = qi::lit("If") >> '(' >> expression >> ')' >> '{' >> *statement >> '}';
        if_else_statement = qi::lit("If") >> '(' >> expression >> ')' >> 
                '{' >> *statement >> '}' >> qi::lit("Else") >>
                         '{' >> *statement >> '}';
        if_statement = if_else_statement | single_if_statement;

        while_statement = qi::lit("While") >> '(' >> expression >> ')' >> '{' >> *statement >> '}';

        expression_statement = expression >> ';';

        statement = if_statement | while_statement | assign_statement | decl_statement | expression_statement | return_statement | ';';

        assign_statement = id >> qi::lexeme[":="] >> expression >> ';';

        typed_id = TYPE_DECL >> id;

        decl_statement = qi::lit("Var") >> typed_id >> qi::lexeme[":="] >> expression >> ';';

        return_statement = qi::lit("Return") >> expression >> ';';

        type_params_list = '(' >> -(typed_id % ',') >> ')';

        func_decl = (qi::lit("Func") >> id >> type_params_list >> qi::lit("->") >> TYPE_DECL
            >> '{' >> *(statement) >> '}')[_val = phx::construct<FuncDeclaration>(_1, _2, _3, _4)];

    }
    
    qi::symbols<char, BinaryOperatorID> OR_OP,
                                        AND_OP,
                                        COMPARE_OP, 
                                        ADD_OP,
                                        MULT_OP,
                                        POW_OP;
    qi::symbols<char, UnaryOperatorID>  UNARY_MINUS_OP,
                                        NOT_OP;
    
    qi::symbols<char, TypeID>           TYPE_DECL;

    qi::rule<Iterator, Skipper, Expression()> OR_level, AND_level, COMPARE_level, ADD_level, MULT_level, POW_level;
    qi::rule<Iterator, Skipper, Expression()> UNARY_MINUS_level, NOT_level;
    qi::rule<Iterator, Skipper, Expression()>  value;


    qi::rule<Iterator, Skipper, Language()> start;
    qi::rule<Iterator, Skipper, Expression()> expression;
    qi::rule<Iterator, Skipper, ID()> id;

    // Should be better solution
    qi::rule<Iterator, Skipper, Literal()> literal;
    qi::rule<Iterator, Skipper, std::string()> id_literal;
    //

    qi::rule<Iterator, Skipper, std::string()> string_literal;
    qi::rule<Iterator, Skipper, std::string()> bool_literal;
    qi::rule<Iterator, Skipper, std::string()> bin_literal;

    qi::rule<Iterator, Skipper, ExpressionList()> expression_list;
    qi::rule<Iterator, Skipper, FunctionCall()> func_call;

    qi::rule<Iterator, Skipper, FuncDeclaration()> func_decl;
    qi::rule<Iterator, Skipper, TypeParametersList()> type_params_list; 
    qi::rule<Iterator, Skipper, Statement()> statement;
    qi::rule<Iterator, Skipper, ExpressionStatement()> expression_statement;
    
    qi::rule<Iterator, Skipper, IfStatement()> if_statement;
    qi::rule<Iterator, Skipper, SingleIfStatement()> single_if_statement;
    qi::rule<Iterator, Skipper, IfElseStatement()> if_else_statement;
    qi::rule<Iterator, Skipper, WhileStatement()> while_statement;
    qi::rule<Iterator, Skipper, DeclStatement()> decl_statement;
    qi::rule<Iterator, Skipper, AssignStatement()> assign_statement;
    qi::rule<Iterator, Skipper, ReturnStatement()> return_statement;
    qi::rule<Iterator, Skipper, TypedID()> typed_id;
};



int main(int argc, char* argv[]) {
    std::string str;
    /* Read file */ {
        std::ifstream fin(argv[1]);
        if (!fin) {
            std::cerr << "Can not open file" << std::endl;
            return 1;
        }
        std::stringstream buffer;
        buffer << fin.rdbuf();
        str = buffer.str();
    }


    Grammar<std::string::iterator, ascii::space_type> g;
    std ::string::iterator begin = str.begin();
    std ::string::iterator end = str.end();
    Language lang;

    if(qi::phrase_parse(begin, end, g, ascii::space, lang) && begin == end) {
        std::cout << "Succeed!" << std::endl;
        std::cout << lang;
    }
    else {
        std::cout << "Failed!" << std::endl;
        std::cout << "Look from : " << std::string{begin, end} << std::endl;
    }
    return 0;
}