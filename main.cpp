#include "parser.h"
#include "lexer.h"

#include <iostream>
#include <string>

int main() {
    typedef lex::lexertl::token<
        char const*, boost::mpl::vector<std::string>
    > token_type;

    typedef lex::lexertl::lexer<token_type> lexer_type;

    typedef Tokens<lexer_type>::iterator_type iterator_type;

    std::string str;
    std::getline(std::cin, str);
    char const* first = str.c_str();
    char const* last = &first[str.size()];

    Tokens<lexer_type> lexer;
    Grammar<iterator_type> g(lexer);

    if(lex::tokenize_and_parse(first, last, lexer, g)) {
        std::cout << "Succeed!\n";
        std::cout << "Remain: " << std::string{first, last} << std::endl;
    }

    return 0;
}