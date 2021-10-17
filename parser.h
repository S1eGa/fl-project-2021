#ifndef _GRAMMAR 
#define _GRAMMAR

#include <vector>

#include "boost/spirit/include/qi.hpp"

namespace qi = boost::spirit::qi;


template<typename Iterator>
struct Grammar: qi::grammar<Iterator> {

    template<typename TokensList>
    Grammar(const TokensList& tokens) : Grammar::base_type{start} {
        start = tokens.ID % ',';
    }

    qi::rule<Iterator> start;
};

#endif