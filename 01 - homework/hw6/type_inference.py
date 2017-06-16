print "hello"

from basic_s_expression_parser_extended import *

line = "(f (append (a b) c '(b c)) (+ 5 d) (sentence (first e) f)) ) )"
# line = "(a1 1 2)"
# line = "(a1 1.1 2)"
# line = "'()"

value = parse(line, OVERALL_RE1, OVERALL_RE2, FIRST, FOLLOW)
print value

# parser will have to be extended to handle quote syntactic sugar and symbols for methods (e.g. +)

# a key - a pure number is never an identifier

# only handles numbers that are integers

# handles non-integer numbers by treating them as identifiers


