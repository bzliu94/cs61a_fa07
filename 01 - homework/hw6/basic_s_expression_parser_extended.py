# 2017-06-13

# extended to handle quote syntactic sugar and symbols for methods (e.g. +)

# special symbols supported inspired by mihai bazon and amit patel

# only intentionally handles numbers that are integers

# numbers and identifiers have overlapping definitions, but prefer number over identifier if same-length, or longer token

# have a two-phase token determination stage

"""

quote sugar

add a rule wrapping list using possibly a single quote

--

id/num token definition overlap

modify first/follow sets and eating up either id or num leads to check that it really is most suitable

number is special case of id; prefer number if num/id versions are same length, prefer longer if num/id versions are different lengths

--

first/follow sets

no first/first or first/follow conflicts as long as we do left factoring and substitution, respectively; remember that substitution can introduce new first/first conflicts

--

assume that numbers we encounter are integers

--

ambiguous CFG implies non-deterministic parsing as there would occasionally be multiple ways to parse an input string and, using our LL(1) parser, non-determinism is for parsing is completely handled by and expressed via having first/first or first/follow conflicts

"""

# 2017-04-08

# predictive parser for basic s-expressions

# grammar taken from amit patel

# note that with s-expression parser, we must ignore spaces

"""

from source:
number and identifier tokens overlap; choose the token that leads to largest match and, in case of ties, choose number

our approach:
we de-conflict number and identifier token definitions by restricting definition for identifier token

"""

"""

FRAGMENTS

(none)

NON-TERMINAL RULES

expr = ID | STR | NUM | list | "'" expr
list = "(" seq ")"
seq = epsilon | expr seq

TOKEN RULES

NUM = [0-9]+
ID = [-+*/!@%^&=.a-zA-Z0-9_]+
# note that this definition for ID token overlaps with definition for NUM token
# ID = [.a-zA-Z]+
STR = "\([^\"]+\|\\.\)*"

no look-ahead or look-behind

ignore:
[ \t\n\r]+

"""

import re
import sys
from collections import deque
import string

# TOKENS ENUMERATED
EPSILON = -1
NUM = 0
ID = 1
STR = 2
EOF = -2

# FRAGMENT/TOKEN REGULAR EXPRESSIONS
NUM_RE = r"(?:[0-9]+)"
ID_RE = r"(?:[-+*/!@%^&=.a-zA-Z0-9_]+)"
# ID_RE = r"(?:[.a-zA-Z]+)"
STR_RE = r"(?:\"\([^\"]+\|\\.\)*\")"
MISC_RE = r"(?:" + "\(|\)|\'" + ")"

# breaking abstraction somewhat when we pass these regular expressions to parse() and getTokens()

OVERALL_RE1 = r"\s*((?P<id>" + ID_RE + r")|(?P<str>" + STR_RE + r")|(?P<misc>" + MISC_RE + r"))\s*"

OVERALL_RE2 = r"\s*((?P<num>" + NUM_RE + r"))\s*"

# PARSER CLASSES
class ParseTree:
  def __init__(self):
    self.root = None
  def setRoot(self, node):
    self.root = node
  def getRoot(self):
    return self.root
  def toString(self):
    root = self.getRoot()
    return root.toString()
  def getValue(self):
    return self.getRoot().getValue()
class Node:
  def __init__(self, children):
    self.children = children
  def addChildAtRight(self, child):
    self.children.append(child)
  def getOrderedChildren(self):
    return self.children[ : ]
  def getIthChild(self, i):
    return self.children[i]
  def getName(self):
    return "N/A"
  def getStringComponents(self):
    children = self.getOrderedChildren()
    child_str_list = [x.toString() for x in children]
    components_list = [self.getName()] + child_str_list
    return components_list
  def toString(self):
    components_list = self.getStringComponents()
    if len(components_list) == 1:
      return components_list[0]
    joined_str = string.join(components_list, " ")
    overall_str = "(" + joined_str + ")"
    return overall_str
  def getValue(self):
    pass
  def error(self):
    raise Exception()
  def isExprNode(self):
    return False
  def isListNode(self):
    return False
  def isSeqNode(self):
    return False
  def isQuoteNode(self):
    return False
class ExprNode(Node):
  def __init__(self):
    Node.__init__(self, [])
  def getName(self):
    return "expr"
  def getValue(self):
    children = self.getOrderedChildren()
    child = children[0]
    return child.getValue()
  def isExprNode(self):
    return True
class ListNode(Node):
  def __init__(self):
    Node.__init__(self, [])
  def getName(self):
    return "list"
  def getValue(self):
    children = self.getOrderedChildren()
    child = children[0]
    return list(child.getValue())
  def isListNode(self):
    return True
class SeqNode(Node):
  def __init__(self):
    Node.__init__(self, [])
  def getName(self):
    return "seq"
  def getValue(self):
    children = self.getOrderedChildren()
    if len(children) == 0:
      return deque()
    elif len(children) == 2:
      child1 = children[0]
      child2 = children[1]
      d = child2.getValue()
      d.appendleft(child1.getValue())
      return d
    else:
      self.error()
  def isSeqNode(self):
    return True
class QuoteNode(Node):
  def __init__(self, child):
    Node.__init__(self, [child])
  def getName(self):
    return "quote"
  def getValue(self):
    child = self.getOrderedChildren()[0]
    return ["quote", child.getValue()]
  def isQuoteNode(self):
    return True

"""
RULES ASSOCIATED WITH EPSILON
non-terminals with epsilon in their FIRST set

seq -> EPSILON

special handling
(none)
"""
"""
PARSER OUTPUT FORMAT
nested lists

GRAMMAR WITH SEMANTIC ACTIONS
expr -> ID { expr.value = ID.value }
	| STR { expr.value = STR.value }
	| NUM { expr.value = NUM.value }
	| list { expr.value = list.value }
	| "'" expr { expr1.value = quote(expr2.value) }
	;
list -> '(' seq ')' ; { list.value = list(seq.value) }
seq -> EPSILON { seq.value = deque() }
	| expr seq { seq1.value = seq2.value; seq1.value.prepend(expr.value) }
	;

"""
class Parser:
  def __init__(self, tokens, FIRST, FOLLOW):
    self.token_deque = deque(tokens)
    self.first = FIRST
    self.follow = FOLLOW
  def getFirstSets(self):
    return self.first
  def getFollowSets(self):
    return self.follow
  def parse(self):
    root_node = self.expr()
    tree = ParseTree()
    tree.setRoot(root_node)
    return tree
  def NUM(self):
    token = self._getNextToken()
    node = token
    if node.isNumNode() == True:
      self._removeNextToken()
    else:
      self.error()
    return node
  def ID(self):
    token = self._getNextToken()
    node = token
    if node.isIDNode() == True:
      self._removeNextToken()
    else:
      self.error()
    return node
  def STR(self):
    token = self._getNextToken()
    node = token
    if node.isStrNode() == True:
      self._removeNextToken()
    else:
      self.error()
    return node
  def expr(self):
    curr_token = self._getLookaheadToken()
    node = ExprNode()
    FIRST = self.getFirstSets()
    symbol = Parser.getFIRSTFOLLOWSymbol(curr_token)
    if symbol == NUM:
      num_node = self.NUM()
      node.addChildAtRight(num_node)
    elif symbol == ID:
      id_node = self.ID()
      node.addChildAtRight(id_node)
    elif symbol == STR:
      str_node = self.STR()
      node.addChildAtRight(str_node)
    elif symbol in FIRST["list"]:
      list_node = self.list()
      node.addChildAtRight(list_node)
    elif symbol == "'":
      self.scan("'")
      expr_node = self.expr()
      quote_node = QuoteNode(expr_node)
      node.addChildAtRight(quote_node)
    else:
      self.error()
    return node
  def list(self):
    curr_token = self._getLookaheadToken()
    node = ListNode()
    FIRST = self.getFirstSets()
    symbol = Parser.getFIRSTFOLLOWSymbol(curr_token)
    if symbol == "(":
      self.scan("(")
      seq_node = self.seq()
      node.addChildAtRight(seq_node)
      self.scan(")")
    else:
      self.error()
    return node
  def seq(self):
    curr_token = self._getLookaheadToken()
    node = SeqNode()
    FIRST = self.getFirstSets()
    FOLLOW = self.getFollowSets()
    symbol = Parser.getFIRSTFOLLOWSymbol(curr_token)
    if symbol in FOLLOW["seq"]:
      pass
    elif symbol in FIRST["expr"]:
      expr_node = self.expr()
      node.addChildAtRight(expr_node)
      seq_node = self.seq()
      node.addChildAtRight(seq_node)
    else:
      self.error()
    return node
  def error(self):
    raise Exception()
  def scan(self, chars):
    # curr_str = char
    self.matchString(chars)
  def _getTokens(self):
    return self.token_deque
  # if no next token exists, return None
  def _getNextToken(self):
    if len(self.token_deque) == 0:
      return None
    else:
      return self.token_deque[0]
  def _removeNextToken(self):
    self.token_deque.popleft()
  def matchString(self, curr_str):
    next_token = self._getNextToken()
    lexeme = next_token.getLexeme()
    if curr_str == lexeme:
      self._removeNextToken()
    else:
      self.error()
  # if no next token exists, return None
  def _getLookaheadToken(self):
    next_token = self._getNextToken()
    return next_token
  # token may be None, which we interpret as EOF
  @staticmethod
  def getFIRSTFOLLOWSymbol(token):
    if token == None:
      return EOF
    else:
      return token.getFIRSTFOLLOWSymbol()
    # return token.getFIRSTFOLLOWSymbol()
def parse(line, OVERALL_RE1, OVERALL_RE2, FIRST, FOLLOW):
  tokens = getTokens(line, OVERALL_RE1, OVERALL_RE2)
  # print tokens
  # print [x.getLexeme() for x in tokens]
  parser = Parser(tokens, FIRST, FOLLOW)
  parse_tree = parser.parse()
  # print parse_tree.toString()
  return parse_tree.getValue()
# TOKEN NODE CLASSES
class TokenNode(Node):
  def __init__(self, lexeme):
    Node.__init__(self, [])
    self.lexeme = lexeme
  def getValue(self):
    return self.getLexeme()
  def getLexeme(self):
    return self.lexeme
  def getFIRSTFOLLOWSymbol(self):
    pass
  def isNumNode(self):
    return False
  def isIDNode(self):
    return False
  def isStrNode(self):
    return False
  def isMiscNode(self):
    return False
class NumNode(TokenNode):
  def __init__(self, lexeme):
    TokenNode.__init__(self, lexeme)
  def getValue(self):
    return string.atoi(self.getLexeme())
  def getFIRSTFOLLOWSymbol(self):
    return NUM
  def isNumNode(self):
    return True
class IDNode(TokenNode):
  def __init__(self, lexeme):
    TokenNode.__init__(self, lexeme)
  def getValue(self):
    return self.getLexeme()
  def getFIRSTFOLLOWSymbol(self):
    return ID
  def isIDNode(self):
    return True
class StrNode(TokenNode):
  def __init__(self, lexeme):
    TokenNode.__init__(self, lexeme)
  def getValue(self):
    return self.getLexeme()
  def getFIRSTFOLLOWSymbol(self):
    return STR
  def isStrNode(self):
    return True
class MiscNode(TokenNode):
  def __init__(self, lexeme):
    TokenNode.__init__(self, lexeme)
  def getFIRSTFOLLOWSymbol(self):
    return self.getLexeme()
  def isMiscNode(self):
    return True
# modified to have manual preferring of NUM over ID if same length, or longer of NUM or ID
def getTokens(lexeme_seq_line, OVERALL_RE1, OVERALL_RE2):
  curr_line = lexeme_seq_line
  regex1 = re.compile(OVERALL_RE1)
  regex2 = re.compile(OVERALL_RE2)
  tokens = []
  curr_pos = 0
  num_chars = len(curr_line)
  while curr_pos != num_chars:
    m1 = regex1.match(curr_line, curr_pos)
    # this is used for non-space-related matching
    lexeme_text1 = m1.group(1)
    # this is used for with-space proper offset
    next_lexeme_text1 = m1.group(0)
    next_lexeme_text = next_lexeme_text1
    token = None
    """
    if m.group("num") != None:
      token = NumNode(lexeme_text)
    elif m.group("id") != None:
    """
    if m1.group("id") != None:
      m2 = regex2.match(curr_line, curr_pos)
      token = None
      chose_num = False
      if m2 != None and m2.group("num") != None:
        lexeme_text2 = m2.group(1)
        next_lexeme_text2 = m2.group(0)
        if len(lexeme_text2) >= len(lexeme_text1):
          next_lexeme_text = next_lexeme_text2
          token = NumNode(lexeme_text2)
          chose_num = True
      if chose_num == False:
        next_lexeme_text = next_lexeme_text1
        token = IDNode(lexeme_text1)
    elif m1.group("str") != None:
      token = StrNode(lexeme_text1)
    elif m1.group("misc") != None:
      token = MiscNode(lexeme_text1)
    if token != None:
      tokens.append(token)
    # curr_line = regex.sub("", curr_line, count = 1)
    token_char_size = len(next_lexeme_text)
    curr_pos += token_char_size
  return tokens

"""

FIRST
FIRST(expr) = {ID, NUM, STR, '(', "'"}
FIRST(list) = {'(', "'"}
FIRST(seq) = {EPSILON, ID, NUM, STR, '(', "'"}

FOLLOW
FOLLOW(expr) = {EOF, ID, NUM, STR, '(', ')', "'"}
FOLLOW(list) = {EOF, ID, NUM, STR, '(', ')', "'"}
FOLLOW(seq) = {')'}

"""

FIRST = {}
FOLLOW = {}
FIRST["expr"] = {ID, NUM, STR, '(', "'"}
FIRST["list"] = {'('}
FIRST["seq"] = {EPSILON, ID, NUM, STR, '(', "'"}
FOLLOW["expr"] = {EOF, ID, NUM, STR, '(', ')', "'"}
FOLLOW["list"] = {EOF, ID, NUM, STR, '(', ')', "'"}
FOLLOW["seq"] = {')'}

def main():
  stream = sys.stdin
  line = stream.readline()
  # line = "((1 2) (3 4) ...)"
  # line = "((1 2) (2 3) ...)"
  # line = "((1 2 ...) (2 3 ...) ...)"
  # line = "(2 1 4 3 ...)"
  # line = "((1 3 ...) (2 4 ...))"
  # line = "((1 ...) ...)"

  # line = "(1 ((2 3) 4) 5 ...)"

  # tokens = getTokens(line, OVERALL_RE)
  # print tokens
  value = parse(line, OVERALL_RE1, OVERALL_RE2, FIRST, FOLLOW)
  print value
if __name__ == "__main__":
  main()


