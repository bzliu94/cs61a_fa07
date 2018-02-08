# 2018-02-08

# replacing name from hw #6 with leftmostElement and leftmostElementIsComposite

# the important thing is that each non-punctuation token is associated with some node

# 2018-01-31

# have token nodes

# have in-order order

import string
from basic_s_expression_parser_extended import *
from collections import defaultdict

NIL = 0

class Counter:
  def __init__(self, init_value = 1):
    self.curr_value = init_value
  def getCurrValue(self):
    return self.curr_value
  def setCurrValue(self, value):
    self.curr_value = value
  def increment(self):
    self.setCurrValue(self.getCurrValue() + 1)

# each token gets a node

class Node:
  # have a class variable
  id_counter = 0
  def __init__(self, leftmostElement, leftmostElementIsComposite, children, is_application, parent = None):
    self.leftmostElement = leftmostElement
    self.leftmostElementIsComposite = leftmostElementIsComposite
    self.children = children[ : ]
    self.is_application = is_application
    self.parent = parent
    self.id_num = Node.id_counter
    Node.id_counter += 1

    # only for parameter tokens
    self.parameter_ID = None

    # only for reference tokens
    self.reference_ID = None

  # LOW-LEVEL METHODS
  ## GETTERS/SETTERS
  def getLeftmostElement(self):
    return self.leftmostElement
  def getLeftmostElementIsComposite(self):
    return self.leftmostElementIsComposite
  def getChildren(self):
    return self.children[ : ]
  def getNumChildren(self):
    return len(self.getChildren())
  def setChildren(self, children):
    self.children = children[ : ]
  def isApplication(self):
    return self.is_application
  def getParent(self):
    return self.parent
  def setParent(self, node):
    self.parent = node
  def haveParent(self):
    return self.parent != None
  def getIDNum(self):
    return self.id_num
  def setLeftmostElement(self, element):
    self.leftmostElement = element
  def setLeftmostElementIsComposite(self, value):
    self.leftmostElementIsComposite = value
  def getName(self):
    if self.getLeftmostElementIsComposite() == False:
      return self.getLeftmostElement()
    else:
      return None
  # MEDIUM-LEVEL METHODS
  ## AUXILIARY METHODS
  # interpret hierarchy as string associated with collection of nested lists
  def toString(self, use_parentheses = False, with_translation = False, translation_prefix = "g"):
    leftmostElement = None
    if self.getLeftmostElementIsComposite() == True:
      leftmostElement = self.getLeftmostElement().toString(use_parentheses, with_translation, translation_prefix)
    else:
      if self.isParameterNodeWithOverallParameterListOfNonNil() == True and with_translation == True:
        # special for translation
        leftmostElement = translation_prefix + str(self.getParameterID())
      elif self.isNonNilVariableReference() == True and with_translation == True:
        # special for translation
        leftmostElement = translation_prefix + str(self.getReferenceID())
      else:
        leftmostElement = self.getLeftmostElement()
    children = self.getChildren()
    num_children = self.getNumChildren()
    children_str_list = [x.toString(use_parentheses, with_translation, translation_prefix) for x in children]
    next_leftmost_element = leftmostElement if (self.isNilVariableReference() == False and self.isParameterNodeWithOverallParameterListOfNil() == False) else ""
    result_str_list = [next_leftmost_element] + children_str_list
    result_str = None
    if num_children == 0:
      result_str = next_leftmost_element
    else:
      result_str = string.join(result_str_list, " ")
    left_marker = "(" if use_parentheses == True else "["
    right_marker = ")" if use_parentheses == True else "]"
    if self.isApplication() == True:
      result_str = left_marker + result_str + right_marker
    return result_str
  # a weakness is that we don't distinguish between function application and function reference
  def toIDString(self):
    id_num_str = str(self.getIDNum())
    children = self.getChildren()
    num_children = self.getNumChildren()
    children_str_list = [x.toIDString() for x in children]
    parent = self.getParent()
    have_parent = self.haveParent()
    parent_id_num_str = str(None) if have_parent == False else str(parent.getIDNum())
    result_str_list = [id_num_str, parent_id_num_str] + children_str_list
    result_str = None
    if num_children == 0:
      result_str = "[" + string.join([id_num_str, parent_id_num_str], " ") + "]"
    else:
      result_str = "[" + string.join(result_str_list, " ") + "]"
    return result_str
  def toSExprString(self):
    return self.toString(True)
  # retrieve hierarchy in form of nested lists
  def toFlattenedList(self):
    children = self.getChildren()
    num_children = self.getNumChildren()
    result_list = []
    if self.getLeftmostElementIsComposite() == False:
      result_list.append(self)
    else:
      result_list.extend(self.getLeftmostElement().toFlattenedList())
    if num_children != 0:
      result_list.extend(reduce(lambda x, y: x + y, [x.toFlattenedList() for x in children]))
    return result_list
  # LOW-LEVEL METHODS
  ## GETTERS/SETTERS
  def isFirstChild(self):
    if self.haveParent() == False:
      return False
    else:
      parent = self.getParent()
      parent_children = parent.getChildren()
      index = parent_children.index(self)
      if index == 0:
        return True
      else:
        return False
  def isSecondChild(self):
    if self.haveParent() == False:
      return False
    else:
      parent = self.getParent()
      parent_children = parent.getChildren()
      index = parent_children.index(self)
      if index == 1:
        return True
      else:
        return False
  # i is 0-indexed
  def isIthChild(self, i):
    if self.haveParent() == False:
      return False
    else:
      parent = self.getParent()
      parent_children = parent.getChildren()
      index = parent_children.index(self)
      if index == i:
        return True
      else:
        return False
  # HIGH-LEVEL METHODS
  ## CHECKS/SETTERS
  # checks that parent node is associated with a particular function name
  def checkParentForName(self, name):
    parent = self.getParent()
    have_parent = self.haveParent()
    parent_name = parent.getName() if have_parent == True else None
    if have_parent == False:
      return False
    else:
      if parent_name == name:
        return True
      else:
        return False
  # checks that there exists an ancestor node that is associated with a particular function name
  # useful for e.g. when we want to know if at some point we are ever in a quoted expression
  def checkParentsForName(self, name):
    parent = self.getParent()
    have_parent = self.haveParent()
    parent_name = parent.getName() if have_parent == True else None
    if have_parent == False:
      return False
    else:
      if parent_name == name:
        return True
      else:
        return parent.checkParentsForName(name)
  ## CONSTRUCTORS
  @staticmethod
  def convertToNodeHierarchy(nested_lists):
    return Node.convertToNodeHierarchyHelper(nested_lists, None)
  @staticmethod
  def convertToNodeHierarchyHelper(nested_lists, parent):
    # if an element is a list, recurse and create a node; otherwise, create a node and do not recurse

    if isinstance(nested_lists, list) == False:
      # we are a token
      leftmostElement = str(nested_lists)
      leftmostElementIsComposite = False
      return Node(leftmostElement, leftmostElementIsComposite, [], False, parent)
    else:
      # we are a combination
      curr_list = nested_lists
      leftmostElement = None
      leftmostElementIsComposite = None
      curr_node = Node(None, None, [], True, parent)
      if len(curr_list) == 0:
        # we are nil
        leftmostElement = NIL
        leftmostElementIsComposite = False
      else:
        # we have an operator and 0+ operands
        if isinstance(curr_list[0], list) == False:
          # operator is simple
          leftmostElement = curr_list[0]
          leftmostElementIsComposite = False
        else:
          # operator is a combination
          leftmostElement = Node.convertToNodeHierarchyHelper(curr_list[0], curr_node)
          leftmostElementIsComposite = True
        remaining_elements = curr_list[1 : ]
        child_nodes = [Node.convertToNodeHierarchyHelper(x, curr_node) for x in remaining_elements]
        curr_node.setChildren(child_nodes)
      curr_node.setLeftmostElement(leftmostElement)
      curr_node.setLeftmostElementIsComposite(leftmostElementIsComposite)
      return curr_node

  # type one

  def isLambdaKeyword(self):
    return self.getName() == "lambda"

  def _isFirstParameter(self):
    # we're assuming that checkParentForName() handles case where parent is non-existent
    return self.checkParentForName("lambda") and self.isFirstChild()

  def _isNonFirstParameter(self):
    # we're assuming that checkParentForName() handles case where parent is non-existent
    return self.haveParent() and self.getParent().checkParentForName("lambda") and self.getParent().isFirstChild()

  # type two

  # we note that for hw #6 extra credit (inferred types), we possibly get wrong results if we don't treat lambda form in a special way; the explanation is that we did not treat lambda differently and say that wrong results are due to short-sighted type inference rules

  # note that token nodes are generated w.r.t. naive perspective of s-expression

  # we are of course assuming we can't overwrite lambda s.t. it is a variable

  def isParameter(self):
    is_first_parameter = self._isFirstParameter()
    is_non_first_parameter = self._isNonFirstParameter()
    is_parameter = is_first_parameter or is_non_first_parameter
    return is_parameter

  # we want to know when we have an empty parameter list (i.e. parameter list is nil)

  def isParameterNodeWithOverallParameterListOfNil(self):
    return self.isParameter() == True and self.getLeftmostElement() == NIL

  def isParameterNodeWithOverallParameterListOfNonNil(self):
    return self.isParameter() == True and self.getLeftmostElement() != NIL

  # type three

  # we want to know when we have a reference that is to value of '() (i.e. s-expression form of nil)

  def isVariableReference(self):
    return self.isLambdaKeyword() == False and self.isParameter() == False

  def isNilVariableReference(self):
    return self.isVariableReference() == True and self.getLeftmostElement() == NIL

  def isNonNilVariableReference(self):
    return self.isVariableReference() == True and self.getLeftmostElement() != NIL

  def findClosestLambdaAncestor(self, include_start_node = False):
    return self.findClosestLambdaAncestorHelper(include_start_node)

  def findClosestLambdaAncestorHelper(self, include_curr_node = False):
    # if we are willing to consider current node as closest lambda ancestor, check it
    if include_curr_node == True and self.isLambdaKeyword() == True:
      return self
    # consider parent of current node as closest lambda ancestor (if we have a parent)
    if self.haveParent() == True:
      return self.getParent().findClosestLambdaAncestorHelper(True)
    # if we have no parent and we encountered no lambdas, return a flag
    return None

  def getParameterTokenNodesForLambdaTokenNode(self):
    # we assume the current node is one for a lambda keyword token
    children = self.getChildren()
    leftmost_parameter_node = children[0]
    result_parameter_token_nodes = None
    if leftmost_parameter_node.getLeftmostElement() == NIL:
      result_parameter_token_nodes = []
    else:
      result_parameter_token_nodes = [leftmost_parameter_node] + leftmost_parameter_node.getChildren()
    return result_parameter_token_nodes

  def getParameterID(self):
    return self.parameter_ID

  def setParameterID(self, ID_value):
    self.parameter_ID = ID_value

  def inorderNumberParameterTokenNodes(self, start_ID_value = 1):
    counter = Counter(start_ID_value)
    self.inorderNumberParameterTokenNodesHelper(counter)

  def inorderNumberParameterTokenNodesHelper(self, counter):
    # if operator is composite, we recurse for operator; 
    # if we have children, we recurse for each; 
    # for both cases, we act if we have token node for a lambda keyword 
    # and access the associated parameters accordingly
    leftmostElement = self.getLeftmostElement()
    leftmostElementIsComposite = self.getLeftmostElementIsComposite()
    children = self.getChildren()

    if leftmostElementIsComposite == True:
      leftmostElement.inorderNumberParameterTokenNodesHelper(counter)
    if self.isLambdaKeyword() == True:
      parameter_token_nodes = self.getParameterTokenNodesForLambdaTokenNode()
      for curr_parameter_token_node in parameter_token_nodes:
        curr_parameter_token_node.setParameterID(counter.getCurrValue())
        counter.increment()
    if self.getNumChildren() != 0:
      for child in children:
        child.inorderNumberParameterTokenNodesHelper(counter)

  def getReferenceID(self):
    return self.reference_ID

  def setReferenceID(self, ID_value):
    self.reference_ID = ID_value

line = "(lambda (x) (lambda (y) (x (lambda (x) (y x)))))"

# line = "(lambda (x z) (lambda (y) (x (lambda (x) (y x)))))"

# line = "(lambda (x z) ((lambda (y) y) x))"

# line = "(lambda () ())"

# line = "(lambda () (lambda (x) x))"

value = parse(line, OVERALL_RE1, OVERALL_RE2, FIRST, FOLLOW)

node = Node.convertToNodeHierarchy(value)
flattened_node_list = node.toFlattenedList()

def getStatusStr(token_node):
  status_str = None
  if token_node.isLambdaKeyword() == True:
    status_str = "lambda"
  elif token_node.isParameterNodeWithOverallParameterListOfNonNil() == True:
    status_str = "parameter"
  elif token_node.isParameterNodeWithOverallParameterListOfNil() == True:
    status_str = "nil parameter"
  elif token_node.isNonNilVariableReference() == True:
    status_str = "reference"
  elif token_node.isNilVariableReference() == True:
    status_str = "nil reference"
  return status_str

tokenNodeToClosestLambdaAncestorInclusiveDict = {}

for token_node in flattened_node_list:
  closest_lambda_ancestor_inclusive = token_node.findClosestLambdaAncestor(True)
  tokenNodeToClosestLambdaAncestorInclusiveDict[token_node] = closest_lambda_ancestor_inclusive

lambdaToClosestLambdaAncestorExclusiveDict = {}

lambda_token_nodes = [x for x in flattened_node_list if x.isLambdaKeyword() == True]

for lambda_token_node in lambda_token_nodes:
  closest_lambda_ancestor_exclusive = lambda_token_node.findClosestLambdaAncestor(False)
  lambdaToClosestLambdaAncestorExclusiveDict[lambda_token_node] = closest_lambda_ancestor_exclusive

lambdaParameterNameTupleToParameterTokenNodeDict = {}

for lambda_token_node in lambda_token_nodes:
  for parameter_token_node in lambda_token_node.getParameterTokenNodesForLambdaTokenNode():
    lambdaParameterNameTupleToParameterTokenNodeDict[(lambda_token_node, parameter_token_node.getName())] = parameter_token_node

root_lambda_token_node = lambda_token_nodes[0]

root_lambda_token_node.inorderNumberParameterTokenNodes()

non_nil_reference_token_nodes = [x for x in flattened_node_list if x.isNonNilVariableReference() == True]

def setReferenceIDForReferenceTokenNode(curr_reference_token_node, tntclaid, ltclaed, lpnttptnd):
  closest_lambda_token_node = tntclaid[curr_reference_token_node]
  setReferenceIDForReferenceTokenNodeHelper(curr_reference_token_node, closest_lambda_token_node, ltclaed, lpnttptnd)

def setReferenceIDForReferenceTokenNodeHelper(curr_reference_token_node, curr_lambda_token_node, ltclaed, lpnttptnd):
  if curr_lambda_token_node == None:
    raise Exception("never found a suitable lambda s.t. parameter exists by desired name")
  else:
    curr_key = (curr_lambda_token_node, curr_reference_token_node.getName())
    if curr_key in lpnttptnd:
      # test whether desired name exists as a parameter for this lambda
      matching_parameter_token_node = lpnttptnd[curr_key]
      curr_reference_token_node.setReferenceID(matching_parameter_token_node.getParameterID())
    else:
      # otherwise, continue to next-closest lambda
      next_lambda_token_node = ltclaed[curr_lambda_token_node]
      setReferenceIDForReferenceTokenNodeHelper(curr_reference_token_node, next_lambda_token_node, ltclaed, lpnttptnd)

for non_nil_reference_token_node in non_nil_reference_token_nodes:
  setReferenceIDForReferenceTokenNode(non_nil_reference_token_node, tokenNodeToClosestLambdaAncestorInclusiveDict, lambdaToClosestLambdaAncestorExclusiveDict, lambdaParameterNameTupleToParameterTokenNodeDict)

print root_lambda_token_node.toString(use_parentheses = True, with_translation = True, translation_prefix = "g")


