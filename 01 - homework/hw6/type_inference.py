# 2018-01-18

# we have a lot of implied lambdas, but we can ignore them as they are throwaway; they don't have names

import string
from basic_s_expression_parser_extended import *
from collections import defaultdict

# "unknown" is un-set
UNKNOWN = 0
PROCEDURE = 1
NUMBER = 2
LIST = 3
SENTENCE_OR_WORD = 4
CONFLICTING = 5

# each token gets a node
# Node ought to be an abstract class
class Node:
  # have a class variable
  id_counter = 0
  def __init__(self, name, children, is_application, parent = None):
    self.name = name
    self.children = children[ : ]
    self.is_application = is_application
    self.parent = parent
    self.id_num = Node.id_counter
    Node.id_counter += 1
    self.type_code = UNKNOWN
  # LOW-LEVEL METHODS
  ## GETTERS/SETTERS
  def getName(self):
    return self.name
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
  # MEDIUM-LEVEL METHODS
  ## AUXILIARY METHODS
  # interpret hierarchy as string associated with collection of nested lists
  def toString(self):
    name = self.getName()
    children = self.getChildren()
    num_children = self.getNumChildren()
    children_str_list = [x.toString() for x in children]
    result_str_list = [name] + children_str_list
    result_str = None
    if num_children == 0:
      result_str = name
    else:
      result_str = string.join(result_str_list, " ")
    if self.isApplication() == True:
      result_str = "[" + result_str + "]"
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
  # retrieve hierarchy in form of nested lists
  def toFlattenedList(self):
    children = self.getChildren()
    num_children = self.getNumChildren()
    if num_children == 0:
      return [self]
    else:
      return [self] + reduce(lambda x, y: x + y, [x.toFlattenedList() for x in children])
  # type is what we infer after considering hierarchy
  def getTypeCode(self):
    return self.type_code
  def setTypeCode(self, type_code):
    self.type_code = type_code
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
  # first provided type
  def checkIsProcedure(self):
    parent = self.getParent()
    have_parent = self.haveParent()
    parent_name = parent.getName() if have_parent == True else None
    if self.checkParentsForName("quote") == False and self.isApplication() == True:
      return True
    elif self.isFirstChild() == True and (parent_name == "map" or parent_name == "every") and self.isApplication() == False:
      return True
    else:
      return False
  # second provided type
  def checkIsNumber(self):
    parent = self.getParent()
    have_parent = self.haveParent()
    parent_name = parent.getName() if have_parent == True else None
    if parent_name in ["+", "-", "max", "min"] and self.isApplication() == False:
      return True
    else:
      return False
  # third provided type
  def checkIsList(self):
    parent = self.getParent()
    have_parent = self.haveParent()
    parent_name = parent.getName() if have_parent == True else None
    if parent_name == "append" and self.isApplication() == False:
      return True
    elif parent_name in ["map", "member"] and self.isSecondChild() == True and self.isApplication() == False:
      return True
    else:
      return False
  # fourth provided type
  def checkIsSentenceOrWord(self):
    parent = self.getParent()
    have_parent = self.haveParent()
    parent_name = parent.getName() if have_parent == True else None
    if parent_name in ["first", "butfirst", "sentence", "member?"] and self.isApplication() == False:
      return True
    elif parent_name == "every" and self.isSecondChild() == True and self.isApplication() == False:
      return True
    else:
      return False
  def setTypeBasedOnChecks(self):
    tentative_types = []
    final_type = UNKNOWN
    if self.checkIsProcedure() == True:
      tentative_types.append(PROCEDURE)
    if self.checkIsNumber() == True:
      tentative_types.append(NUMBER)
    if self.checkIsList() == True:
      tentative_types.append(LIST)
    if self.checkIsSentenceOrWord() == True:
      tentative_types.append(SENTENCE_OR_WORD)
    if len(tentative_types) > 1:
      final_type = CONFLICTING
    elif len(tentative_types) == 1:
      final_type = tentative_types[0]
    elif len(tentative_types) == 0:
      final_type = UNKNOWN
    self.setTypeCode(final_type)
  ## CONSTRUCTORS
  @staticmethod
  def convertToNodeHierarchy(nested_lists):
    return Node.convertToNodeHierarchyHelper(nested_lists, None)
  @staticmethod
  def convertToNodeHierarchyHelper(nested_lists, parent):
    if isinstance(nested_lists, list) == False:
      # leaves are non-lists
      name = str(nested_lists)
      return Node(name, [], False, parent)
    else:
      # we have a leaf, with first element as name and with rest of elements for nodes
      curr_list = nested_lists
      name = curr_list[0]
      remaining_elements = curr_list[1 : ]
      curr_node = Node(name, [], True, parent)
      child_nodes = [Node.convertToNodeHierarchyHelper(x, curr_node) for x in remaining_elements]
      curr_node.setChildren(child_nodes)
      return curr_node
  ## AUXILIARY METHODS
  # these methods combine assessments across multiple function calls (i.e. nodes)
  # mostly, we check that we are consistent; otherwise, we are conflicting
  @staticmethod
  def consolidateTypesForGivenNames(nodes, chosen_names):
    name_to_type_code_dict = defaultdict(lambda: UNKNOWN)
    chosen_name_set = set(chosen_names)
    Node.consolidateTypesForGivenNamesHelper(nodes, chosen_name_set, name_to_type_code_dict)
    return name_to_type_code_dict
  @staticmethod
  def consolidateTypesForGivenNamesHelper(nodes, chosen_name_set, name_to_type_code_dict):
    for node in nodes:
      curr_name = node.getName()
      curr_type_code = node.getTypeCode()
      next_type_code = curr_type_code
      # chosen name set determines whether we care about a variable
      if curr_name in chosen_name_set:
        prev_type_code = name_to_type_code_dict[curr_name]
        # if we are unknown, we are not improving
        # if we are conflicting, we jump straight to conflicting
        # if we are inconsistent, we are conflicting
        if curr_type_code == UNKNOWN:
          next_type_code = prev_type_code
        elif curr_type_code == CONFLICTING:
          next_type_code = CONFLICTING
        elif curr_type_code != prev_type_code and prev_type_code != UNKNOWN:
          next_type_code = CONFLICTING
        name_to_type_code_dict[curr_name] = next_type_code
  @staticmethod
  def getTypeStr(type_code):
    type_str = None
    if type_code == UNKNOWN:
      type_str = "?"
    elif type_code == PROCEDURE:
      type_str = "procedure"
    elif type_code == NUMBER:
      type_str = "number"
    elif type_code == LIST:
      type_str = "list"
    elif type_code == SENTENCE_OR_WORD:
      type_str = "sentence-or-word"
    elif type_code == CONFLICTING:
      type_str = "x"
    return type_str

# need to pre-process to get body for a function definition
line = "(define (foo a b c d e f) (f (append (a b) c '(b c)) (+ 5 d) (sentence (first e) f)))))"
# need to extract variables that we care about; the variables we care about are the parameters to the given function
value = parse(line, OVERALL_RE1, OVERALL_RE2, FIRST, FOLLOW)
node = Node.convertToNodeHierarchy(value)
flattened_node_list = node.toFlattenedList()
# right now, we have a problem - for parsing, we are not distinguishing between function un-applied and function applied with zero arguments, and we should be
# variable_names = ["a", "b", "c", "d", "e", "f"]
variable_names = [x.getName() for x in node.getChildren()[0].getChildren()]
for node in flattened_node_list:
  node.setTypeBasedOnChecks()
curr_name_to_type_code_dict = Node.consolidateTypesForGivenNames(flattened_node_list, variable_names)
pair_str_list = []
for name in variable_names:
  type_code = curr_name_to_type_code_dict[name]
  type_str = Node.getTypeStr(type_code)
  pair_str = "(" + name + " " + type_str + ")"
  pair_str_list.append(pair_str)
next_pair_str = None
if len(variable_names) == 0:
  next_pair_str = ""
else:
  next_pair_str = reduce(lambda x, y: x + " " + y, pair_str_list)
result_str = "(" + next_pair_str + ")"
print result_str


