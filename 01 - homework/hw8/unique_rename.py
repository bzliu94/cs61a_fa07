# 2018-02-08

# deal with s-expressions instead of favoring left-most elements
# we find that using specific terms introduced in the textbook 
# makes the process go more smoothly

# 2018-01-31

# we hade token nodes

import string
from basic_s_expression_parser_extended import *
from collections import defaultdict

TRANSLATION_PREFIX_STR = "g"

class Counter:
  def __init__(self, init_value = 1):
    self.curr_value = init_value
  def getCurrValue(self):
    return self.curr_value
  def setCurrValue(self, value):
    self.curr_value = value
  def increment(self):
    self.setCurrValue(self.getCurrValue() + 1)

class Entity:
  def __init__(self, parent_entity):
    self.parent_entity = parent_entity
    # only use this for parameter primitives
    self.parameter_ID = None
    # only use this for reference primitives
    self.reference_ID = None
    # use this for dealing with scope for lambdas
    self.closest_lambda_ancestor_inclusive = None
    self.closest_lambda_ancestor_exclusive = None
    # use this for dealing with direct parameter primitives for a name for a lambda
    self.direct_parameter_name_to_primitive_dict = {}
  def getParentEntity(self):
    return self.parent_entity
  def haveParentEntity(self):
    return self.parent_entity != None
  # i is 0-indexed
  def isIthSubexpression(self, i):
    if self.haveParentEntity() == False:
      return False
    else:
      parent_entity = self.getParentEntity()
      if parent_entity.isSExpr() == False:
        return False
      else:
        num_subexpressions = parent_entity.getNumSubexpressions()
        if num_subexpressions < (i + 1):
          return False
        else:
          return parent_entity.getIthSubexpression(i) == self
  # i is 0-indexed
  def getIthSubexpression(self, i):
    return None
  def toString(self, do_translated = False, translation_prefix_str = TRANSLATION_PREFIX_STR):
    pass
  def toTranslatedString(self, prefix_str = TRANSLATION_PREFIX_STR):
    pass
  def isPrimitive(self):
    return False
  def isSExpr(self):
    return False
  def toPreorderPrimitiveList(self):
    pass
  def toPreorderSExprList(self):
    pass
  def isDirectLambdaSExpr(self):
    return False
  def isDirectLambdaParameterPrimitive(self):
    return False
  def isVariableReference(self):
    return False
  def isDirectLambdaKeywordPrimitive(self):
    return False
  def setParameterID(self, ID_value):
    self.parameter_ID = ID_value
  def getParameterID(self):
    return self.parameter_ID
  def setReferenceID(self, ID_value):
    self.reference_ID = ID_value
  def getReferenceID(self):
    return self.reference_ID
  def getClosestLambdaAncestor(self, include_curr_sexpr = False):
    return self.getClosestLambdaAncestorHelper(include_curr_sexpr)
  def getClosestLambdaAncestorHelper(self, include_curr_sexpr = False):
    # if we are willing to consider current node as closest lambda ancestor, check it
    if include_curr_sexpr == True and self.isSExpr() == True and self.isDirectLambdaSExpr() == True:
      return self
    # consider parent of current entity as closest lambda ancestor (if we have a parent)
    if self.haveParentEntity() == True:
      return self.getParentEntity().getClosestLambdaAncestorHelper(True)
    # if we have no parent and we encountered no lambdas, return a flag
    return None
  def toStatus(self):
    status = None
    if self.isDirectLambdaSExpr() == True:
      have_subexpressions = self.getNumSubexpressions() > 0
      subexpressions = self.getSubexpressions()
      name = None
      if have_subexpressions == False or subexpressions[0].isPrimitive() == False:
        name = "have no primitive operator"
      else:
        first_subexpression = subexpressions[0]
        primitive_operator = first_subexpression
        name = primitive_operator.getCharStr()
      status = (name, "lambda sexpr")
    elif self.isDirectLambdaParameterPrimitive() == True:
      status = (self.getCharStr(), "parameter primitive")
    elif self.isVariableReference() == True:
      status = (self.getCharStr(), "reference primitive")
    elif self.isDirectLambdaKeywordPrimitive() == True:
      status = (self.getCharStr(), "lambda keyword primitive")
    elif self.isDirectLambdaSExpr() == False and self.isSExpr() == True:
      status = "benign sexpr"
    return status
  def getClosestLambdaAncestorInclusive(self):
    return self.closest_lambda_ancestor_inclusive
  def memoizeClosestLambdaAncestorInclusive(self):
    self.closest_lambda_ancestor_inclusive = self.getClosestLambdaAncestor(True)
  def getClosestLambdaAncestorExclusive(self):
    return self.closest_lambda_ancestor_exclusive
  def memoizeClosestLambdaAncestorExclusive(self):
    self.closest_lambda_ancestor_exclusive = self.getClosestLambdaAncestor(False)
  def setPrimitiveForDirectParameterName(self, primitive, name):
    (self.direct_parameter_name_to_primitive_dict)[name] = primitive
  def havePrimitiveForDirectParameterName(self, name):
    return name in (self.direct_parameter_name_to_primitive_dict)
  def getPrimitiveForDirectParameterName(self, name):
    return (self.direct_parameter_name_to_primitive_dict)[name]

class Primitive(Entity):
  def __init__(self, char_str, parent_entity):
    Entity.__init__(self, parent_entity)
    self.char_str = char_str
  def getCharStr(self):
    return self.char_str
  def toString(self, do_translated = False, translation_prefix_str = TRANSLATION_PREFIX_STR):
    if do_translated == True:
      return self.toTranslatedString(translation_prefix_str)
    else:
      return self.getCharStr()
  def toTranslatedString(self, prefix_str = TRANSLATION_PREFIX_STR):
    if self.isDirectLambdaParameterPrimitive() == True:
      return prefix_str + str(self.getParameterID())
    elif self.isVariableReference() == True:
      return prefix_str + str(self.getReferenceID())
    else:
      return self.getCharStr()
  def isPrimitive(self):
    return True
  def toPreorderPrimitiveList(self):
    return [self]
  def toPreorderSExprList(self):
    return []
  def isDirectLambdaParameterPrimitive(self):
    if self.haveParentEntity() == False:
      return False
    is_lambda_keyword_primitive = self.getCharStr() == "lambda"
    is_parameter_primitive = is_lambda_keyword_primitive == False and self.getParentEntity().getParentEntity().isDirectLambdaSExpr() == True and \
      self.getParentEntity().isIthSubexpression(1) == True
    return is_parameter_primitive
  def isVariableReference(self):
    return self.isDirectLambdaSExpr() == False and self.isDirectLambdaParameterPrimitive() == False and self.isDirectLambdaKeywordPrimitive() == False
  def isDirectLambdaKeywordPrimitive(self):
    return self.getCharStr() == "lambda"

class SExpr(Entity):
  def __init__(self, subexpressions, parent_entity):
    Entity.__init__(self, parent_entity)
    self.subexpressions = subexpressions
  def getSubexpressions(self):
    return self.subexpressions
  def getNumSubexpressions(self):
    return len(self.getSubexpressions())
  def setSubexpressions(self, subexpressions):
    self.subexpressions = subexpressions
  # i is 0-indexed
  def getIthSubexpression(self, i):
    if self.getNumSubexpressions() < (i + 1):
      raise Exception()
    else:
      return self.getSubexpressions()[i]
  def toString(self, do_translated = False, translation_prefix_str = TRANSLATION_PREFIX_STR):
    core_str = None
    if self.getNumSubexpressions() == 0:
      core_str = ""
    else:
      core_str = reduce(lambda x, y: x + " " + y, [x.toString(do_translated, translation_prefix_str) for x in self.getSubexpressions()])
    return "(" + core_str + ")"
  def toTranslatedString(self, prefix_str = TRANSLATION_PREFIX_STR):
    return self.toString(True, prefix_str)
  def havePrimitiveOperator(self):
    if self.getNumSubexpressions() > 0:
      operator = self.getSubexpressions()[0]
      if operator.isPrimitive() == True:
        return True
    return False
  def getPrimitiveOperator(self):
    return self.getSubexpressions()[0]
  def isSExpr(self):
    return True
  def isDirectLambdaSExpr(self):
    have_primitive_operator = self.havePrimitiveOperator()
    if have_primitive_operator == True:
      primitive_operator = self.getPrimitiveOperator()
      # changed the following in keeping with DRY
      if primitive_operator.isDirectLambdaKeywordPrimitive() == True:
        return True
    return False
  def getParameterPrimitivesForDirectLambdaSExpr(self):
    if self.isDirectLambdaSExpr() == False:
      raise Exception()
    subexpressions = self.getSubexpressions()
    parameter_sexpr = subexpressions[1]
    parameter_primitives = parameter_sexpr.getSubexpressions()
    return parameter_primitives
  def toPreorderPrimitiveList(self):
    primitive_list = reduce(lambda x, y: x + y, [x.toPreorderPrimitiveList() for x in self.getSubexpressions()], [])
    return primitive_list
  def toPreorderSExprList(self):
    sexpr_list = [self] + reduce(lambda x, y: x + y, [x.toPreorderSExprList() for x in self.getSubexpressions()], [])
    return sexpr_list
  @staticmethod
  def constructUsingNestedTokenLists(nested_token_lists):
    return SExpr.constructUsingNestedTokenListsHelper(nested_token_lists, None)
  @staticmethod
  def constructUsingNestedTokenListsHelper(nested_token_lists, parent_entity):
    if isinstance(nested_token_lists, list) == False:
      char_str = nested_token_lists
      return Primitive(char_str, parent_entity)
    else:
      curr_sexpr = SExpr(None, parent_entity)
      subexpressions = [SExpr.constructUsingNestedTokenListsHelper(x, curr_sexpr) for x in nested_token_lists]
      curr_sexpr.setSubexpressions(subexpressions)
      return curr_sexpr
  @staticmethod
  def preorderNumberDirectLambdaParameterPrimitives(entity, start_ID_value = 1):
    counter = Counter(start_ID_value)
    SExpr.preorderNumberDirectLambdaParameterPrimitivesHelper(entity, counter)
  @staticmethod
  def preorderNumberDirectLambdaParameterPrimitivesHelper(entity, counter):
    if entity.isPrimitive() == True:
      if entity.isDirectLambdaParameterPrimitive() == True:
        entity.setParameterID(counter.getCurrValue())
        counter.increment()
    elif entity.isSExpr() == True:
      for subexpression in entity.getSubexpressions():
        SExpr.preorderNumberDirectLambdaParameterPrimitivesHelper(subexpression, counter)
  @staticmethod
  def memoizeClosestLambdaAncestorsForAllEntities(entity):
    SExpr.memoizeClosestLambdaAncestorsForAllEntitiesHelper(entity)
  @staticmethod
  def memoizeClosestLambdaAncestorsForAllEntitiesHelper(entity):
    entity.memoizeClosestLambdaAncestorInclusive()
    entity.memoizeClosestLambdaAncestorExclusive()
    if entity.isSExpr() == True:
      for subexpression in entity.getSubexpressions():
       SExpr.memoizeClosestLambdaAncestorsForAllEntitiesHelper(subexpression)
  @staticmethod
  def memoizeForLambdasDirectParameterNameToPrimitive(entity):
    SExpr.memoizeForLambdasDirectParameterNameToPrimitiveHelper(entity)
  @staticmethod
  def memoizeForLambdasDirectParameterNameToPrimitiveHelper(entity):
    if entity.isDirectLambdaSExpr() == True:
      parameter_primitive_list = entity.getParameterPrimitivesForDirectLambdaSExpr()
      for parameter_primitive in parameter_primitive_list:
        name = parameter_primitive.getCharStr()
        entity.setPrimitiveForDirectParameterName(parameter_primitive, name)
    if entity.isSExpr() == True:
      for subexpression in entity.getSubexpressions():
        SExpr.memoizeForLambdasDirectParameterNameToPrimitiveHelper(subexpression)
  @staticmethod
  def memoizeReferenceIDsForReferencePrimitives(entity):
    SExpr.memoizeReferenceIDsForReferencePrimitivesHelper(entity)
  @staticmethod
  def memoizeReferenceIDsForReferencePrimitivesHelper(entity):
    if entity.isVariableReference() == True:
      SExpr.memoizeReferenceIDForReferencePrimitive(entity)
    if entity.isSExpr() == True:
      for subexpression in entity.getSubexpressions():
        SExpr.memoizeReferenceIDsForReferencePrimitivesHelper(subexpression)
  @staticmethod
  def memoizeReferenceIDForReferencePrimitive(reference_primitive):
    closest_lambda_sexpr = reference_primitive.getClosestLambdaAncestorInclusive()
    SExpr.memoizeReferenceIDForReferencePrimitiveHelper(reference_primitive, closest_lambda_sexpr)
  @staticmethod
  def memoizeReferenceIDForReferencePrimitiveHelper(reference_primitive, lambda_sexpr):
    if lambda_sexpr == None:
      raise Exception("never found a suitable lambda s.t. parameter exists under desired name")
    else:
      name = reference_primitive.getCharStr()
      have_match = lambda_sexpr.havePrimitiveForDirectParameterName(name)
      if have_match == True:
        # test whether desired name exists as a parameter for this lambda
        parameter_primitive = lambda_sexpr.getPrimitiveForDirectParameterName(name)
        reference_primitive.setReferenceID(parameter_primitive.getParameterID())
      else:
        # otherwise, continue to next-closest lambda
        next_lambda_sexpr = lambda_sexpr.getClosestLambdaAncestorExclusive()
        SExpr.memoizeReferenceIDForReferencePrimitiveHelper(reference_primitive, next_lambda_sexpr)

line = "(lambda (x) (lambda (y) (x (lambda (x) (y x)))))"
# line = "(lambda (x z) (lambda (y) (x (lambda (x) (y x)))))"
# line = "(lambda (x z) ((lambda (y) y) x))"
# line = "(lambda () ())"
# line = "(lambda () (lambda (x) x))"
value = parse(line, OVERALL_RE1, OVERALL_RE2, FIRST, FOLLOW)
root_sexpr = SExpr.constructUsingNestedTokenLists(value)
SExpr.preorderNumberDirectLambdaParameterPrimitives(root_sexpr, 1)
SExpr.memoizeClosestLambdaAncestorsForAllEntities(root_sexpr)
SExpr.memoizeForLambdasDirectParameterNameToPrimitive(root_sexpr)
SExpr.memoizeReferenceIDsForReferencePrimitives(root_sexpr)
print root_sexpr.toTranslatedString()


