"""

# leave out leading offset

# non-offset items are lists, impulses, ellipsis

# for example, have for pairup "((1 2) (3 4) ...)", 
# but this means at top level we have O {L O L E}, 
# for first and second sublists we have O {I O I}

# specifically, we have for first sub-list:
# O {I O I} -> 1 {I 1 I}

# specifically, we have for second sub-list:
# O {I O I} -> 3 {I 1 I}

# specifically, we have for top-level list:
# O {L O L E} -> 1 {{I 1 I} 2 {I 1 I} E}

# offsets are based on left-most index for left neighbor

pairup_primary_pattern_str = "((1 2) (3 4) ...)"

# leave out left-most offset for memoization

# only include left-most offset at very top

# we are memoizing in a way that is is not susceptible to left-most offset distribution ambiguity 
# and maximizes re-use for shortest repeating sub-pattern detection

# only at top-most level do we possibly have a left-most offset, which is kept separately

"""

"""

"don't stop until you fail"

with n_E = 1, time is at worst quadratic in n_wl * n_{e, post-collapse}

with n_E = 0, time is at worst linear in n_wl

with n_E >= 2, time is possibly super-quadratic in n_wl * n_{e, post-collapse}

note that we are assuming delta is != 0, in which case we could have infinite output index mask size

"""

"""

n_s = O((n_{e, post-collapse} * n_wl / n_E) ^ n_E + n_wl)

n_s is size of output index mask in terms of number of scalars

n_{e, post-collapse} is number of elements after finding badge SRSP's in pattern (e.g. number of lists, ellipses, scalars)

n_wl is size of input word list

n_E is number of ellipses

"""

"""

sample, explosion, concavity

sampling - the larger n_{e, post-collapse}, the less instances we can get w.r.t. n_wl, but the larger each instance is

explosion - ellipses vertically stacked leads to maximal amplification

concavity - maximal amplification comes when ellipsis shallow element counts (which upper bounds number of sub-lists at a particular list) are equal, i.e. number of shallow element counts are (due to sampling) the product of max. shallow element count for if we are flat divided by number of ellipses

when number of ellipses is zero, we have a trivial upper bound of not O(1), but n_wl

"""

items = ["the", "rain", "in", "spain", "stays", "mainly", "on", "the", "plain"]

# [[1, 2], [3, 4], [5, 6] [7, 8]]
pairup_primary_pattern_str = "((1 2) (3 4) ...)"

# [[1, 2], [2, 3], [3, 4], [4, 5], [5, 6], [6, 7], [7, 8], [8, 9]]
overlap_primary_pattern_str = "((1 2) (2 3) ...)"

# [[1, 2, 3, 4, 5, 6, 7, 8, 9]
# [2, 3, 4, 5, 6, 7, 8, 9]
# [3, 4, 5, 6, 7, 8, 9]
# [4, 5, 6, 7, 8, 9]
# [5, 6, 7, 8, 9]
# [6, 7, 8, 9]
# [7, 8, 9]
# [8, 9]
# [9]]
tails_primary_pattern_str = "((1 2 ...) (2 3 ...) ...)"

tails2_primary_pattern_str = "((1 ...) ...)"

# [2, 1, 4, 3, 6, 5, 8, 7]
# NOTE - differences don't agree, even if there is a pattern (of size two); 
# treat 2 1 4 3 as one big example; 8 is divisible by both four and two
swap_primary_pattern_str = "(2 1 4 3 ...)"

# [[1, 3, 5, 7, 9], [2, 4, 5, 6, 8]]
split_primary_pattern_str = "((1 3 ...) (2 4 ...))"

import re
import string
import math

from collections import defaultdict, deque

from basic_s_expression_parser import *
from kmp_and_srsp_modified import *

def flatten(curr_list):
  if type(curr_list) != list:
    return [curr_list]
  if len(curr_list) == 0:
    return []
  elif len(curr_list) == 1:
    return flatten(curr_list[0])
  else:
    return flatten(curr_list[0]) + flatten(curr_list[1 : ])

# print flatten(4)

# print flatten([1, [2, 3, [4]], 5])

def listEndsInEllipsis(curr_list):
  if len(curr_list) == 0:
    return False
  elif curr_list[len(curr_list) - 1] == "...":
    return True
  else:
    return False

# print listEndsInEllipsis([1, 2, 3, "..."])

def getFirstScalar(curr_list):
  candidate_values = [x for x in curr_list if type(x) == int]
  result = None
  if len(candidate_values) == 0:
    result = None
  else:
    result = candidate_values[0]
  return result

# print getFirstScalar(flatten([1, [2, 3, [4, "..."]], 5, "..."]))

class Pattern:
  def __init__(self, parent_pattern):
    self.parent_pattern = parent_pattern
  def getParentPattern(self):
    return self.parent_pattern
  @staticmethod
  def listifiedPatternStr(pattern_str):
    value = parse(pattern_str, OVERALL_RE, FIRST, FOLLOW)
    return value
  # return is (root pattern, top left-most offset) tuple; 
  # root pattern can be None and top left-most offset can be None; 
  # LOP stands for list-offset-pattern
  @staticmethod
  def toLOPForm(listified_pattern):
    flattened_listified_pattern = flatten(listified_pattern)
    if len(flattened_listified_pattern) == 0:
      return (None, None)
    else:
      top_leftmost_offset = flattened_listified_pattern[0]
      result = Pattern.toLOPFormHelper(listified_pattern, top_leftmost_offset, None)
      next_result = (result, top_leftmost_offset)
      return next_result
  @staticmethod
  def toLOPFormHelper(listified_pattern, base_index, parent_pattern = None):
    if type(listified_pattern) == list:
      ends_in_ellipsis = listEndsInEllipsis(listified_pattern)
      pattern = ListPattern(parent_pattern, None, ends_in_ellipsis)
      subpatterns = []
      # offset is based on left-most value and left neighbor left-most value; 
      # each new level gives different ways by which to increase base index
      prev_base_index = None
      curr_base_index = base_index
      num_indices = len(listified_pattern) - (1 if ends_in_ellipsis == True else 0)
      for i in xrange(num_indices):
        curr_listified_pattern = listified_pattern[i]
        if curr_listified_pattern == "...":
          continue
        curr_pattern1 = Pattern.toLOPFormHelper(curr_listified_pattern, curr_base_index, pattern)
        subpatterns.append(curr_pattern1)
        if i != num_indices - 1:
          prev_base_index = curr_base_index
          curr_base_index = flatten(listified_pattern[i + 1])[0]
          curr_offset = curr_base_index - prev_base_index
          curr_pattern2 = OffsetPattern(pattern, curr_offset)
          subpatterns.append(curr_pattern2)
      pattern.setSubpatterns(subpatterns)
    elif type(listified_pattern) == int:
      pattern = ImpulsePattern(parent_pattern)
    return pattern
  def toListified(self):
    pass
  def toString(self):
    pass
  def isListPattern(self):
    return False
  def isOffsetPattern(self):
    return False
  def isImpulsePattern(self):
    return False

class ListPattern(Pattern):
  def __init__(self, parent_pattern, subpatterns, is_expandable):
    Pattern.__init__(self, parent_pattern)
    self.subpatterns = subpatterns
    self.is_expandable = is_expandable
  def setSubpatterns(self, subpatterns):
    self.subpatterns = subpatterns
  def getSubpatterns(self):
    return self.subpatterns
  def isExpandable(self):
    return self.is_expandable
  def toListified(self):
    result = reduce(lambda x, y: x + y, [[x.toListified()] for x in self.getSubpatterns()], [])
    return result
  def toString(self):
    return "(LP-{" + ("T" if (self.isExpandable() == True) else "F") + "} " + string.join([x.toString() for x in self.getSubpatterns()], " ") + ")"
  def isListPattern(self):
    return True
  def toIsomorphicCollapsedBadges(self):
    # badge-tuple-internal-offset-value-is-expandable-tuple to badge dictionary
    btioviettbd = {}
    # offset value to offset badge dictionary
    ovtobd = {}
    return self.exploreTICB(btioviettbd, ovtobd)
  # return an ordered list of badges
  def exploreTICB(self, btioviettbd, ovtobd):
    subpatterns = self.getSubpatterns()
    subbadges = []
    for subpattern in subpatterns:
      subbadge_list = subpattern.exploreTICB(btioviettbd, ovtobd)
      subbadges.extend(subbadge_list)
    badges = self.postVisitTICB(btioviettbd, ovtobd, subbadges)
    return badges
  # return an ordered list of badges
  def postVisitTICB(self, btioviettbd, ovtobd, subbadges):
    # have not yet collapsed
    result = headWildcardSRSPExceptWhenSizeLEQTwo([None] + subbadges)
    result_with_standard_head, result_with_special_head = result
    # assume list is never empty; 
    # special pattern is without an initial offset
    next_subbadges = None
    internal_offset_value = None
    if self.isExpandable() == False:
      next_subbadges = subbadges
    else:
      """
      standard_head = result_with_standard_head[1]
      special_head = subbadges[0]
      # print "heads:", standard_head, special_head
      """
      internal_offset_badge = result_with_standard_head[0]
      # if we have too few examples, offset is one
      if internal_offset_badge == None:
        internal_offset_value = 1
      else:
        internal_offset_value = internal_offset_badge.getSize()
      offsetless_pattern = result_with_special_head[1 : ]
      next_subbadges = offsetless_pattern
    # badge_tuple = tuple(subbadges)
    badge_tuple = tuple(next_subbadges)
    btioviet = (badge_tuple, internal_offset_value, self.isExpandable())
    badge = None
    if btioviet in btioviettbd:
      badge = btioviettbd[btioviet]
    else:
      badge = ListBadge(next_subbadges, internal_offset_value, self.isExpandable())
      btioviettbd[btioviet] = badge
    return [badge]

# note that delta does not by itself determine the absolute value of a scalar described by a scalar pattern
class OffsetPattern(Pattern):
  def __init__(self, parent_pattern, size):
    Pattern.__init__(self, parent_pattern)
    self.size = size
  def getSize(self):
    return self.size
  def toListified(self):
    return self.toString()
  def toString(self):
    return str(self.getSize())
  def isOffsetPattern(self):
    return True
  def toIsomorphicCollapsedBadges(self):
    pass
  # return an ordered list of badges
  def exploreTICB(self, btioviettbd, ovtobd):
    badges = self.postVisitTICB(btioviettbd, ovtobd, [])
    return badges
  # return an ordered list of badges
  def postVisitTICB(self, btioviettbd, ovtobd, subbadges):
    offset_value = self.getSize()
    badge = None
    if offset_value in ovtobd:
      badge = ovtobd[offset_value]
    else:
      badge = OffsetBadge(offset_value)
      ovtobd[offset_value] = badge
    return [badge]

class ImpulsePattern(Pattern):
  def __init__(self, parent_pattern):
    Pattern.__init__(self, parent_pattern)
  def toListified(self):
    return self.toString()
  def toString(self):
    return "I"
  def isImpulsePattern(self):
    return False
  def toIsomorphicCollapsedBadges(self):
    pass
  # return an ordered list of badges
  def exploreTICB(self, btioviettbd, ovtobd):
    badges = self.postVisitTICB(btioviettbd, ovtobd, [])
    return badges
  # return an ordered list of badges
  def postVisitTICB(self, btioviettbd, ovtobd, subbadges):
    badge = IMPULSE_BADGE
    return [badge]

class Badge:
  def __init__(self):
    pass
  def toString(self):
    return None
  def isListBadge(self):
    return False
  def isOffsetBadge(self):
    return False
  def isImpulseBadge(self):
    return False

class ListBadge(Badge):
  def __init__(self, subbadge_repeating_unit_list, internal_offset_value, is_expandable):
    Badge.__init__(self)
    self.subbadge_repeating_unit_list = subbadge_repeating_unit_list
    self.internal_offset_value = internal_offset_value
    self.is_expandable = is_expandable
  def getSubbadgeRUL(self):
    return self.subbadge_repeating_unit_list
  def getInternalOffsetValue(self):
    return self.internal_offset_value
  def isExpandable(self):
    return self.is_expandable
  def toString(self):
    return "(LB-{" + str(self.getInternalOffsetValue()) + "}-{" + ("T" if (self.isExpandable() == True) else "F") + "} " + string.join([x.toString() for x in self.getSubbadgeRUL()], " ") + ")"
  def toMaskIndexList(self, max_allowed_index, offset):
    curr_offset = offset
    subbadge_rul = self.getSubbadgeRUL()
    internal_offset_value = self.getInternalOffsetValue()
    is_expandable = self.isExpandable()
    """
    if is_expandable == True:
      print "internal offset value:", internal_offset_value
    """
    num_instances_seen = 0
    curr_result = []
    while True:
      curr_instance = []
      for element_badge in subbadge_rul:
        mask, is_successful = element_badge.toMaskIndexList(max_allowed_index, curr_offset)
        if is_successful == True:
          if element_badge.isOffsetBadge() == True:
            curr_offset += element_badge.getSize()
          else:
            if mask != None:
              curr_instance.append(mask)
        elif is_successful == False:
          if num_instances_seen >= 1:
            return curr_result, True
          else:
            return None, False
      curr_result.extend(curr_instance)
      num_instances_seen += 1
      if is_expandable == True:
        curr_offset += internal_offset_value
      else:
        return curr_result, True
    return [], True
  def isListBadge(self):
    return True

class OffsetBadge(Badge):
  def __init__(self, size):
    Badge.__init__(self)
    self.size = size
  def getSize(self):
    return self.size
  def toString(self):
    return str(self.getSize())
  def toMaskIndexList(self, max_allowed_index, offset):
    return [], True
  def isOffsetBadge(self):
    return True

class ImpulseBadge(Badge):
  def __init__(self):
    Badge.__init__(self)
  def toString(self):
    return "I"
  def toMaskIndexList(self, max_allowed_index, offset):
    if offset > max_allowed_index or offset < 1:
      return None, False
    else:
      return offset, True
  def isImpulseBadge(self):
    return True

IMPULSE_BADGE = ImpulseBadge()

def convertNestedListsToNestedDeques(value):
  return convertNestedListsToNestedDequesHelper(value)

def convertNestedListsToNestedDequesHelper(value):
  if type(value) != list:
    return value
  elif type(value) == list:
    values = [convertNestedListsToNestedDequesHelper(x) for x in value]
    next_values = deque(values)
    return next_values

# print convertNestedListsToNestedDeques([[1, 2], 3])

def applyIndexMaskToItemList(index_mask, item_list):
  deque_index_mask = convertNestedListsToNestedDeques(index_mask)
  return applyIndexMaskToItemListHelper(deque_index_mask, item_list)

# is destructive w.r.t. deque_index_mask
def applyIndexMaskToItemListHelper(deque_index_mask, item_list):
  if type(deque_index_mask) == int:
    return item_list[deque_index_mask - 1]
  elif type(deque_index_mask) == deque:
    if len(deque_index_mask) == 0:
      return []
    else:
      curr_mask = deque_index_mask[0]
      result1 = applyIndexMaskToItemListHelper(curr_mask, item_list)
      deque_index_mask.popleft()
      rest_of_mask = deque_index_mask
      result2 = applyIndexMaskToItemListHelper(rest_of_mask, item_list)
      return [result1] + result2

"""

result = Pattern.listifiedPatternStr("((1 3 ...) (2 4 ...))")

next_result = Pattern.toLOPForm(result)

# print next_result

next_next_result = next_result

root_pattern, leftmost_offset = next_next_result

# print leftmost_offset

# print root_pattern.toString()

# print root_pattern.getSubpatterns()

# print "subpattern subpatterns:", [x.getSubpatterns() for x in root_pattern.getSubpatterns() if x.isOffsetPattern() == False]

# print root_pattern.toListified()

badges = root_pattern.toIsomorphicCollapsedBadges()

root_badge = badges[0]

# print root_pattern.toString()

# print root_badge.toString()

"""

pattern_str_list = [pairup_primary_pattern_str, overlap_primary_pattern_str, tails_primary_pattern_str, tails2_primary_pattern_str, swap_primary_pattern_str, split_primary_pattern_str]

for pattern_str in pattern_str_list:
  listified_pattern = Pattern.listifiedPatternStr(pattern_str)
  root_pattern, leftmost_offset = Pattern.toLOPForm(listified_pattern)
  badges = root_pattern.toIsomorphicCollapsedBadges()
  root_badge = badges[0]
  # print root_pattern.toString()
  print leftmost_offset, root_badge.toString()

  max_allowed_index = len(items)
  index_mask, is_successful = root_badge.toMaskIndexList(max_allowed_index, leftmost_offset)
  print index_mask

  words_regrouped = applyIndexMaskToItemList(index_mask, items)

  print words_regrouped

  print


