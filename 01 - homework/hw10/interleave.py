# 2018-05-06

# interleave characters from two strings 
# s.t. we totally consume the given strings

# keys are that (1) we skillfully discard based on length, 
# and (2) we handle base-case where one operand is empty correctly

"""
str1 = "test1"
str2 = "test2"
"""

"""
str1 = "abc"
str2 = "defg"
"""

str1 = "456"
str2 = "7890"

def interleave(str1, str2):
  result_set = interleaveHelper(str1, str2, set([""]))
  results = list(result_set)
  return results

def interleaveHelper(str1, str2, result_set):
  if len(str1) == 0 and len(str2) == 0:
    return result_set
  elif len(str1) == 0:
    next_set = set([x + str2 for x in list(result_set)])
    return next_set
  elif len(str2) == 0:
    next_set = set([x + str1 for x in list(result_set)])
    return next_set
  else:
    # case one: eat left-most character from left operand
    result_list1 = list(result_set)
    next_result_list1 = [x + str1[0] for x in result_list1]
    next_result_set1 = set(next_result_list1)
    # case two: eat left-most character from right operand
    result_list2 = list(result_set)
    next_result_list2 = [x + str2[0] for x in result_list2]
    next_result_set2 = set(next_result_list2)
    # handle recursion
    next_next_result_set1 = interleaveHelper(str1[1 : ], str2, next_result_set1)
    next_next_result_set2 = interleaveHelper(str1, str2[1 : ], next_result_set2)
    # combine results
    return next_next_result_set1 | next_next_result_set2

result = interleave(str1, str2)

# print result

# print len(result)

import string

for curr_str in result:
  next_str = string.join(list(curr_str), " ")
  print next_str


