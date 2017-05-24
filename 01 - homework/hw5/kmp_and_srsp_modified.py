# 2017-04-16

"""

Using first half of Knuth-Morris-Pratt (KMP) pattern-matching 
for shortest repeating sub-pattern (SRSP) determination in O(n) time

Left edge and right edge are "sacred" locations. If we have a repeating sub-pattern that covers the whole input string, it will exist starting at left edge and exist ending at right edge. We always have a repeating pattern, even if it happens to be size n. We never match the whole string with first half of KMP for bulk of the algorithm. We have three cases. For the first case, we have smaller repeating pattern, e.g. with input string "abcabcabc" and smaller repeating sub-pattern "abc", in which case removing max. proper suffix from whole string gives us smallest repeating sub-pattern "abc". For the second case, we have a non-empty normal-prefix and proper-suffix overlap but no smaller repeating sub-pattern, e.g. "abcpppabc" and removing max. proper suffix from whole string gives us "abcppp", but n % leftover_size = 9 % 6 != 0, so the smallest repeating sub-pattern is the whole string "abcpppabc". For the third case, we have an empty normal-prefix and proper-suffix overlap and no smaller repeating sub-pattern, e.g. "abcpppppp" and removing max. proper suffix from whole string gives us "abcpppppp", so the smallest repeating sub-pattern is the whole string "abcpppppp". The key is that the three situations cover the whole space of possible situations and left and right edge are "sacred" locations because they are what the first half of KMP (table-building) work with and if we have a repeating pattern, it exists at the left and right edges.

"""

"""

inspired by buge

"""

# first half of KMP

def KMPFailureFunction(pattern_str):
  i = 1
  j = 0
  m = len(pattern_str)
  f = [0] * m
  while i < m:
    if pattern_str[j] == pattern_str[i]:
      f[i] = j + 1
      i = i + 1
      j = j + 1
    elif j > 0:
      j = f[j - 1]
    else:
      f[i] = 0
      i = i + 1
  return f

# uses first half of KMP
def SRSP(pattern_str):
  if len(pattern_str) == 0:
    return []
  m = len(pattern_str)
  f = KMPFailureFunction(pattern_str)
  proper_suffix_size = f[m - 1]
  left_piece_size = m - proper_suffix_size
  if m % left_piece_size == 0:
    return pattern_str[ : left_piece_size]
  else:
    return pattern_str

# second half of KMP

# retrieve index for beginning of first occurrence of P in T

def KMPMatch(T, P):
  n = len(T)
  m = len(P)
  f = KMPFailureFunction(P)
  i = 0
  j = 0
  while i < n:
    if P[j] == T[i]:
      if j == m - 1:
        return i - m + 1
      i = i + 1
      j = j + 1
    elif j > 0:
      j = f[j - 1]
    else:
      i = i + 1
  raise Exception("no substring of T matching P")

def KMPFailureFunctionWithHeadWildcard(pattern_str, do_head_wildcard = False):
  i = 1
  j = 0
  m = len(pattern_str)
  f = [0] * m
  while i < m:
    if pattern_str[j] == pattern_str[i] or (j == 0 and do_head_wildcard == True):
      f[i] = j + 1
      i = i + 1
      j = j + 1
    elif j > 0:
      j = f[j - 1]
    else:
      f[i] = 0
      i = i + 1
  return f

# uses first half of KMP; 
# returns (srsp with standard head, srsp with special head) tuple
def headWildcardSRSP(pattern_str):
  if len(pattern_str) == 0:
    return []
  m = len(pattern_str)
  f = KMPFailureFunctionWithHeadWildcard(pattern_str, True)
  proper_suffix_size = f[m - 1]
  left_piece_size = m - proper_suffix_size
  if m % left_piece_size == 0:
    """
    special_head = pattern_str[0]
    standard_head = None
    """
    """
    if m == left_piece_size:
      standard_head = special_head
    elif m > left_piece_size:
      standard_head = pattern_str[left_piece_size]
    """
    result_with_standard_head = None
    result_with_special_head = None
    if m == left_piece_size:
      result_with_special_head = pattern_str
      result_with_standard_head = pattern_str
    elif m > left_piece_size:
      result_with_special_head = pattern_str[ : left_piece_size]
      result_with_standard_head = pattern_str[left_piece_size : (left_piece_size * 2)]
      # print "sizes:", m, left_piece_size
      # print "results:", result_with_special_head, result_with_standard_head
    # result_with_standard_head = standard_head + pattern_str[1 : left_piece_size]
    # next_special_head = None if (special_head == standard_head) else special_head
    # result = (result_with_standard_head, next_special_head)
    result = (result_with_standard_head, result_with_special_head)
    return result
    # return pattern_str[ : left_piece_size]
  else:
    # return pattern_str
    result = (pattern_str, pattern_str)
    return result

def headWildcardSRSPExceptWhenSizeLEQTwo(pattern_str):
  n = len(pattern_str)
  if n <= 2:
    result = (pattern_str, pattern_str)
    return result
  elif n >= 3:
    return headWildcardSRSP(pattern_str)

def main():
  print SRSP("abcabcabc")
  print KMPMatch("abacaabaccabacabaabb", "abacab") == 10
  print headWildcardSRSP("abcabc")
  print headWildcardSRSP("cbcabc")
  print headWildcardSRSP("abcabcd")
  print "hello"
  print headWildcardSRSP("ab")
  print headWildcardSRSPExceptWhenSizeLEQTwo("ab")
  print headWildcardSRSPExceptWhenSizeLEQTwo("a")

if __name__ == "__main__":
  main()


