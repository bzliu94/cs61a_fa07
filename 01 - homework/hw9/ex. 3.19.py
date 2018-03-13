# 2018-03-09

# we deviate from wikipedia article assumptions; 
# before, we assume we have n nodes and n edges; 
# this means that we always have a cycle; 
# however, the sicp problem states that we care about 
# cycles reachable via starting at head pointer 
# and following next pointers via cdr; 
# this means we don't care about cycles 
# not reachable from head and cycle might not exist; 
# to address possibility of cycle not existing, 
# we know ignoring first step (s.t. hare and tortoise are at head) 
# that if there is no cycle, tortoise never catches up to hare; 
# so if hare reaches a node s.t. next pointer is null, 
# then there is no cycle; then, for first step, 
# if next pointer is null, again there is no cycle; 
# so for every step we always check if hare is at node 
# s.t. next pointer is null and if it is, 
# there is no cycle reachable from head

# time is O(lambda + mu) = O(n); 
# space is O(1)

# inspired by wikipedia article on cycle detection

def floyd(f_list, x0):

    def f(x):
      return f_list[x]

    # if we ever reach a node s.t. next pointer is None, 
    # we return -1
    def double_f_with_last_node_detection(x):
      next_node = f(x)
      if next_node == None:
        return -1
      next_next_node = f(next_node)
      if next_next_node == None:
        return -1
      return next_next_node

    if len(f_list) == 0:
      raise Exception()

    # phase #1 - get hare and tortoise to meet
    tortoise = f(x0)
    hare = double_f_with_last_node_detection(x0)
    if hare == -1:
      return (None, None)
    while tortoise != hare:
        tortoise = f(tortoise)
        hare = double_f_with_last_node_detection(hare)
        if hare == -1:
          return (None, None)
  
    # phase #2 - get mu  
    mu = 0
    tortoise = x0
    while tortoise != hare:
        tortoise = f(tortoise)
        hare = f(hare)
        mu += 1
 
    # phase #3 - get lambda
    lam = 1
    hare = f(tortoise)
    while tortoise != hare:
        hare = f(hare)
        lam += 1
 
    # lambda is loop length, mu is start index of loop

    return lam, mu

print floyd([1, 0, 5, 0, 0, None], 0)
print floyd([1, 2, 3, 4, None], 0)
print floyd([6, 6, 0, 1, 4, 3, 3, 4, 0], 0)
print floyd([5, 0, 0, 0, 0, 0], 0)
print floyd([1, 0, 5, 0, 0, None], 0)
print floyd([0], 0)
print floyd([None, 1], 0)
print floyd([None], 0)
# print floyd([], 0)


