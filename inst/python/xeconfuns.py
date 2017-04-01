from math import floor
import collections

# some python implementations of R functions
# commonly used in XEconDB game specifications
# to facility conversion to otree 

def ifelse(test, val1, val2):
	if test:
		return val1
	return val2

def cases(*args):
	num_cond = floor(len(args) / 2)
	
	for i in range(num_cond):
		cond_ind = i*2
		if args[cond_ind]:
			return args[cond_ind+1]
	if len(args) > num_cond*2:
		return args[len(args)]

def setdiff(x,y):
	xs = set(as_iterable(x))
	ys = set(as_iterable(y))
	return list(xs.difference(ys))

def c(*args):
	list(args)

def as_iterable(x):
	if isinstance(x, collections.Iterable):
		return x
	return [x]

