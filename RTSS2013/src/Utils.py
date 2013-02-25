from itertools import tee, islice, izip_longest

def enum (*sequential, **named):
    enums = dict(zip(sequential, range(len(sequential))), **named)
    reverse = dict((value, key) for key, value in enums.iteritems())
    forward = dict((key, value) for key, value in enums.iteritems())
    enums['reverse_mapping'] = reverse
    enums['forward_mapping'] = forward
    return type('Enum', (), enums)

def peekaheadIterator (iterable, window=1):
    items, nexts = tee(iterable, 2)
    nexts = islice(nexts, window, None)
    return izip_longest(items, nexts)