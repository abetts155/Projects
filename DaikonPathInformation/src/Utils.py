import Calculations

m5Directory = 'm5out'

def clean ():
    import Debug
    import shutil, os
    for paths, dirs, files in os.walk(os.path.abspath(os.curdir)):
        files.sort()
        for filename in files:
            ext = os.path.splitext(filename)[1]
            if ext == '.udraw' or ext == '.dis' or ext in Calculations.ECLIPSE.fileExtensions:
                fullPath = os.path.join(paths, filename)
                Debug.verboseMessage("Removing '%s'" % fullPath)
                os.remove(fullPath)
            if os.access(filename, os.X_OK) and os.path.exists(filename + '.c'):
                fullPath = os.path.join(paths, filename)
                Debug.verboseMessage("Removing executable '%s'" % fullPath)
                os.remove(fullPath)
        for directory in dirs:
            if directory == m5Directory:
                fullPath = os.path.join(paths, directory)
                Debug.verboseMessage("Removing '%s'" % fullPath)
                shutil.rmtree(fullPath)

def enum (*sequential, **named):
    enums = dict(zip(sequential, range(len(sequential))), **named)
    reverse = dict((value, key) for key, value in enums.iteritems())
    forward = dict((key, value) for key, value in enums.iteritems())
    enums['reverse_mapping'] = reverse
    enums['forward_mapping'] = forward
    return type('Enum', (), enums)

def peekaheadIterator (iterable, window=1):
    from itertools import tee, islice, izip_longest
    items, nexts = tee(iterable, 2)
    nexts = islice(nexts, window, None)
    return izip_longest(items, nexts)