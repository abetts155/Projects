import sys

import datetime
import inspect
import random
import typing


def go_ahead(weight=None):
    if weight:
        return random.random() < weight
    else:
        return bool(random.getrandbits(1))


def pick_element(a_list: typing.List,
                 remove: bool=False):
    element = random.choice(a_list)
    if remove:
        a_list.remove(element)
    return element


def random_8_bit_integer():
    return random.randint(-2 ** 4, 2 ** 4 - 1)


def is_strict_subclass(candidate, base_class):
    return issubclass(candidate, base_class) and candidate != base_class


verbose = False
def verbose_message(*args):
    if verbose:
        print(*args, flush=True)


debug = False


def debug_message(*args):
    if debug:
        caller_frame = inspect.stack()[1]
        info = inspect.getframeinfo(caller_frame[0])
        print('[{:%H:%M:%S}: {}@{}]'.format(datetime.datetime.now(), info.function, info.lineno),
              *args,
              flush=True)


def error_message(*args):
    print('ERROR:', *args, file=sys.stderr, flush=True)
    sys.exit(1)
