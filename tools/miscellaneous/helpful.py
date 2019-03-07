import random
import sys
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


def error_message(*args):
    print('ERROR:', *args, file=sys.stderr, flush=True)
    sys.exit(1)


def is_strict_subclass(candidate, base_class):
    return issubclass(candidate, base_class) and candidate != base_class


def blanks(length):
    return ' ' * length


def newlines(length=1):
    return '\n' * length
