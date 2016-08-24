import sys

from tools.lib.env import config

def verbose_message(string, module):
    if config.Arguments.verbose:
        print("[%s] %s" % (module, string), file=sys.stderr)

        
def debug_message (string, module, debug_level=1):
    if config.Arguments.debug > 0 and debug_level <= config.Arguments.debug:
        print("[%s] %s" % (module, string), file=sys.stderr)

        
def warning_message(string):
    print("[*****WARNING*****] %s" % string, file=sys.stderr)


def exit_message(string):
    print(string, file=sys.stderr)
    sys.exit(0)