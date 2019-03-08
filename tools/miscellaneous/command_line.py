import argparse
import random
import sys

from ast import (annotations, ast)
from miscellaneous import helpful


def is_arithmetic_and_binary(a_class):
    return issubclass(a_class, ast.ArithmeticOperator) and issubclass(a_class, ast.BinaryOperator)


class CheckForNonnegativeValue(argparse.Action):
    def __call__(self, parser, namespace, value, option_string=None):
        if value < 0:
            raise argparse.ArgumentError(self, 'the parameter must be a non-negative number')
        setattr(namespace, self.dest, value)


class CheckForPositiveValue(argparse.Action):
    def __call__(self, parser, namespace, value, option_string=None):
        if value <= 0:
            raise argparse.ArgumentError(self, 'the parameter must be a positive number')
        setattr(namespace, self.dest, value)


def parse():
    parser = argparse.ArgumentParser(description='Generate a C or Ada program',
                                     formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    parser.add_argument('--filename',
                        help='write into this file',
                        metavar='<FILE>')

    parser.add_argument('--language',
                        choices=[language.name.lower() for language in ast.Language],
                        help='output language',
                        type=str.lower,
                        required=True)

    parser.add_argument('--analysis',
                        choices=[analysis.name.lower() for analysis in annotations.Analysis],
                        help='the type of program analysis',
                        type=str.lower,
                        required=True)

    parser.add_argument('--subprograms',
                        type=int,
                        action=CheckForPositiveValue,
                        help='number of subprograms',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('--default-instrumentation',
                        choices=[profile.name.lower() for profile in annotations.InstrumentationProfile
                                 if profile != annotations.InstrumentationProfile.DEFAULT],
                        help='choose the default instrumentation profile',
                        type=str.lower,
                        default=None)

    parser.add_argument('--no-instrumentation-override',
                        action='store_true',
                        help='use the default instrumentation profile for all subprograms',
                        default=False)

    parser.add_argument('--formal-parameter-limit',
                        type=int,
                        action=CheckForPositiveValue,
                        help='maximum number of formal parameters in a subprogram',
                        default=10,
                        metavar='<INT>')

    parser.add_argument('--expression-depth',
                        type=int,
                        action=CheckForPositiveValue,
                        help='maximum number of operations in an expression',
                        default=1,
                        metavar='<INT>')

    parser.add_argument('--ignore-operators',
                        choices=[a_class.__name__ for a_class in ast.get_operators(is_arithmetic_and_binary)],
                        help='do not use these binary arithmetic operators in expressions',
                        nargs='+',
                        type=str.title,
                        default=[])

    parser.add_argument('--block-length',
                        type=int,
                        action=CheckForPositiveValue,
                        help='maximum number of statements in a basic block (excluding nested basic blocks)',
                        default=5,
                        metavar='<INT>')

    parser.add_argument('--basic-blocks',
                        type=int,
                        action=CheckForPositiveValue,
                        help='maximum number of basic blocks in a subprograms',
                        default=10,
                        metavar='<INT>')

    parser.add_argument('--loops',
                        type=int,
                        action=CheckForNonnegativeValue,
                        help='maximum number of loops in a subprogram',
                        metavar='<INT>',
                        default=0)

    parser.add_argument('--loop-depth',
                        type=int,
                        action=CheckForPositiveValue,
                        help='maximum nesting depth of loops',
                        metavar='<INT>',
                        default=sys.maxsize)

    parser.add_argument('--call-depth',
                        type=int,
                        action=CheckForPositiveValue,
                        help='maximum nesting depth of calls',
                        metavar='<INT>',
                        default=sys.maxsize)

    parser.add_argument('--allow-recursion',
                        action='store_true',
                        help='allow calls to be recursive',
                        default=False)

    return vars(parser.parse_args())


def check_arguments(args):
    def is_valid_instrumentation_profile_for_analysis():
        if args['analysis'] == annotations.Analysis.COVERAGE:
            return args['default_instrumentation'] in annotations.InstrumentationProfile.coverage_profiles()
        else:
            return args['default_instrumentation'] in annotations.InstrumentationProfile.timing_profiles()

    if args['basic_blocks'] < args['loops'] * 2:
        helpful.error_message('The number of basic blocks in a subprogram must be at least twice the number of loops')

    if len(args['ignore_operators']) == len(ast.get_operators(is_arithmetic_and_binary)):
        helpful.error_message('All binary arithmetic operators are ignored: The program generator needs at least one to'
                              ' create expressions')

    if args['default_instrumentation']:
        if not is_valid_instrumentation_profile_for_analysis():
            helpful.error_message("Invalid default instrumentation profile '{}' "
                                  "for analysis '{}'".format(args['default_instrumentation'].name,
                                                             args['analysis'].name))

    return args


def standardise_arguments(args):
    # The raw argument is a list, but we really want a set.
    ignore_operators = set(args['ignore_operators'])
    # Create an operator class for each operator name.
    args['ignore_operators'] = [getattr(sys.modules[ast.__name__], op) for op in ignore_operators]
    args['language'] = ast.Language[args['language'].upper()]
    args['analysis'] = annotations.Analysis[args['analysis'].upper()]

    if args['default_instrumentation']:
        args['default_instrumentation'] = annotations.InstrumentationProfile[args['default_instrumentation'].upper()]
    else:
        # A default instrumentation profile is required by RVS.
        if args['analysis'] == annotations.Analysis.TIMING:
            choices = [profile for profile in annotations.InstrumentationProfile.timing_profiles()
                       if profile != annotations.InstrumentationProfile.DEFAULT]
            args['default_instrumentation'] = random.choice(choices)
        else:
            choices = [profile for profile in annotations.InstrumentationProfile.coverage_profiles()
                       if profile != annotations.InstrumentationProfile.DEFAULT]
            args['default_instrumentation'] = random.choice(choices)
