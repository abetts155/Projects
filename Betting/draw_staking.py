#!/usr/bin/env python3

import argparse
import sys

assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'


class args:
    previous = None
    profit = None
    odds = None


def calculate():
    target = sum(args.previous) + args.profit
    return target/(args.odds-1.0)


def parse_command_line():
    parser = argparse.ArgumentParser(description='Draw staking calculator')

    parser.add_argument('--previous',
                        type=float,
                        nargs='+',
                        help='all previous stakes',
                        metavar='<STAKE>',
                        required=True)

    parser.add_argument('--profit',
                        type=int,
                        help='the profit sought',
                        metavar='<POUNDS>',
                        required=True)

    parser.add_argument('--odds',
                        type=float,
                        help='the odds of the next draw in decimal',
                        metavar='<VAL>',
                        required=True)

    for arg, value in vars(parser.parse_args()).items():
        assert hasattr(args, arg), ('arg {} not found'.format(arg))
        setattr(args, arg, value)


def main():
    parse_command_line()
    stake = calculate()
    print('You need to place a stake of {:.2f}'.format(stake))
    return 0


if __name__ == '__main__':
    sys.exit(main())
