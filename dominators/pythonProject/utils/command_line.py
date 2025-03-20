import argparse


def add_config_option(parser: argparse.ArgumentParser):
    parser.add_argument('-C',
                        '--config',
                        help='generate according to the config parameters in this JSON file',
                        metavar='<FILE>.json',
                        required=True)


def add_cfg_options(parser: argparse.ArgumentParser, required: bool = False):
    parser.add_argument('-P',
                        '--program',
                        help='read the CFGs from this JSON file',
                        metavar='<FILE>.json',
                        required=required)

    parser.add_argument('--cfgs',
                        help='only analyse the named CFGs',
                        metavar='<NAME>',
                        type=str,
                        nargs='+')
