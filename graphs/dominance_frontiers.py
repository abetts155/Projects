import argparse
import logging
import pathlib
import sys
import threading
import cProfile
import pstats
import io

from rich.logging import RichHandler

import graphs.dominators
import graphs.graph_input
import graphs.graphs
import graphs.vertices
import utils.command_line

logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s:%(asctime)s:%(message)s",
    datefmt="%H:%M:%S",
    handlers=[RichHandler()]
)
logger = logging.getLogger(__name__)


def parse_the_command_line() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Do dominance frontier analysis')
    utils.command_line.add_cfg_options(parser, True)
    args = parser.parse_args()
    return args


def main(file: pathlib.Path, name_filter: list[str]):
    cfgs = graphs.graph_input.read_cfgs(file, name_filter)

    for cfg in cfgs.values():
        print(cfg.name)
        cfg.dotify(f"{cfg.name}")
        direction = graphs.graphs.GraphDirection.Forwards
        pr = cProfile.Profile()
        pr.enable()
        betts_tree = graphs.dominators.BettsBreadthFirst(cfg, direction)
        betts_tree.dotify(f"{cfg.name}.betts")
        pr.disable()

        s = io.StringIO()
        ps = pstats.Stats(pr, stream=s).sort_stats('cumulative')
        ps.print_stats(5)
        print(s.getvalue())


        dominator_tree = graphs.dominators.LengauerTarjan(cfg, direction)
        assert set(betts_tree.its_edges) == set(dominator_tree.its_edges)

        #df = graphs.dominators.DominanceFrontiers(cfg, dominator_tree, direction)
        #betts_df = graphs.dominators.BettsDominanceFrontiers(cfg, direction)


if __name__ == '__main__':
    threading.stack_size(2 ** 26)
    sys.setrecursionlimit(2 ** 30)
    args = parse_the_command_line()
    main(args.program, args.cfgs)
