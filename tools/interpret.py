import argparse
import enum
import matplotlib.pyplot as plt
import sys


class State(enum.Enum):
    SIZE = 0
    GCC = 1
    CINS = 2
    XSTUTILS = 3


def next_state(state: State):
    if state == State.SIZE:
        return State.GCC
    if state == State.GCC:
        return state.CINS
    elif state == State.CINS:
        return state.XSTUTILS
    else:
        return state.SIZE


def main(**kwargs):
    state = State.SIZE
    time = 0.0
    count = 0
    values = {k: [] for k in State}
    with open(kwargs['filename'], 'r') as rd:
        for line in rd:
            line = line.strip()
            if line:
                lexemes = line.split()
                if lexemes[0] == State.SIZE.name:
                    size = int(lexemes[1])
                    values[state].append(size)
                    state = next_state(state)
                else:
                    if lexemes[0] == 'user' or lexemes[0] == 'sys':
                        time_string = lexemes[1]
                        minute_marker = time_string.find('m')
                        second_marker = time_string.find('s')
                        minutes = int(time_string[:minute_marker])
                        seconds = float(time_string[minute_marker+1:second_marker])
                        time += (minutes * 60) + seconds
                    if count == 4:
                        values[state].append(time)
                        time = 0.0
                        count = 0
                        state = next_state(state)
                    else:
                        count += 1
    print(values[State.GCC])
    print(values[State.CINS])
    print(values[State.XSTUTILS])
    print(values[State.SIZE])

    plt.plot(values[State.SIZE], values[State.GCC])
    plt.plot(values[State.SIZE], values[State.CINS])
    plt.plot(values[State.SIZE], values[State.XSTUTILS])
    plt.legend([State.GCC.name, State.CINS.name, State.XSTUTILS.name])
    plt.xlabel('#Subprograms')
    plt.ylabel('Time (seconds)')
    plt.show()


def parse_the_command_line():
    parser = argparse.ArgumentParser(description='Interpret and plot results')

    parser.add_argument('--filename',
                        help='read raw numbers from this file',
                        required=True)

    return parser.parse_args()


if __name__ == '__main__':
    assert sys.version_info >= (3, 0), 'Script requires Python 3.0 or greater to run'
    main(**vars(parse_the_command_line()))
