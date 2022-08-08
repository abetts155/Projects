from argparse import ArgumentParser
from random import choice
from typing import Dict, List


def generate(length: int, minimum: int, maximum: int) -> List[int]:
    options = [k for k in range(minimum, maximum + 1)]
    data = []
    for _ in range(length):
        picked = choice(options)
        data.append(picked)
        options.remove(picked)
    return data


class Vertex:
    __slots__ = ['value', 'left', 'right']

    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None


def print_graph(low: Vertex, mid: Vertex):
    print('-' * 80)
    crawler = low
    while crawler:
        if mid == crawler:
            print('|{}|'.format(crawler.value), end=' ')
        else:
            print(crawler.value, end=' ')
        crawler = crawler.right
    print()
    print('-' * 80)
    print()


def sort(data: List[int]):
    low, mid, high = Vertex(data[0]), Vertex(data[1]), Vertex(data[2])
    low.right = mid
    mid.left = low
    mid.right = high
    high.left = mid

    left_size = 1
    right_size = 1

    print_graph(low, mid)

    for value in data[3:]:
        vertex = Vertex(value)

        if vertex.value < low.value:
            low.left = vertex
            vertex.right = low
            low = vertex
            left_size += 1
        elif vertex.value > high.value:
            vertex.left = high
            high.right = vertex
            high = vertex
            right_size += 1
        else:
            mid_distance = abs(value - mid.value)
            if value < mid.value:
                low_distance = abs(value - low.value)

                if low_distance < mid_distance:
                    other = low.right
                    other.left = vertex
                    vertex.right = other
                    vertex.left = low
                    low.right = vertex
                else:
                    other = mid.left
                    other.right = vertex
                    vertex.left = other
                    vertex.right = mid
                    mid.left = vertex

                left_size += 1
            else:
                high_distance = abs(value - high.value)

                if mid_distance < high_distance:
                    other = mid.right
                    other.left = vertex
                    vertex.right = other
                    vertex.left = mid
                    mid.right = vertex
                else:
                    other = high.left
                    other.right = vertex
                    vertex.left = other
                    vertex.right = high
                    high.left = vertex

                right_size += 1

        if abs(left_size - right_size) > 1:
            if left_size > right_size:
                mid = mid.left
                left_size -= 1
                right_size += 1
            else:
                mid = mid.right
                left_size += 1
                right_size -= 1

        print_graph(low, mid)

    done = True
    vertex = low
    data.clear()
    while vertex:
        data.append(vertex.value)
        if vertex.right and vertex.value > vertex.right.value:
            done = False
        vertex = vertex.right

    print(done)


def main(data: List[int], length: int, minimum: int, maximum: int):
    if not data:
        data = generate(length, minimum, maximum)

    print('In:  {}'.format('  '.join(str(x) for x in data)))

    if len(data) >= 3:
        a, b, c, *rest = data

        if a > b:
            a, b = b, a

        if c < a:
            data[0], data[1], data[2] = c, a, b
        elif c > b:
            data[0], data[1], data[2] = a, b, c
        else:
            data[0], data[1], data[2] = a, c, b

        sort(data)

    elif len(data) == 2:
        if data[0] > data[1]:
            data[1], data[0] = data[0], data[1]

    print('Out: {}'.format('  '.join(str(x) for x in data)))


def parse_command_line():
    parser = ArgumentParser(description='Sort an array of numbers')

    parser.add_argument('-L',
                        '--length',
                        type=int,
                        help='length of array to sort',
                        default=10,
                        metavar='<INT>')

    parser.add_argument('--min',
                        type=int,
                        help='minimum allowed integer in the array',
                        default=0,
                        metavar='<INT>')

    parser.add_argument('--max',
                        type=int,
                        help='maximum allowed integer in the array',
                        default=100,
                        metavar='<INT>')

    parser.add_argument('-A',
                        '--array',
                        type=int,
                        help='sort this array',
                        nargs='+')

    return parser.parse_args()


if __name__ == '__main__':
    args = parse_command_line()
    main(args.array, args.length, args.min, args.max)
