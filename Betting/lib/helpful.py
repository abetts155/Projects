from typing import List


def split_into_contiguous_groups(integers: List[int]) -> List[List[int]]:
    integers.sort()
    sublists = [[integers[0]]]
    for i in integers[1:]:
        current = sublists[-1]
        if i - current[-1] == 1:
            current.append(i)
        else:
            sublists.append([i])
    return sublists


def to_string(sublists: List[List[int]]) -> str:
    label = ''
    for sublist in sublists:
        if len(sublist) == 1:
            label = '{}{}{}'.format(label, ',' if label else '', sublist[0])
        else:
            label = '{}{}{}-{}'.format(label, ',' if label else '', sublist[0], sublist[-1])
    return label
