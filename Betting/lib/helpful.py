from math import ceil
from typing import List, Tuple


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


class DisplayGrid:
    def __init__(self, cells: int, ncols: int = 3):
        self._ncols = ncols
        self._nrows = 1
        self._cell_to_indices = {}
        self._calculate_dims(cells)
        self._calculate_indices()

    def _calculate_dims(self, cells: int):
        if cells > 1:
            if cells <= self._ncols:
                self._nrows = 1
            else:
                self._nrows = ceil(cells / self._ncols)

    def _calculate_indices(self):
        row_id = 0
        col_id = 0
        for i in range(self.nrows * self.ncols):
            self._cell_to_indices[i] = (row_id, col_id)
            if col_id == self._ncols - 1:
                row_id += 1
                col_id = 0
            else:
                col_id += 1

    @property
    def nrows(self):
        return self._nrows

    @property
    def ncols(self):
        return self._ncols

    def index(self, cell: int) -> Tuple[int, int]:
        return self._cell_to_indices[cell]
