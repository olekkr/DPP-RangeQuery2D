import numpy as np


def remapping(id, d):
    row_offset = (id / 2 ** (d)) * 2 ** (d + 1)
    column_offset = 2 * (id - (id % (2**d)))
    print(id, d, row_offset + column_offset)


remapping(0, 1)
remapping(1, 1)
remapping(2, 1)
remapping(3, 1)
print("\n\n\n")

remapping(0, 2)
remapping(1, 2)
remapping(2, 2)
remapping(3, 2)
remapping(4, 2)
remapping(5, 2)
remapping(6, 2)
remapping(7, 2)
remapping(8, 2)
remapping(9, 2)
remapping(10, 2)
remapping(11, 2)
remapping(12, 2)
remapping(13, 2)
remapping(14, 2)
remapping(15, 2)
