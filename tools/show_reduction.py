from collections import OrderedDict
from matplotlib import pyplot as plt
from re import compile


parameter_line = compile(r'[=]+ depth=(?P<DEPTH>\d+)\s+vertices=(?P<VERTICES>\d+) [=]+')
results_line = compile(r'Old=(?P<OLD>\d+.\d+)\s+New=(?P<NEW>\d+.\d+)')

data = OrderedDict()
with open('data.txt', 'r') as in_file:
    for line in in_file:
        settings = parameter_line.match(line)
        if settings:
            depth = int(settings.group('DEPTH'))
            vertices = int(settings.group('VERTICES'))

        results = results_line.match(line)
        if results:
            old = float(results.group('OLD'))
            new = float(results.group('NEW'))
            data.setdefault(depth, []).append((vertices, old, new))


nrows = 4
ncols = 5
fig, axs = plt.subplots(nrows, ncols, figsize=(15, 12), constrained_layout=True)
row = 0
col = 0
for depth, depth_data in data.items():
    x_values = []
    old_line = []
    new_line = []

    for vertices, old, new in depth_data:
        x_values.append(vertices)
        old_line.append(old)
        new_line.append(new)

    ax = axs[row, col]
    ax.plot(x_values, old_line, label='Old', color='black', marker='|')
    ax.plot(x_values, new_line, label='New', color='dodgerblue', marker='.')
    ax.set_title('Depth={}'.format(depth))
    ax.legend()

    if col == ncols - 1:
        row += 1
        col = 0
    else:
        col += 1

plt.show()
