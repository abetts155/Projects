from matplotlib import pyplot as plt


def set_matplotlib_defaults():
    plt.style.use('dark_background')
    plt.rcParams['font.family'] = 'Helvetica'
    plt.rcParams['font.size'] = 16
    plt.rcParams['axes.labelsize'] = 14
    plt.rcParams['axes.labelweight'] = 'bold'
    plt.rcParams['axes.titlesize'] = 20
    plt.rcParams['xtick.labelsize'] = 12
    plt.rcParams['ytick.labelsize'] = 12
    plt.rcParams['legend.fontsize'] = 10
    plt.rcParams['figure.titlesize'] = 22
    plt.rcParams['figure.figsize'] = (18, 12)
