import sys
import re
import matplotlib.pyplot as plt

def parse_lines(lines):
    sizes, offline_with_partition, offline_no_partition, cooper, lengauer_tarjan = [], [], [], [], []
    times_pattern = re.compile(r'^\s*([0-9.]+)\s*([0-9.]+)\s*([0-9.]+)\s*([0-9.]+)\s*$')
    for line in lines:
        line = line.strip()
        if line:
            if re.match(r'^\d+$', line):
                n = int(line)
                sizes.append(n)
            else:
                m = times_pattern.match(line)
                if m:
                    t1, t2, t3, t4 = m.groups()
                    offline_with_partition.append(float(t1))
                    offline_no_partition.append(float(t2))
                    cooper.append(float(t3))
                    lengauer_tarjan.append(float(t4))
    return sizes, offline_with_partition, offline_no_partition, cooper, lengauer_tarjan


def main():
    path = sys.argv[1]
    with open(path, "r", encoding="utf-8") as f:
        size, offline_with_partition, offline_no_partition, cooper, lengauer_tarjan = parse_lines(f.readlines())

    plt.figure(figsize=(9, 6))
    plt.plot(size, offline_with_partition, linewidth=1, marker='o', markersize=4, markerfacecolor='none', markeredgewidth=1.2, color="#00429d", label="Offline (with partition)")
    plt.plot(size, offline_no_partition, linewidth=1, marker='^', markersize=4, markerfacecolor='none', markeredgewidth=1.2, color="#4f9ee3", label="Offline (no partition)")
    plt.plot(size, cooper, linewidth=1, marker='x', markersize=4, markerfacecolor='none', markeredgewidth=1.2, color="#d62828", label="Cooper")
    plt.plot(size, lengauer_tarjan, linewidth=1, marker='.', markersize=4, markerfacecolor='none', markeredgewidth=1.2, color="#ff7f0e", label="Lengauer–Tarjan")

    ax = plt.gca()
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.tick_params(top=False, right=False)

    plt.xlabel("#Vertices")
    plt.ylabel("Runtime (seconds)")
    plt.legend()
    plt.tight_layout()
    plt.savefig("dense.png", dpi=220)
    plt.show()

if __name__ == "__main__":
    main()
