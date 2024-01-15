import csv
import matplotlib.pyplot as plt
import numpy as np

### Data retrival
csv_path = "benchmark.csv"
with open(csv_path, "r") as csv_file:
    lines = csv.reader(csv_file, delimiter=",", quotechar="|")

    str_lines = []
    for line in lines:
        str_lines.append(line)

    backends = []
    ns = []
    ms = []
    ts = []
    for str_line in str_lines[1:]:
        backends.append(str_line[0])
        ns.append(int(str_line[1]))
        ms.append(int(str_line[2]))
        ts.append(int(str_line[3]))


### Data exploitation
ns = ns[0:10]
ms = ms[0:10]

datas_c = np.array(ts[0:10])
datas_opencl = np.array(ts[10:20])
datas_opencl_bis = np.array(ts[20:27])
datas_opencl_n_vs_m = np.array(ts[27:])

log_datas_c = np.log(datas_c)
log_datas_opencl = np.log(datas_opencl)
log_datas_opencl_bis = np.log(datas_opencl_bis)

### Plot config
xaxis_labels = []
for nm in zip(ns, ms):
    form_str = "[n = {}, m = {}]".format(nm[0], nm[1])
    xaxis_labels.append(form_str)

### Benchmarking plot
plt.title("Benchmarking")
plt.plot(
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    log_datas_c,
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    log_datas_opencl,
)

plt.legend(["log_datas_c", "log_datas_opencl"])

plt.xticks([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], labels=xaxis_labels, fontsize="8")
plt.xlabel("Array input size")
plt.ylabel("Mean time in microseconde (log)")
plt.show()

### Speed Up plot
plt.title("Speed Up")
plt.plot(
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    datas_c / datas_opencl,
)

plt.legend(["datas_c / datas_opencl"])
plt.xticks([0, 1, 2, 3, 4, 5, 6, 7, 8, 9], labels=xaxis_labels, fontsize="8")
plt.xlabel("Array input sizes")
plt.ylabel("Speed Up factor")
plt.show()


### Log runtime Parallel Implementation plot
plt.title("Log runtime Parallel Implementation")
plt.plot(
    np.log([10, 100, 1000, 10000, 100000, 1000000, 10000000]),
    log_datas_opencl_bis,
    np.log([10, 100, 1000, 10000, 100000, 1000000, 10000000]),
    np.log(
        [
            10**2,
            100**2,
            1000**2,
            10000**2,
            100000**2,
            1000000**2,
            10000000**2,
        ]
    ),
    "r--",
    np.log([10, 100, 1000, 10000, 100000, 1000000, 10000000]),
    np.log([10, 100, 1000, 10000, 100000, 1000000, 10000000]),
    "b--",
)

plt.legend(["datas_opencl_bis", "log(n.m)", "log(n)"])
plt.xlabel("Array input sizes (n = m) (log)")
plt.ylabel("runtime (log)")
plt.show()

print(
    "\n n vs m => (n=10 m=100_000) = ",
    str(datas_opencl_n_vs_m[0]),
    "us",
    "\n n vs m => (n=100_000 m=10) = ",
    str(datas_opencl_n_vs_m[1]),
    "us",
)
