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
datas_c = np.array(ts[0:4])
datas_opencl = np.array(ts[4:10])
datas_opencl_n_vs_m = np.array(ts[10:12])

ns_c = ns[:4]
ms_c = ms[:4]

ns_opencl = ns[4:10]
ms_opencl = ms[4:10]

### Plot config
xaxis_labels = []
for nm in zip(ns_c, ms_c):
    form_str = "[n = {}, m = {}]".format(nm[0], nm[1])
    xaxis_labels.append(form_str)

xaxis_labels_opencl = []
for nm in zip(ns_opencl, ms_opencl):
    form_str = "[n = {}, m = {}]".format(nm[0], nm[1])
    xaxis_labels_opencl.append(form_str)

### Benchmarking plot
xs = range(len(datas_c))
plt.title("Benchmarking")
plt.plot(
    xs,
    datas_c,
    xs,
    datas_opencl[:len(xs)],
)
plt.legend(["datas_c", "datas_opencl"])
plt.xticks(xs, labels=xaxis_labels, fontsize="8")
plt.xlabel("Array input size")
plt.ylabel("Runtime (us)")
plt.show()

### Speed Up plot
plt.title("Speed Up")
plt.plot(
    xs,
    datas_c / datas_opencl[:len(xs)],
)

plt.legend(["datas_c / datas_opencl"])
plt.xticks(xs, labels=xaxis_labels, fontsize="8")
plt.xlabel("Array input sizes")
plt.ylabel("Speed Up factor")
plt.show()


### Parallel Implementation plot
xs_opencl = range(len(datas_opencl))
plt.title("Parallel Implementation")
plt.plot(
    xs_opencl,
    datas_opencl,
)

plt.legend(["datas_opencl_bis", "log(n)"])
plt.xticks(xs_opencl, labels=xaxis_labels_opencl, fontsize="8")
plt.xlabel("Array input sizes")
plt.ylabel("Runtime (us)")
plt.show()

### n vs m plot
plt.title("n vs m")
plt.bar([0, 1], datas_opencl_n_vs_m)

plt.xticks([0, 1], labels=["n = 10, m = 100_000", "n = 100_000, m = 10"], fontsize="8")
plt.xlabel("Array input shape")
plt.ylabel("Runtime (us)")
plt.show()
