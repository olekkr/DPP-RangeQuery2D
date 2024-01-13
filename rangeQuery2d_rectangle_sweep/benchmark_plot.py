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
    for str_line in str_lines[1:] :
        backends.append(str_line[0])
        ns.append(int(str_line[1]))
        ms.append(int(str_line[2]))
        ts.append(int(str_line[3]))


### Data exploitation
ns = ns[0:10]
ms = ms[0:10]

datas_c = np.array(ts[0:10])
datas_opencl = np.array(ts[10:])

log_datas_c = np.log(ts[0:10])
log_datas_opencl = np.log(ts[10:])


### Plot config
xaxis_labels = []
for nm in zip(ns, ms) :
    form_str = "[n = {}, m = {}]".format(nm[0], nm[1])
    xaxis_labels.append(form_str)


### Benchmarking plot
plt.title("Benchmarking")
plt.plot(
    [0,1,2,3,4,5,6,7,8,9],
    log_datas_c,
    [0,1,2,3,4,5,6,7,8,9],
    log_datas_opencl,
)

plt.legend(["log_datas_c","log_datas_opencl"])

plt.xticks([0,1,2,3,4,5,6,7,8,9], labels=xaxis_labels, fontsize='8')
plt.xlabel("Array input size")
plt.ylabel("Mean time in microseconde (log)")
plt.show()

### Speed Up plot
plt.title("Speed Up")
plt.plot(
    [0,1,2,3,4,5,6,7,8,9],
    datas_c / datas_opencl,
    
)

plt.legend(["datas_c / datas_opencl"])
plt.xticks([0,1,2,3,4,5,6,7,8,9], labels=xaxis_labels, fontsize='8')
plt.xlabel("Array input sizes")
plt.ylabel("Speed Up factor")
plt.show()