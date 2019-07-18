# ASLR or Address-Space Layout Randomization
# https://doc.dpdk.org/guides/prog_guide/multi_proc_support.html

echo 0 | sudo tee /proc/sys/kernel/randomize_va_space
