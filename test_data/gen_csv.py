import time
import sys

print("First,Second,Third")

for i in range(1000):
    time.sleep(0.5)
    print("{},300,7000".format(i))
    sys.stdout.flush()