import time
import sys
from random_words import RandomWords

rw = RandomWords()

column_count = 4
word_count = 4
print(",".join(rw.random_words(count=column_count)))
for i in range(1000):
    time.sleep(0.1)
    print(",".join(" ".join(rw.random_words(count=word_count)) for i in range(column_count)))
    sys.stdout.flush()