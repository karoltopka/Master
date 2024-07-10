
from mrjob.job import MRJob

class MRLineAndCharCount(MRJob):
    def mapper(self, _, line):
        yield "lines", 1
        yield "chars", len(line.strip())
        yield "words", len(line.split())
    def reducer(self, key, values):
        yield key, sum(values)

if __name__ == "__main__":
    MRLineAndCharCount.run()
