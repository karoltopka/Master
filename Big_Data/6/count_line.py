
from mrjob.job import MRJob

class MRlineCount(MRJob):

    def mapper(self, _, line):
        yield None, 1

    def reducer(self, key, values):
        yield "sum of lines:", sum(values)

if __name__ == "__main__":
    MRlineCount.run()
