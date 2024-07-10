from mrjob.job import MRJob
import csv

class MRAverageDistance(MRJob):

    def mapper(self, _, line):
        reader = csv.reader([line])
        for row in reader:
            if row[17].isdigit():
                distance = float(row[17]) 
                yield "average_distance", distance

    def reducer(self, key, values):
        total_distance = 0
        count = 0
        for value in values:
            total_distance += value
            count += 1
        if count > 0:
            average_distance = total_distance / count
            yield key, average_distance

if __name__ == '__main__':
    MRAverageDistance.run()
