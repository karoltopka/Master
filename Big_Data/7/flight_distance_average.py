
from mrjob.job import MRJob
from mrjob.step import MRStep
import csv

class MRAverageDistance(MRJob):

    def mapper(self, _, line):
        # Using tuple unpacking to extract each field from the line
        (year, month, day, day_of_week, airline, flight_number, tail_number,
         origin_airport, destination_airport, scheduled_departure, departure_time,
         departure_delay, taxi_out, wheels_off, scheduled_time, elapsed_time, air_time,
         distance, wheels_on, taxi_in, scheduled_arrival, arrival_time, arrival_delay,
         diverted, cancelled, cancellation_reason, air_system_delay, security_delay,
         airline_delay, late_aircraft_delay, weather_delay) = line.split(',')

        # Convert distance to float and yield
        try:
            distance = float(distance)
            yield "average_distance", distance
        except ValueError:
            # Skip lines where distance cannot be converted to float (unexpectedly malformed data)
            pass

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
