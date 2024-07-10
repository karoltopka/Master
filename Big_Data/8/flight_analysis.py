from mrjob.job import MRJob
from mrjob.step import MRStep
import csv

class MRFlightDelays(MRJob):
    def mapper(self, _, line):
        try:
            # Split line into fields
            (year, month, day, day_of_week, airline, flight_number, tail_number, origin_airport, destination_airport,
             scheduled_departure, departure_time, departure_delay, taxi_out, wheels_off, scheduled_time, elapsed_time,
             air_time, distance, wheels_on, taxi_in, scheduled_arrival, arrival_time, arrival_delay, diverted,
             cancelled, cancellation_reason, air_system_delay, security_delay, airline_delay, late_aircraft_delay,
             weather_delay) = line.split(',')
            
            # Convert and calculate delays
            month = int(month)
            dep_delay = float(departure_delay) if departure_delay else 0
            arr_delay = float(arrival_delay) if arrival_delay else 0
            
            # Yield month and delays
            yield month, (dep_delay, arr_delay)
        except (IndexError, ValueError):
            pass

    def reducer(self, key, values):
        total_dep_delay = 0
        total_arr_delay = 0
        num_records = 0
        
        # Sum delays and count records
        for value in values:
            total_dep_delay += value[0]
            total_arr_delay += value[1]
            num_records += 1
        
        # Calculate average delays
        avg_dep_delay = total_dep_delay / num_records
        avg_arr_delay = total_arr_delay / num_records
        
        # Yield month and average delays
        yield key, (avg_dep_delay, avg_arr_delay)

if __name__ == '__main__':
    MRFlightDelays.run()
