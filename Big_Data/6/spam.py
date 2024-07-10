
from mrjob.job import MRJob
from mrjob.step import MRStep
import re
from collections import Counter

class MRSpamFilter(MRJob):
    def steps(self):
        return [
            MRStep(
                mapper=self.mapper_get_words,
                reducer=self.reducer_count_words
            ),
            MRStep(
                mapper=self.mapper_get_keys,
                reducer=self.reducer_output
            )
        ]

    def mapper_get_words(self, _, line):
        # Rozdzielanie linii na słowa z pominięciem znaków niealfanumerycznych
        words = re.findall(r'[a-z]+', line.lower())
        for word in words:
            yield word, 1

    def reducer_count_words(self, word, counts):
        # Zliczanie wystąpień słów
        yield word, sum(counts)

    def mapper_get_keys(self, word, count):
        # Mapowanie słów na klucze "common" (10 najczęstszych) i "unique" (występujące tylko raz)
        if count == 1:
            yield "unique", word
        else:
            yield "common", (count, word)

    def reducer_output(self, key, values):
        if key == "unique":
            # Zliczanie słów występujących tylko raz
            unique_words = list(values)
            yield "Count of unique words", len(unique_words)
        else:
            # Wyświetlanie 10 najczęstszych słów
            top10 = Counter(dict(values)).most_common(10)
            yield "Top 10 common words", [word for count, word in top10]

if __name__ == '__main__':
    MRSpamFilter.run()
