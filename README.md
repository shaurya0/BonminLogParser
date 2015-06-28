# BonminLogParser
This project compiles an executable which can be used for extracting the results of the output generated
by [Bonmin](https://projects.coin-or.org/Bonmin). It utilizes the Haskell attoparsec library as a replacement for regular expressions.
The results are output to a csv file

Example usage:

```bash
./bonmin some_minlp.nl > some_minlp.txt
./BonminLogParser -i=$PATH/some_minlp.txt  -o=$PATH/results.csv
```
