rm -r experiment
mkdir experiment

sh subset.sh 10000 > experiment/training.txt
sh subset.sh 1000 > experiment/testing.txt

uclidfc --simulate data/results.csv `cat experiment/training.txt` --table -s z3seq -s z3arr -s z3las --train --language-models experiment

uclidfc --simulate data/results.csv `cat experiment/testing.txt` --table -s z3seq -s z3arr -s z3las --language-models experiment > experiment/results-lida.csv
uclidfc --simulate data/results.csv `cat experiment/testing.txt` --table -s z3seq > experiment/results-z3seq.csv
uclidfc --simulate data/results.csv `cat experiment/testing.txt` --table -s z3arr > experiment/results-z3arr.csv
uclidfc --simulate data/results.csv `cat experiment/testing.txt` --table -s z3las > experiment/results-z3las.csv

python3 plot.py