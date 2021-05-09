rm -r experiment
mkdir experiment

training=$1
testing=$2

queries=`find data -type f -name *.smt* | shuf -n $(($training + $testing))`
queries=($queries)

printf "%s\n" "${queries[@]:0:training}" > experiment/training.txt
printf "%s\n" "${queries[@]:training}" > experiment/testing.txt

uclidfc --simulate data/results.csv `cat experiment/training.txt` --table -s z3seq -s z3arr -s z3las --train --language-models experiment

uclidfc --simulate data/results.csv `cat experiment/testing.txt` --table -s z3seq -s z3arr -s z3las --language-models experiment > experiment/results-lida.csv
uclidfc --simulate data/results.csv `cat experiment/testing.txt` --table -s z3seq > experiment/results-z3seq.csv
uclidfc --simulate data/results.csv `cat experiment/testing.txt` --table -s z3arr > experiment/results-z3arr.csv
uclidfc --simulate data/results.csv `cat experiment/testing.txt` --table -s z3las > experiment/results-z3las.csv
uclidfc --simulate data/results.csv `cat experiment/testing.txt` --table -s z3str4 > experiment/results-z3str4.csv

python3 plot.py > experiment/score.csv