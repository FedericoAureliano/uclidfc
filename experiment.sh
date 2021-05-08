rm training.txt testing.txt z3str4-arr.csv z3str4-seq.csv z3str4-las.csv
sh subset.sh 1000 > training.txt
sh subset.sh 100 > testing.txt
uclidfc --simulate data/results.csv `cat training.txt` --table -s z3seq -s z3arr -s z3las --train --language-models . > all.csv
uclidfc --simulate data/results.csv `cat testing.txt` --table -s z3seq -s z3arr -s z3las --language-models . > lida.csv