fswatch $1 | xargs -n1 -I{} ./uclidfc $1 -r false -o $2