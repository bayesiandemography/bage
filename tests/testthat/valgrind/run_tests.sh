
# Code to run valgrind tests locally (needs to be modified to match file structure etc)

docker build --platform=linux/amd64 -t r-valgrind-bage .

docker run --platform=linux/amd64 -it --rm -v ~/Documents/git.nosync/bage:/home/code  r-valgrind-bage bash

cd /home/code valgrind --leak-check=full --track-origins=yes Rscript tests/testthat/test-bage_mod-methods.R


