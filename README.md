The HarvardX PH125.9x Data Science: Capstone course requires the following files for Movielens project:

- The report in Rmd format (`movielens.Rmd`)
- The report in PDF format (`movielens.pdf`)
- The code in R format that generates the report (`movielens.R`)

There is also a `bibliography.bib` file, as I decided to keep bibliography separated.
In order to compile the `.Rmd` file, you need to have R and Tex installed on your machine.

Note: If you don't have a powerful machine, I suggest to modify the `movielens.R#L699` (line 699) where the variable `lambdas` is declared and then to run the code with only one value.