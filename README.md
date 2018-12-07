# papertools

Papertools is a package to help you write papers in rmarkdown efficiently.
It contains the following functions. As it modifies system files and the global environment it will never be hosted on CRAN.

## as_word
Converts numbers to words.

## logit2prob
Converts logits to probabilities.

## q_alpha
Gives standardised alphas without any supporting statistics.

## remove_lead
Removes leading zeros from numbers and converts to character.

## runif_change
Caches analyses onto your computer. This function compares an object to a previous version. If there have been changes it re-runs code, otherwise it just loads previous results into the global evironment.

To install run the following code in R:<br/>
```
#install.packages("devtools")
devtools::install_github("JConigrave/papertools")
```
