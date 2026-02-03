# cadagno
The cadagno package is intended to (i) read and clean TOB-format data files created by the measuring equipment at the Cadagno lake, and (ii) facilitate and automate the data analysis and plotting of the results.

## Installation
(requires remotes or devtools)
> remotes::install_github("supsiecomic/cadagno")

## Dependencies
- TTR

## Usage
assuming that TOB data files are found in "tob" directory, list the TOB files:
>tobdata <- list.files(pat = "tob", pattern = "TOB", full.names = TRUE)

read TOB data as data frame:
>x <- cadagno::read_TOB(fname = tobdata[1], outfile = FALSE)

clean TOB data:
>y <- cadagno::clean_TOB(x = x, outfile = FALSE)

plot single variable:
>cadagno::plot_var(var = y$DO_mg, depth = y$Depth)

plot set of variables:
>cadagno::plot_set(x = y)
