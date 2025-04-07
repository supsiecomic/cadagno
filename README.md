# cadagno
The cadagno package is intended to (i) read and clean TOB-format data files created by the measuring equipment at the Cadagno lake, and (ii) facilitate and automate the data analysis and plotting of the results.

## Installation
(requires devtools)
> devtools::install_github("supsiecomic/cadagno")

## Dependencies
None

## Usage
assuming that TOB data files are found in "tob" directory:
>tobdata <- list.files(pat = "tob", pattern = "TOB", full.names = TRUE)/
>x <- cadagno::read_TOB(fname = tobdata[1], outfile = FALSE)/
>y <- cadagno::clean_TOB(x = x, outfile = FALSE)/
>cadagno::plot_var(var = y$DO_mg, depth = y$Depth)/
>cadagno::plot_set(x = y)
