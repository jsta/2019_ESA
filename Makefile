all: slides.pdf

slides.pdf: slides.Rmd
	Rscript -e "rmarkdown::render('$<')"
