library(rmarkdown)
render('Test_SparseOLS.Rmd', output_format = 'pdf_document',
       run_pandoc = TRUE)
