#' Generate a report from the output of an AbSeq V3 pRESTO pipeline script.
#'
#' @param   config      yaml file containing run information
#' @param   sample      sample name.
#' @param   run         run name.
#' @param   output_dir  directory to write report to.
#' @param   input_dir   directory containing pRESTO log tables.
#' 
#' @return  TRUE.
#' 
#' @export
presto_abseq3 <- function(config, sample="Sample", run="Run", output_dir=".", input_dir=".") {
    ## DEBUG
    # config="test/test.yaml"; data="test/logs"
    
    abseq3_rmd <- system.file("reports/AbSeqV3.Rmd", package="prestor")
    render_params <- c(list(data=normalizePath(input_dir)),
                       yaml.load_file(config))
    
    rmarkdown::render(abseq3_rmd, 
                      output_format="pdf_document",
                      output_file=paste0("AbSeqV3_", run, "_", sample, ".pdf"),
                      output_dir=output_dir,
                      params=render_params)
    
    return(TRUE)
}