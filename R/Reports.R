# Report templates

#' R Markdown to PDF format for pRESTO reports
#'
#' @param   toc         if \code{TRUE} add table of contents.
#' 
#' @return  An R Markdown output format.
#' 
#' @export
pdf_presto <- function(toc=TRUE) {
    #header <- system.file("reports/header.tex", package="prestor")
    #includes=includes(in_header=header))
    #template <- system.file("templates/presto.tex", package="prestor")
    rmarkdown::pdf_document(toc=toc, fig_width=7.5, fig_height=4.5, fig_crop=TRUE,
                            fig_caption=TRUE, number_sections=TRUE,
                            citation_package="biblatex")
}

#' Generate a report from the output of an AbSeq V3 pRESTO pipeline script.
#'
#' @param   input_dir      directory containing pRESTO log tables.
#' @param   output_dir     directory to write report to.
#' @param   title          report title.
#' @param   sample         sample name.
#' @param   run            run name.
#' @param   author         run name.
#' @param   version        pRESTO version used.
#' @param   description    description of the run.
#' @param   date           date of run. If \code{NULL} use the current date.
#' @param   output_file    output file name. If \code{NULL} the name will be build
#'                         from the sample, run and date.
#' @param   config         yaml file containing paramaters. Parameters in the yaml
#'                         file will override anything specified as function arguments.
#' @param   format         output format. One of \code{"pdf"} or \code{"html"}.
#' @param   quiet          if TRUE do not print out knitr processing output.
#' 
#' @return  Path to the output file.
#' 
#' @export
report_abseq3 <- function(input_dir=".", output_dir=".", 
                          title="pRESTO Report: AbSeq v3", sample="Sample", run="Run", 
                          author="", version="", description="", date=NULL, 
                          output_file=NULL, config=NULL, format=c("pdf", "html"), 
                          quiet=TRUE) {
    ## DEBUG
    # config="test/test.yaml"; data="test/logs"
    
    # Check args
    format <- match.arg(format)
    if (format == "pdf") { 
        format <- "pdf_presto"
        format_ext <- ".pdf"
    } else if (format == "html") {
        format <- "html_document"
        format_ext <- ".html"
    }
    
    # Get absolute paths
    input_dir <- normalizePath(input_dir)
    output_dir <- normalizePath(output_dir)
    
    # Set rendering parameters
    render_params <- list(data=input_dir,
                          title=title,
                          sample=sample,
                          run=run,
                          author=author,
                          version=version,
                          description=description,
                          date=date)
    
    # Load config from yaml file
    if (!is.null(config)) {
        config_params <- yaml.load_file(config)
        render_params <- modifyList(render_params, config_params)
    }

    if (is.null(render_params$date)) { 
        render_params$date <- format(Sys.time(), "%Y-%m-%d") 
    }
    if (is.null(output_file)) { 
        output_file <- paste0(render_params$run, "_", 
                              render_params$sample, "_", 
                              render_params$date, format_ext) 
    }
    
    # Workaround for broken intermediates_dir argument to rmarkdown::render
    temp_dir <- tempdir()
    report_dir <- system.file("reports", package="prestor")
    file.copy(report_dir, temp_dir, recursive=T)
    rmd <- file.path(temp_dir, "reports", "AbSeqV3.Rmd")
    
    # Render
    #rmd <- system.file("reports/AbSeqV3.Rmd", package="prestor")
    #intermediates_dir=tempdir(),
    rmarkdown::render(rmd, 
                      output_format=format,
                      output_file=output_file,
                      output_dir=output_dir,
                      knit_root_dir=output_dir,
                      quiet=quiet,
                      params=render_params)
}

#' Default chunk evaluation function
#' 
#' Simple pass through default function for chunk evaluation in template.
#' 
#' @return  \code{TRUE}
#' 
#' @export
chunkEval <- function() { TRUE }