#' Parse tabled log output from pRESTO tools
#'
#' @param   log_file   a tabled pRESTO log
#' 
#' @return  a data.frame of the log table
#' 
#' @export
loadLogTable <- function(log_file) {
    # Load log table
    log_df <- read.delim(log_file, as.is=TRUE, fill=FALSE, na.strings=c("", "None"))
    
    return(log_df)
}


#' Parse console output from a pRESTO pipeline
#'
#' @param    log_file    console output filename
#' 
#' @return   data.frame  with columns (step, task, field, value)
#' 
#' @export
loadConsoleLog <- function(log_file) {
    # Parse console output
    log_text <- scan(log_file, what=character(), sep="\n", quiet=TRUE)
    log_text <- str_trim(log_text, side="both")
    log_text <- log_text[!grepl("(^PROGRESS>)|(^END>)", log_text)]
    log_df <- as.data.frame(str_split_fixed(log_text, "> ", 2), stringsAsFactors=FALSE)
    names(log_df) <- c("field", "value")
    
    # Assign steps and tasks
    log_df$step <- NA
    log_df$task <- NA
    x <- c(which(log_df[1] == "START"), nrow(log_df))
    for (i in 2:length(x)) {
        r <- x[i-1]:x[i]
        log_df$step[r] <- i - 1
        if ("COMMAND" %in% log_df$field[r]) {
            n <- paste(log_df$value[x[i-1]], 
                       log_df$value[r][log_df$field[r] == "COMMAND"],
                       sep="-")
        } else {
            n <- log_df$value[x[i-1]]
        }
        log_df$task[r] <- n
    }
    
    # Remove START field
    #log_list <- dlply(log_df, .(step))
    log_df <- subset(log_df, !(field %in% c("START", "COMMAND")), select=c(step, task, field, value))
    log_df <- transform(log_df, step=as.numeric(step), task=factor(task, levels=unique(task)))
    
    return(log_df)
}


#' Define universal plot settings
#' 
#' @param    size
#' @return   a ggplot2 theme object
getBaseTheme <- function(font=8) {
    # Define universal plot settings
    base_theme <- theme_bw() + 
        theme(text=element_text(size=font)) +
        theme(plot.title=element_text(size=font)) +
        theme(legend.text=element_text(size=font)) +
        theme(strip.background=element_rect(fill="white"),
              strip.text=element_text(size=font, face="bold")) + 
        #theme(strip.background=element_blank(),
        #      strip.text=element_text(size=font, face="bold")) + 
        theme(axis.title=element_text(size=font, vjust=0.25),
              axis.text.x=element_text(size=font, vjust=0.5, hjust=0.5),
              axis.text.y=element_text(size=font)) +
        theme(plot.background=element_blank(), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
    
    return(base_theme)
}


#' Plot multiple ggplot objects
#' @references  http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
#' 
#' @param   ...    ggplot objects to plot
#' @param   ncol   number of columns in the plot 
#' @return  NULL
multiplot <- function(..., ncol=1) {
    p <- list(...)
    n <- length(p)
    layout <- matrix(seq(1, ncol*ceiling(n/ncol)), ncol=ncol, nrow=ceiling(n/ncol))
    
    # Plot
    if (n == 1) {
        plot(p[[1]])
    } else {
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(nrow(layout), ncol(layout))))
        for (i in 1:n) {
            idx <- as.data.frame(which(layout == i, arr.ind=T))
            plot(p[[i]], vp=viewport(layout.pos.row = idx$row, layout.pos.col=idx$col))
        }
    }
}


# Check log data.frame for valid fields and issue stop message if invalid
#
# @param   log_df  data.frame of tabled pRESTO log
# @param   fields  vector of column names to check
# @param   logic   one of "all" or "any" controlling whether all or at least one of
#                  the fields must be valid
# @return  TRUE is log fields are valid and a string message if not.
checkLogFields <- function(log_df, fields, logic=c("all" ,"any")) {
    # Check arguments
    logic <- match.arg(logic)
    
    if (logic == "all") {
        # Check that fields exist
        if (!all(fields %in% names(log_df))) {
            msg <- paste("Log table must contain the fields:", paste(fields, collapse=", "))
            return(msg)
        }
        # Check that all values are not NA
        for (f in fields) {
            if (all(is.na(log_df[, f]))) { 
                msg <- paste("The field", f, "contains no data") 
                return(msg)
            }
        }
    } else if (logic == "any") {
        # Check that fields exist
        if (!any(fields %in% names(log_df))) {
            msg <- paste("Log table must contain at least one of the fields:", paste(fields, collapse=", "))
            return(msg)
        }
        # Check that all values are not NA
        invalid <- sapply(fields, function(f) all(is.na(log_df[, f])))
        if (all(invalid)) { 
            msg <- paste("None of the fields", paste(fields, collapse=", "), "contain data") 
            return(msg)
        }
    }
    
    # Return TRUE if all checks pass
    return(TRUE)
}