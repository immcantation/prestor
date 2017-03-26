# Prototype.
# 
# @author     Jason Anthony Vander Heiden
# @copyright  Copyright 2015 Kleinstein Lab, Yale University. All rights reserved
# @license    Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported
# @version    0.2.0
# @date       2015.04.05

#### Constants ####
FILTER_COUNT_FIELDS <- c("LENGTH"  = "Read length", 
                         "MASKED"  = "Masked positions", 
                         "MISSING" = "Missing characters", 
                         "QUALITY" = "Mean quality score", 
                         "REPEATS" = "Maximum repeat length")

# n <- 5
# plot(1:n, col=colorspace::rainbow_hcl(n, l=65, c=100), pch=16, cex=8)
# PRESTO_PALETTE <- c("red"    = "#FF6C91",
#                    "orange" = "#BC9D00",
#                    "green"  = "#00BC57",
#                    "blue"   = "#00B8E5",
#                    "purple" = "#CD79FF",
#                    "grey"   = "#7F7F7F")
# n <- 5
# plot(1:n, col=RColorBrewer::brewer.pal(n, "Set1"), pch=16, cex=8)
PRESTO_PALETTE <- c("red"        = "#E41A1C",
                    "orange"     = "#FF7F00",
                    "green"      = "#4DAF4A",
                    "blue"       = "#377EB8",
                    "purple"     = "#984EA3",
                    "grey"       = "#999999",
                    "dark_blue"  = "#17364F",
                    "dark_red"   = "#6A0C0D",
                    "dark_green" = "#234F21")


#### Plotting functions ####

#' Plot console output from a pRESTO pipeline
#'
#' @param    log_df  data.frame returned by loadConsoleLog
#'                   with columns (step, task, field, value).
#' @param    title   text to preprend to each plot title.
#' @param    pass    log entries defining passed counts. Should contain "UNDETERMINED" 
#'                   if you specified --keepmiss to CollapseSeq.
#' @param    fail    log entries defining failed counts. Should count "UNDETERMINED" 
#'                   if you did NOT specify --keepmiss to CollapseSeq.
#' @param    sizing  defines the style and sizing of the theme. One of 
#'                   \code{c("figure", "window")} where \code{sizing="figure"} is appropriately
#'                   sized for pdf export at 7 to 7.5 inch width, and \code{sizing="window"}
#'                   is sized for an interactive session.
#' 
#' @return   data.frame of passed and failed counts with columns
#'           (step, task, pass, fail, total, fraction)
#' 
#' @family   pRESTO log plotting functions
#' 
#' @export
plotConsoleLog <- function(log_df, title="Reads retained by pipeline step", 
                           pass=c("PASS", "UNIQUE"), fail=c("FAIL", "DUPLICATE", "UNDETERMINED"),
                           sizing=c("figure", "window")) {
    ## DEBUG
    # log_df <- console_log
    # title=""; pass=c("PASS", "UNIQUE"); fail=c("FAIL", "DUPLICATE", "UNDETERMINED")
    # base_theme <- alakazam:::getBaseTheme(sizing="window") + theme(legend.position="none")
    
    # Check arguments
    sizing <- match.arg(sizing)
    
    # Set base plot settings
    base_theme <- alakazam:::getBaseTheme(sizing=sizing) + 
        theme(legend.position="none") +
        theme(axis.text.x=element_text(size=rel(0.9)))
        #theme(axis.text.x=element_text(angle=90, hjust=0.5, vjust=0.5))
    
    # Get passed entries
    pass_df <- log_df %>%
        filter_(interp(~x %in% pass, x=as.name("field"))) %>%
        mutate_at("value", as.numeric) %>%
        group_by_("step", "task") %>%
        summarize_(pass=interp(~sum(x, na.rm=TRUE), x=as.name("value")))

    # Get failed entries
    fail_df <- log_df %>%
        filter_(interp(~x %in% fail, x=as.name("field"))) %>%
        mutate_at("value", as.numeric) %>%
        group_by_("step", "task") %>%
        summarize_(fail=interp(~sum(x, na.rm=TRUE), x=as.name("value")))
    
    # Merge passed and failed counts
    count_df <- inner_join(pass_df, fail_df, by=c("step", "task")) %>%
        rowwise() %>%
        mutate_(total=interp(~sum(x, y), x=as.name("pass"), y=as.name("fail")),
                fraction=interp(~x/y, x=as.name("pass"), y=as.name("total")))

    # Define x-axis labels with two lines
    x_names <- unique(count_df$task)
    x_labels <- setNames(gsub("-", "\n", x_names), x_names)
    
    # Plot count of reads passing each step
    p1 <- ggplot(count_df, aes(x=task, y=pass, group=step)) + 
        base_theme + 
        ggtitle(title) + 
        xlab("") +
        ylab("Reads retained (count)") +
        scale_x_discrete(labels=x_labels) +
        scale_y_continuous(labels=scientific) +
        geom_bar(aes(fill=task), stat="identity", position=position_dodge(width=0.8), width=0.7)
    # Plot faction of reads passing each step
    p2 <- ggplot(count_df, aes(x=task, y=fraction, group=step)) + 
        base_theme + 
        xlab("") +
        ylab("Reads retained (% of input)") +
        scale_x_discrete(labels=x_labels) +
        scale_y_continuous(labels=percent) +
        geom_bar(aes(fill=task), stat="identity", position=position_dodge(width=0.8), width=0.7)
    gridPlot(p1, p2, ncol=1)

    return(count_df)
}


#' Plot FilterSeq log table
#'
#' @param    ...     data.frames returned by loadLogTable to plot
#' @param    titles  vector of titles for each log in ...; 
#'                   if NULL the titles will be empty.
#' @param    cutoff  value at which to draw a vertical line separating 
#'                   passing and failing values.
#' @param    sizing  defines the style and sizing of the theme. One of 
#'                   \code{c("figure", "window")} where \code{sizing="figure"} is appropriately
#'                   sized for pdf export at 7 to 7.5 inch width, and \code{sizing="window"}
#'                   is sized for an interactive session.
#' 
#' @return   NULL
#' 
#' @family   pRESTO log plotting functions
#' 
#' @export
plotFilterSeq <- function(..., titles=NULL, cutoff=20, sizing=c("figure", "window")) {
    ## DEBUG
    # titles=NULL
    # cutoff=20

    # Parse arguments
    log_list <- list(...)
    log_count <- length(log_list)
    sizing <- match.arg(sizing)
    
    # Define titles
    if (is.null(titles)) {
        titles <- rep("", log_count)
    } else if (length(titles) != log_count) {
        stop("You must specify one title per input log table.")
    }
    
    # Set base plot settings
    base_theme <- alakazam:::getBaseTheme(sizing=sizing)

    # Define plot objects for each log table
    plot_list <- list()
    for (i in 1:log_count) {
        log_df <- log_list[[i]]
        
        # Get count field
        log_fields <- names(log_df)
        count_field <- log_fields[log_fields %in% names(FILTER_COUNT_FIELDS)]
        if (length(count_field) < 1) {
            stop("No valid FilterSeq log field was found.")
        } else if (length(count_field) < 1) {
            stop("Too many FilterSeq log fields were found. Only one count field should be present.")
        }
        # Check count field
        check <- alakazam:::checkColumns(log_df, count_field)
        if (check != TRUE) { stop(check) }
        
        # Table counts
        log_tab <- as.data.frame(table(value=floor(as.numeric(log_df[, count_field]))), 
                                 responseName="count")
        #print(log_tab)
        #log_tab$value <- as.numeric(log_tab$value)
        x_intercept <- which(log_tab$value == cutoff)
        p1 <- ggplot(log_tab, aes(x=value, y=count)) +
            base_theme +     
            ggtitle(titles[i]) +
            xlab(FILTER_COUNT_FIELDS[count_field]) +
            ylab("Reads") +
            #scale_x_continuous() + 
            scale_y_continuous(labels=scientific) + 
            #geom_histogram(fill=PRESTO_PALETTE["blue"], binwidth=1, center=0) +
            geom_bar(fill=PRESTO_PALETTE["blue"], stat="identity", width=0.95) +
            geom_vline(xintercept=x_intercept, color=PRESTO_PALETTE["red"], size=0.5, linetype=3)

        plot_list[[i]] <- p1
    }
    
    # Plot
    do.call(gridPlot, args=c(plot_list, ncol=1))
}


#' Plot MaskPrimer log table
#'
#' @param    ...        data.frames returned by loadLogTable to plot.
#' @param    titles     vector of titles for each log in ...; 
#'                      if NULL the titles will be empty.
#' @param    style      type of plot to draw. One of:
#'                      \itemize{
#'                        \item \code{"histogram"}:  total error distribution
#'                        \item \code{"count"}:      count of matches for each primer
#'                        \item \code{"error"}:      error distributions for each primer
#'                        \item \code{"position"}:   distribution of start positions by primer
#'                                                   for matches >= max_error.
#'                      }
#' @param    max_error  error threshold to used to determing whether a primer match is valid.
#' @param    sizing     defines the style and sizing of the theme. One of 
#'                      \code{c("figure", "window")} where \code{sizing="figure"} is appropriately
#'                      sized for pdf export at 7 to 7.5 inch width, and \code{sizing="window"}
#'                      is sized for an interactive session.
#'                  
#' @return   NULL
#' 
#' @family   pRESTO log plotting functions
#' 
#' @export
plotMaskPrimers <- function(..., titles=NULL, style=c("histogram", "count", "error", "position"),
                            max_error=0.2, sizing=c("figure", "window")) {
    ## DEBUG
    # c('PRIMER', 'PRSTART', 'BARCODE', 'ERROR')
    # titles=NULL
    # max_error=0.2
    # style="h"

    # Parse arguments
    style <- match.arg(style)
    sizing <- match.arg(sizing)
    log_list <- list(...)
    log_count <- length(log_list)
    
    # Define titles
    if (is.null(titles)) {
        titles <- rep("", log_count)
    } else if (length(titles) != log_count) {
        stop("You must specify one title per input log table.")
    }
    
    # Set base plot settings
    base_theme <- alakazam:::getBaseTheme(sizing=sizing)
    
    # Define plot objects for each log table
    plot_list <- list()
    for (i in 1:log_count) {
        log_df <- log_list[[i]]
        if (style == "histogram") {
            # Check for valid log table
            check <- alakazam:::checkColumns(log_df, c("ERROR"))
            if (check != TRUE) { stop(check) }
            
            # Plot total error distribution
            p1 <- ggplot(log_df, aes(x=ERROR)) +
                base_theme +     
                ggtitle(titles[i]) +
                xlab("Error") +
                ylab("Reads") +
                scale_x_continuous(limits=c(-0.05, 1.05), breaks=seq(0.0, 1.0, 0.2)) +
                scale_y_continuous(labels=scientific_format()) + 
                geom_histogram(binwidth=0.025, colour="white", fill=PRESTO_PALETTE["blue"], 
                               size=0.25, center=0) +
                geom_vline(xintercept=max_error, color=PRESTO_PALETTE["red"], size=0.5, linetype=3)
        } else if (style == "count") {
            # Check for valid log table
            check <- alakazam:::checkColumns(log_df, c("ERROR", "PRIMER"))
            if (check != TRUE) { stop(check) }
            
            # Set passed and failed 
            log_df$RESULT <- factor(log_df$ERROR <= max_error, 
                                    levels=c(TRUE, FALSE), 
                                    labels=c("Pass", "Fail"))
            # Plot primer match counts
            guide_values <- setNames(c(PRESTO_PALETTE["blue"], PRESTO_PALETTE["red"]), c("Pass", "Fail"))
            p1 <- ggplot(log_df, aes(x=PRIMER)) +
                base_theme + 
                theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
                ggtitle(titles[i]) +
                xlab("") +
                ylab("Reads") +
                scale_fill_manual(name="", values=guide_values) +
                geom_bar(aes(fill=RESULT), position="stack", width=0.7)
        } else if (style == "error") {
            # Check for valid log table
            if (!all(c("ERROR", "PRIMER") %in% names(log_df))) {
                stop("Log table must contain the fields 'PRIMER' and 'ERROR'.")
            } else if (all(is.na(log_df$ERROR))) {
                stop("The field 'ERROR' contains no data.")
            } else if (all(is.na(log_df$PRIMER))) {
                stop("The field 'PRIMER' contains no data.")
            }
            
            # Plot error distribution by primer
            p1 <- ggplot(log_df, aes(x=PRIMER, y=ERROR)) +
                base_theme + 
                theme(legend.position="none") +
                theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
                ggtitle(titles[i]) +
                xlab("") +
                ylab("Error") +
                scale_y_continuous(limits=c(-0.05, 1.05), breaks=seq(0, 1, 0.2)) +
                geom_violin(aes(fill=PRIMER), adjust=3.0, scale="width", trim=T, width=0.7) +
                geom_errorbarh(aes(xmin=(..x..) - 0.4, xmax=(..x..) + 0.4), size=1.5,
                               stat="summary", fun.y="mean") +
                geom_hline(yintercept=max_error, color=PRESTO_PALETTE["red"], size=0.5, linetype=3)    
        } else if (style == "position") {
            # Check for valid log table
            check <- alakazam:::checkColumns(log_df, c("PRIMER", "PRSTART"))
            if (check != TRUE) { stop(check) }
            
            # Plot start position by primer
            p1 <- ggplot(subset(log_df, ERROR <= max_error), aes(x=PRIMER, y=PRSTART)) +
                base_theme + 
                theme(legend.position="none") +
                theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
                ggtitle(titles[i]) +
                xlab("") +
                ylab("Start position") +
                geom_boxplot(aes(fill=PRIMER), width=0.7)
        } else {
            stop("Nothing to plot.")
        }

        plot_list[[i]] <- p1
    }
    
    # Plot
    do.call(gridPlot, args=c(plot_list, ncol=1))
}


#' Plot BuildConsensus log table
#' 
#' @param    ...          data.frames returned by loadLogTable to plot
#' @param    titles       vector of titles for each log in ...; 
#'                        if NULL the titles will be empty.
#' @param    style        type of plot to draw. One of:
#'                        \itemize{
#'                          \item \code{"size"}:       distribution of UMI read group sizes
#'                                                     (number of reads per UMI).
#'                          \item \code{"error"}:      distribution of diversity scores
#'                                                     (DIVERSITY) or error rates (ERROR) 
#'                                                     for all UMI read groups.
#'                          \item \code{"prfreq"}:     distribution of majory primer frequency 
#'                                                     (PRFREQ) for all UMI read groups.
#'                          \item \code{"prsize"}:     distribution of UMI read group size
#'                                                     for groups with PRFREQ >= primer_freq.
#'                          \item \code{"prerror"}:    distribution of UMI read group diversity
#'                                                     or error rates for groups with 
#'                                                     PRFREQ >= primer_freq.
#'                        }
#' @param    min_size     minimum UMI count threshold.
#' @param    max_error    maximum error rate threshold.
#' @param    primer_freq  minimum frequency threshold for consensus primers.
#' @param    sizing       defines the style and sizing of the theme. One of 
#'                        \code{c("figure", "window")} where \code{sizing="figure"} is appropriately
#'                        sized for pdf export at 7 to 7.5 inch width, and \code{sizing="window"}
#'                        is sized for an interactive session.
#'                  
#' @return   NULL
#' 
#' @family   pRESTO log plotting functions
#' 
#' @export
plotBuildConsensus <- function(..., titles=NULL, 
                               style=c("size", "error", "prfreq", "prsize", "prerror"), 
                               min_size=1, max_error=0.1, primer_freq=0.6, sizing=c("figure", "window")) {
    ## DEBUG
    # log_df <- consensus_log_1
    # c('BARCODE', 'SEQCOUNT', 'PRIMER', 'PRCOUNT', 'PRCONS', 'PRFREQ', 'CONSCOUNT', 'DIVERSITY', 'ERROR')
    # c('PRIMER', 'PRSTART', 'BARCODE', 'ERROR')
    # titles=rep("", 2)
    # min_size=1
    # max_error=0.1
    # primer_freq=0.6
    # style="h"
    
    # Parse arguments
    style <- match.arg(style)
    sizing <- match.arg(sizing)
    log_list <- list(...)
    log_count <- length(log_list)
        
    # Define titles
    if (is.null(titles)) {
        titles <- rep("", log_count)
    } else if (length(titles) != log_count) {
        stop("You must specify one title per input log table.")
    }
    
    # Set base plot settings
    base_theme <- alakazam:::getBaseTheme(sizing=sizing) +
        theme(legend.position="bottom")

    # Function to calculate PRCONS and PRFREQ
    .calcPrimerFreq <- function(df) {
        # Return input if PRCONS and PRFREQ are already calculated
        if (all(c("PRCONS", "PRFREQ") %in% names(df)) & 
            !all(is.na(df$PRCONS)) & 
            !all(is.na(df$PRFREQ))) {
            return(df)
        }
        
        # Check for valid log table
        check <- alakazam:::checkColumns(df, c("PRIMER", "PRCOUNT"))
        if (check != TRUE) { stop(check) }
        
        # Get primer counts and names
        count_list <- strsplit(as.character(df$PRCOUNT), ",")
        count_list <- lapply(count_list, as.numeric)
        primer_list <- strsplit(as.character(df$PRIMER), ",")
        
        # Define consensus primers and frequency
        index <- sapply(count_list, which.max)
        prfreq <- sapply(count_list, function(x) max(x, na.rm=TRUE)/sum(x, na.rm=TRUE))
        prcons <- sapply(1:length(primer_list), function(i) primer_list[[i]][index[i]])
        
        # Update columns
        df$PRCONS <- prcons
        df$PRRFEQ <- prfreq     
        
        return(df)
    }
    
    # Define plot objects for each log table
    plot_list <- list()
    for (i in 1:log_count) {
        log_df <- log_list[[i]]
        
        if (style == "size") {
            # Check for valid log table
            check <- alakazam:::checkColumns(log_df, c("SEQCOUNT", "CONSCOUNT"))
            if (check != TRUE) { stop(check) }
            
            # Plot UMI size distribution
            seq_tab <- log_df %>%
                group_by_("SEQCOUNT") %>%
                dplyr::summarize(UMICOUNT=n())
            cons_tab <- log_df %>%
                filter_(interp(~!is.na(x), x=as.name("CONSCOUNT"))) %>%
                group_by_("CONSCOUNT") %>%
                dplyr::summarize(UMICOUNT=n())
            
            guide_values <- setNames(c(PRESTO_PALETTE["green"], PRESTO_PALETTE["blue"]), c("seq", "cons"))
            guide_labels <- setNames(c("Total", "Consensus"), c("seq", "cons"))
            p1 <- ggplot() + 
                base_theme +
                ggtitle(titles[i]) +
                xlab("Reads per UMI") +
                ylab("Number of UMIs") +
                #scale_x_continuous(trans=log2_trans(),
                #                   breaks = trans_breaks("log2", function(x) 2^x),
                #                   labels = trans_format("log2", math_format(2^.x))) +
                scale_y_log10(breaks=trans_breaks("log10", function(x) 10^x),
                              labels=trans_format("log10", math_format(10^.x))) + 
                scale_fill_manual(name="Reads", values=guide_values, labels=guide_labels) +
                geom_bar(data=seq_tab, aes(x=SEQCOUNT, y=UMICOUNT, fill="seq"), 
                         stat="identity", color=guide_values["seq"], position="identity") +
                geom_bar(data=cons_tab, aes(x=CONSCOUNT, y=UMICOUNT, fill="cons"), 
                         stat="identity", color=guide_values["cons"], position="identity") +
                geom_vline(xintercept=min_size, color=PRESTO_PALETTE["red"], size=0.5, linetype=3)
        } else if (style == "error") {
            # Check for valid log table
            log_fields <- names(log_df)
            error_fields <- c("ERROR"="Error", "DIVERSITY"="Diversity")
            if ("ERROR" %in% log_fields) {
                # Check that error column is valid
                check <- alakazam:::checkColumns(log_df, "ERROR")
                if (check != TRUE) { stop(check) }
                f <- "ERROR"
            } else if ("DIVERSITY" %in% log_fields) {
                # Check that p-value column is valid
                check <- alakazam:::checkColumns(log_df, "DIVERSITY")
                if (check != TRUE) { stop(check) }
                f <- "DIVERSITY"
            } else {
                stop("Log table must contain one of fields 'ERROR' or 'DIVERSITY'.")
            }
            
            # Plot UMI error
            log_df <- log_df[is.finite(log_df[[f]]), ]
            p1 <- ggplot(log_df, aes_string(x=f)) +
                base_theme +
                ggtitle(titles[i]) +
                xlab(error_fields[f]) +
                ylab("Number of UMIs") +
                scale_x_continuous(limits=c(-0.05, 1.05), breaks=seq(0.0, 1.0, 0.2)) +
                scale_y_continuous(labels=scientific) + 
                geom_histogram(binwidth=0.025, fill=PRESTO_PALETTE["blue"], color="white", 
                               size=0.25, center=0) +
                geom_vline(xintercept=max_error, color=PRESTO_PALETTE["red"], 
                           size=0.5, linetype=3)
        } else if (style == "prfreq") {
            # Check primer fields and calculate PRFREQ and PRCONS if needed
            log_df <- .calcPrimerFreq(log_df)
            
            # Plot majority primer frequency    
            p1 <- ggplot(log_df, aes(x=PRFREQ)) +
                base_theme +
                ggtitle(titles[i]) +
                xlab("Primer frequency") +
                ylab("Number of UMIs") +
                scale_x_continuous(limits=c(-0.05, 1.05), breaks=seq(0.0, 1.0, 0.2)) +
                scale_y_continuous(labels=scientific) + 
                geom_histogram(binwidth=0.025, fill=PRESTO_PALETTE["blue"], colour="white", 
                               size=0.25, center=0) +
                geom_vline(xintercept=primer_freq, color=PRESTO_PALETTE["red"], 
                           size=0.5, linetype=3)
        } else if (style == "prsize") {
            # Check log table and calculate PRFREQ and PRCONS if needed
            check <- alakazam:::checkColumns(log_df, c("CONSCOUNT"))
            if (check != TRUE) { stop(check) }
            log_df <- .calcPrimerFreq(log_df)
            
            # Check if violin plot will work
            primer_tab <- log_df %>%
                group_by_("PRCONS") %>%
                dplyr::summarize(count=n())
            violin <- if (all(primer_tab$count >= 10)) { TRUE } else { FALSE }
            
            # Plot UMI size distribution by majority primer
            p1 <- ggplot(subset(log_df, PRFREQ >= primer_freq), aes(x=PRCONS, y=CONSCOUNT)) +
                base_theme + theme(legend.position="none") +
                ggtitle(titles[i]) +
                xlab("Primer") +
                ylab(paste0("Reads per UMI (PRFREQ >= ", primer_freq, ")")) +
                scale_y_continuous(trans=log2_trans(),
                                   breaks=trans_breaks("log2", function(x) 2^x),
                                   labels=trans_format("log2", math_format(2^.x)))
            if (violin) {
                p1 <- p1 + geom_violin(aes(fill=PRCONS), adjust=3.0, scale="width", 
                                       trim=T, width=0.7) +
                    geom_errorbarh(aes(xmin=(..x..) - 0.4, xmax=(..x..) + 0.4), size=1.5,
                                   stat="summary", fun.y="mean")
            } else {
                warning("Not enough data points for violin plot. Falling back on boxplot.")
                p1 <- p1 + geom_boxplot(aes(fill=PRCONS), width=0.7)
            }
            p1 <- p1 + geom_hline(yintercept=min_size, color=PRESTO_PALETTE["red"], 
                                  size=0.5, linetype=3)
        } else if (style == "prerror") {
            # Check log table and calculate PRFREQ and PRCONS if needed
            log_fields <- names(log_df)
            error_fields <- c("ERROR"="Error", "DIVERSITY"="Diversity")
            if ("ERROR" %in% log_fields) {
                # Check that error column is valid
                check <- alakazam:::checkColumns(log_df, "ERROR")
                if (check != TRUE) { stop(check) }
                f <- "ERROR"
            } else if ("DIVERSITY" %in% log_fields) {
                # Check that p-value column is valid
                check <- alakazam:::checkColumns(log_df, "DIVERSITY")
                if (check != TRUE) { stop(check) }
                f <- "DIVERSITY"
            } else {
                stop("Log table must contain one of fields 'ERROR' or 'DIVERSITY'.")
            }
            log_df <- .calcPrimerFreq(log_df)
            
            # Check if violin plot will work
            primer_tab <- log_df %>%
                group_by_("PRCONS") %>%
                dplyr::summarize(count=n())
            violin <- if (all(primer_tab$count >= 10)) { TRUE } else { FALSE }
            
            # Plot UMI error distribution by majority primer 
            p1 <- ggplot(subset(log_df, PRFREQ >= primer_freq), aes_string(x="PRCONS", y=f)) +
                base_theme + theme(legend.position="none") +
                ggtitle(titles[i]) +
                xlab("Primer") +
                ylab(paste0(error_fields[f], " (PRFREQ >= ", primer_freq, ")")) +
                scale_y_continuous(breaks=seq(0, 1, 0.1))
            if (violin) {
                p1 <- p1 + geom_violin(aes(fill=PRCONS), adjust=3.0, scale="width", 
                                       trim=T, width=0.7) +
                    geom_errorbarh(aes(xmin=(..x..) - 0.4, xmax=(..x..) + 0.4), size=1.5,
                                   stat="summary", fun.y="mean")
            } else {
                warning("Not enough data points for violin plot. Falling back on boxplot.")
                p1 <- p1 + geom_boxplot(aes(fill=PRCONS), width=0.7)
            }
        } else {
            stop("Nothing to plot.")
        }
        
        plot_list[[i]] <- p1
    }
    
    # Plot
    do.call(gridPlot, args=c(plot_list, ncol=1))    
}


#' Plot AssemblePairs log table
#'
#' @param    ...          data.frames returned by loadLogTable to plot
#' @param    titles       vector of titles for each log in ...; 
#'                        if NULL the titles will be empty.
#' @param    style        type of plot to draw. One of:
#'                        \itemize{
#'                          \item \code{"error"}:     distribution of error percentage or 
#'                                                    percent identity.
#'                          \item \code{"pvalue"}:    distribution of p-value or e-value scores.
#'                          \item \code{"length"}:    distribution of assembly read lengths.
#'                          \item \code{"overlap"}:   distribution assembly overlaps.
#'                          \item \code{"hexerror"}:  hexbin plot of error or identity vs overlap length
#'                                                    with shading by count of assembled reads.
#'                          \item \code{"field"}:     number of passing and failing assemblies by
#'                                                    annotation value for the column specified
#'                                                    in the \code{field} argument.
#'                        }
#' @param    max_error    maximum error threshold for passing assembly in the 
#'                        align subcommand.
#' @param    pvalue       p-value threshold for passing assembly in the 
#'                        align subcommand.
#' @param    min_ident    minimum identity threshold for passing assemblies in the 
#'                        reference subcommand.
#' @param    evalue       e-value threshold for alignment in the reference subcommand.
#' @param    field        field name to plot assembly success rates for.
#' @param    sizing       defines the style and sizing of the theme. One of 
#'                        \code{c("figure", "window")} where \code{sizing="figure"} is appropriately
#'                        sized for pdf export at 7 to 7.5 inch width, and \code{sizing="window"}
#'                        is sized for an interactive session.
#'                  
#' @return   NULL
#' 
#' @family   pRESTO log plotting functions
#' 
#' @export
plotAssemblePairs <- function(..., titles=NULL, style=c("error", "pvalue", "length", "overlap", "hexerror", "field"), 
                              max_error=0.3, pvalue=1e-5, min_ident=0.5, evalue=1e-5, 
                              field="PRCONS", sizing=c("figure", "window")) {
    ## DEBUG
    # c("ID", "LENGTH", "OVERLAP", "PVALUE", "ERROR", "FIELDS1", "FIELDS2")
    # c("ID", "REFID", "LENGTH", "OVERLAP", "GAP", "EVALUE1", "EVALUE2", "IDENTITY", "FIELDS1", "FIELDS2")
    # log_df <- assembly_log_2
    # titles=rep("", 2); style=c("a"); max_error=0.3; pvalue=1e-5; min_ident=0.5; evalue=1e-5; field="PRCONS"
    
    # Parse arguments
    style <- match.arg(style)
    sizing <- match.arg(sizing)
    log_list <- list(...)
    log_count <- length(log_list)
    
    # Define titles
    if (is.null(titles)) {
        titles <- rep("", log_count)
    } else if (length(titles) != log_count) {
        stop("You must specify one title per input log table.")
    }
    
    # Set base plot settings
    base_theme <- alakazam:::getBaseTheme(sizing=sizing) +
        theme(legend.position="bottom")
    
    # Define plot objects for each log table
    plot_list <- list()
    for (i in 1:log_count) {
        log_df <- log_list[[i]]
        
        if (style == "error") {
            # Set field parameters and check validity of log table
            log_fields <- names(log_df)
            error_fields <- c("ERROR"="Error", "IDENTITY"="Identity")
            error_params <- c("ERROR"=max_error, "IDENTITY"=min_ident)
            f <- log_fields[log_fields %in% names(error_fields)]
            if (length(f) != 1) {
                stop("Log table must exactly one of the fields 'ERROR' or 'IDENTITY'.")
            }
            check <- alakazam:::checkColumns(log_df, f)
            if (check != TRUE) { stop(check) }
            
            # Plot assembly error
            log_df <- log_df[is.finite(log_df[[f]]), ]
            p1 <- ggplot(log_df, aes_string(x=f)) +
                base_theme + 
                ggtitle(titles[i]) +
                xlab(error_fields[f]) +
                ylab("Number of reads") +
                scale_x_continuous(limits=c(-0.05, 1.05), breaks=seq(0.0, 1.0, 0.2), labels=percent) +
                scale_y_continuous(labels=scientific) + 
                geom_histogram(binwidth=0.025, fill=PRESTO_PALETTE["blue"], color='white', 
                               size=0.25, center=0) +
                geom_vline(xintercept=error_params[f], color=PRESTO_PALETTE["red"], 
                           size=0.5, linetype=3)
        } else if (style == "pvalue") {
            # Set field parameters and check validity of log table
            log_fields <- names(log_df)
            pvalue_fields <- c("PVALUE"="P-Value", "EVALUE"="E-Value")
            pvalue_params <- c("PVALUE"=pvalue, "EVALUE"=evalue)
            if (all(c("EVALUE1", "EVALUE2") %in% log_fields)) {
                # Check that e-value columns are valid
                check <- alakazam:::checkColumns(log_df, c("EVALUE1", "EVALUE2"))
                if (check != TRUE) { stop(check) }
                # Melt E-values
                log_df <- log_df %>%
                    select_("EVALUE1", "EVALUE2") %>%
                    gather_(key_col="FILE", value_col="EVALUE", gather_cols=c("EVALUE1", "EVALUE2"))
                log_df$FILE <- translateStrings(log_df$FILE,
                                                c("Input File 1"="EVALUE1", "Input File 2"="EVALUE2"))
                f <- "EVALUE"
            } else if ("PVALUE" %in% log_fields) {
                # Check that p-value column is valid
                check <- alakazam:::checkColumns(log_df, c("PVALUE"))
                if (check != TRUE) { stop(check) }
                f <- "PVALUE"
            } else {
                stop("Log table must contain either the field 'PVALUE' or the
                     both of the fields 'EVALUE1' and 'EVALUE2'.")
            }
            
            # Build log10 transformed data and define plot settings
            log_df <- log_df[!is.na(log_df[, f]), ]
            log_df[, f] <- -log10(log_df[, f])
            x_binwidth <- diff(range(log_df[, f])) / 100
            #x_origin <- - x_binwidth / 2
            #x_range <- range(log_df[, f])
            #x_diff <- diff(x_range)
            #x_breaks <- seq(round_any(x_range[1], 10, f=ceiling), 
            #                round_any(x_range[2], 10, f=floor),
            #                round_any(x_diff/5, 10, f=floor))
            
            # Plot assembly p-value
            log_df <- log_df[is.finite(log_df[[f]]), ]
            p1 <- ggplot(log_df, aes_string(x=f)) +
                base_theme + 
                ggtitle(titles[i]) +
                xlab(pvalue_fields[f]) +
                ylab("Number of reads") +
                #scale_x_continuous(breaks=x_breaks,
                #                   labels=trans_format('identity', math_format(10^-.x))) + 
                scale_x_continuous(labels=trans_format('identity', math_format(10^-.x))) + 
                scale_y_continuous(labels=scientific)
            if (f == "PVALUE") {
                p1 <- p1 + geom_histogram(binwidth=x_binwidth, fill=PRESTO_PALETTE["blue"], 
                                          color='white', size=0.25, 
                                          center=0, position="identity")
            } else if (f == "EVALUE") {
                p1 <- p1 + geom_histogram(binwidth=x_binwidth, fill=PRESTO_PALETTE["blue"], 
                                          color='white', size=0.25, 
                                          center=0, position="identity") +
                    facet_grid(. ~ FILE)
            }
            p1 <- p1 + geom_vline(xintercept=-log10(pvalue_params[f]), color=PRESTO_PALETTE["red"], 
                                  size=0.5, linetype=3)
        } else if (style == "length") {
            # Check for valid log table
            check <- alakazam:::checkColumns(log_df, c("LENGTH"))
            if (check != TRUE) { stop(check) }
            
            # Plot assembly length
            #x_origin <- min(log_df$LENGTH, na.rm=TRUE) - 0.5
            log_df <- log_df[is.finite(log_df[["LENGTH"]]), ]
            p1 <- ggplot(log_df, aes(x=LENGTH)) +
                base_theme + 
                ggtitle(titles[i]) +
                xlab("Sequence length") +
                ylab("Number of reads") +
                scale_y_continuous(labels=scientific) + 
                geom_histogram(binwidth=1, fill=PRESTO_PALETTE["blue"], 
                               color=PRESTO_PALETTE["blue"], center=0)
        } else if (style == "overlap") {
            # Check for valid log table
            check <- alakazam:::checkColumns(log_df, c("OVERLAP"))
            if (check != TRUE) { stop(check) }
            
            # Plot assembly overlap
            #x_origin <- min(log_df$OVERLAP, na.rm=TRUE) - 0.5
            log_df <- log_df[is.finite(log_df[["OVERLAP"]]), ]
            p1 <- ggplot(log_df, aes(x=OVERLAP)) +
                base_theme + 
                ggtitle(titles[i]) +
                xlab("Nucleotides of overlap") +
                ylab("Number of reads") +
                scale_y_continuous(labels=scientific) + 
                geom_histogram(binwidth=1, fill=PRESTO_PALETTE["blue"], 
                               color=PRESTO_PALETTE["blue"], center=0)
            if (any(na.omit(log_df$OVERLAP) < 0)) {
                p1 <- p1 + geom_vline(xintercept=0, color=PRESTO_PALETTE["red"], 
                                      size=0.5, linetype=3)
            }
        } else if (style == "hexerror") {
            # Set field parameters and check validity of log table
            log_fields <- names(log_df)
            error_fields <- c("ERROR"="Error", "IDENTITY"="Identity")
            error_params <- c("ERROR"=max_error, "IDENTITY"=min_ident)
            f <- log_fields[log_fields %in% names(error_fields)]
            if (length(f) != 1) {
                stop("Log table must contain exactly one of the fields 'ERROR' or 'IDENTITY'.")
            }
            check <- alakazam:::checkColumns(log_df, c("OVERLAP", f))
            if (check != TRUE) { stop(check) }
            
            # Plot overlap vs assembly error
            p1 <- ggplot(subset(log_df, !is.na(OVERLAP)), aes_string(x="OVERLAP", y=f)) + 
                base_theme + 
                ggtitle(titles[i]) +
                xlab('Nucleotides of overlap') +
                ylab(error_fields[f]) +
                scale_y_continuous(labels=percent) +
                scale_fill_gradient2(name='Reads', low="white", high=PRESTO_PALETTE["dark_blue"], 
                                     trans="log10") +
                stat_binhex(bins=50)
            if (any(na.omit(log_df$OVERLAP) < 0)) {
                p1 <- p1 + geom_vline(xintercept=0, color=PRESTO_PALETTE["red"], 
                                      size=0.5, linetype=3)
            }
        } else if (style == "field") {
            # Check log table
            check <- alakazam:::checkColumns(log_df, c("FIELDS1", "FIELDS2"), logic="any")
            if (check != TRUE) { stop(check) }
            
            # Determine if assembly passed or failed
            log_fields <- names(log_df)
            if (all(c("ERROR", "PVALUE") %in% log_fields)) {
                # Check fields are valid
                check <- alakazam:::checkColumns(log_df, c("ERROR", "PVALUE"))
                if (check != TRUE) { stop(check) }
                # Assign result
                log_df$RESULT <- (log_df$PVALUE <= pvalue & log_df$ERROR <= max_error)
                log_df$RESULT[is.na(log_df$PVALUE) | is.na(log_df$ERROR)] <- FALSE
            } else if ("IDENTITY" %in% log_fields) {
                # Check fields are valid
                check <- alakazam:::checkColumns(log_df, c("IDENTITY"))
                if (check != TRUE) { stop(check) }
                # Assign result
                log_df$RESULT <- (log_df$IDENTITY >= min_ident)
                log_df$RESULT[is.na(log_df$IDENTITY)] <- FALSE 
            } else {
                stop("Log table must contain either the fields 'PVALUE' and 'ERROR' or 
                     the field 'IDENTITY'.") 
            }
            # Convert result to factor
            log_df$RESULT <- factor(log_df$RESULT, levels=c(TRUE, FALSE), labels=c("Pass", "Fail"))
            
            # Get field values
            field_regex <- paste0("(?<=", field, "=)[^\\|]+")
            log_df$VALUE1 <- stri_extract_first_regex(log_df$FIELDS1, field_regex)
            log_df$VALUE2 <- stri_extract_first_regex(log_df$FIELDS2, field_regex)
            
            # Remove columns with no matches
            value_fields <- c("VALUE1", "VALUE2")
            keep_fields <- sapply(log_df[, value_fields], function(x) !all(is.na(x)))
            value_fields <- value_fields[keep_fields]

            # Table field counts
            log_tab <- log_df %>%
                select_(.dots=c("RESULT", value_fields)) %>%
                gather_(key_col="FIELD",
                        value_col="VALUE",
                        gather_cols=value_fields) %>%
                group_by_("RESULT", "FIELD", "VALUE") %>%
                dplyr::summarize(COUNT=n())

            # Check for numeric and convert
            is_num <- suppressWarnings(all(!is.na(as.numeric(log_tab$VALUE))))
            if (is_num) { log_tab$VALUE <- as.numeric(log_tab$VALUE) }
            
            # Plot passed assembly by primer
            log_tab$FIELD <- translateStrings(as.character(log_tab$FIELD), 
                                              c("Input File 1"="VALUE1", "Input File 2"="VALUE2"))
            guide_values <- setNames(c(PRESTO_PALETTE["blue"], PRESTO_PALETTE["red"]), c("Pass", "Fail"))
            p1 <- ggplot(log_tab, aes(x=VALUE, y=COUNT, fill=RESULT)) + 
                base_theme + 
                ggtitle(titles[i]) +
                xlab(field) +
                ylab('Number of assembled reads') +
                scale_y_continuous(labels=scientific) +
                #scale_y_log10(breaks=trans_breaks('log10', function(x) 10^x),
                #              labels=trans_format('log10', math_format(10^.x))) + 
                scale_fill_manual(name="Result", values=guide_values) +
                geom_bar(aes(fill=RESULT), stat="identity", position="stack", width=0.7) +
                facet_grid(. ~ FIELD, scales="free_x")
        } else {
            stop("Nothing to plot.")
        }
        
        plot_list[[i]] <- p1
    }
    
    # Plot
    do.call(gridPlot, args=c(plot_list, ncol=1))    
}


#' Plot ParseHeaders log table
#'
#' @param    ...     data.frames returned by loadLogTable to plot
#' @param    titles  vector of titles for each log in ...; 
#'                   if NULL the titles will be empty.
#' @param    style   type of plot to draw. One of:
#'                   \itemize{
#'                     \item \code{"primer"}:  pie chart of primer usage.
#'                     \item \code{"count"}:   histogram of counts.
#'                   }
#' @param    primer  column name to plot when \code{style="primer"}.
#' @param    count   column name to plot when \code{style="count"}.
#' @param    sizing  defines the style and sizing of the theme. One of 
#'                   \code{c("figure", "window")} where \code{sizing="figure"} is appropriately
#'                   sized for pdf export at 7 to 7.5 inch width, and \code{sizing="window"}
#'                   is sized for an interactive session.
#' 
#' @return   NULL
#' 
#' @family   pRESTO log plotting functions
#' 
#' @export
plotParseHeaders <- function(..., titles=NULL, style=c("primer", "count"), 
                             primer="PRCONS", count="DUPCOUNT", sizing=c("figure", "window")) {
    ## DEBUG
    # c('PRCONS', 'CONSCOUNT', 'DUPCOUNT')
    # log_df <- parse_log_3
    # titles=NULL; primer="PRCONS"; count="DUPCOUNT"
    
    # Parse arguments
    style <- match.arg(style)
    sizing <- match.arg(sizing)
    log_list <- list(...)
    log_count <- length(log_list)
    
    # Define titles
    if (is.null(titles)) {
        titles <- rep("", log_count)
    } else if (length(titles) != log_count) {
        stop("You must specify one title per input log table.")
    }
    
    # Set base plot settings
    base_theme <- alakazam:::getBaseTheme(sizing=sizing)
    
    # Define plot objects for each log table
    plot_list <- list()
    for (i in 1:log_count) {
        log_df <- log_list[[i]]
        
        if (style == "primer") {
            # Check for valid log table
            check <- alakazam:::checkColumns(log_df, primer)
            if (check != TRUE) { stop(check) }
        
            # Plot primer abundance
            names(log_df)[names(log_df) == primer] <- "PRIMER"
            primer_tab <- log_df %>%
                group_by_("PRIMER") %>%
                dplyr::summarize(COUNT=n()) %>%
                ungroup() %>%
                dplyr::mutate_(FREQ=interp(~x/sum(x, na.rm=TRUE), x=as.name("COUNT")))
                
            guide_labels <- setNames(paste0(primer_tab$PRIMER, " (", primer_tab$COUNT, ")"), 
                                     primer_tab$PRIMER)
            p1 <- ggplot(primer_tab, aes(x="", y=FREQ)) +
                base_theme + 
                theme(panel.border=element_blank(), axis.ticks=element_blank()) + 
                ggtitle(titles[i]) +
                xlab("") +
                ylab("") +
                scale_fill_discrete(name="Primer", labels=guide_labels) +
                scale_y_continuous(labels=percent) +
                geom_bar(aes(fill=PRIMER), stat="identity", position="stack", width=1, 
                         size=0.25, color="white") +
                geom_text(aes(y=(cumsum(FREQ) - FREQ/2), label=scales::percent(FREQ)), 
                          size=rel(3)) +
                coord_polar(theta="y")
        } else if (style == "count") {
            # Check for valid log table
            check <- alakazam:::checkColumns(log_df, count)
            if (check != TRUE) { stop(check) }
            
            # Plot count distribution
            p1 <- ggplot(log_df, aes_string(x=count)) +
                base_theme +
                ggtitle(titles[i]) +
                xlab(count) +
                ylab("Number of sequences") +
                scale_y_sqrt(labels=scientific) + 
                scale_x_continuous(trans=log2_trans(),
                                   breaks = trans_breaks("log2", function(x) 2^x),
                                   labels = trans_format("log2", math_format(2^.x))) +
                geom_histogram(bins=50, fill=PRESTO_PALETTE["blue"], 
                               color=PRESTO_PALETTE["blue"], center=0)
        } else {
            stop("Nothing to plot.")
        }
        
        plot_list[[i]] <- p1
    }
    
    # Plot
    do.call(gridPlot, args=c(plot_list, ncol=1))
}
