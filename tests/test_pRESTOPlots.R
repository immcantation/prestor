# Alakazam pRESTO plotting demo
# @author  Jason Anthony Vander Heiden  
# @date    2015.04.06

#### Imports ####
library(alakazam)
library(presto)

#### Run parameters ####
data_path <- "/mnt/archive/oconnor_mg_memory"
sample_name <- "AB0RF_HD13P"
sample_path <- file.path(data_path, "presto", sample_name)
font=6

#### Plot Console Log ####
console_log <- loadConsoleLog(file.path(sample_path, "Pipeline.log"))
plotConsoleLog(console_log, font=font)

#### Plot FilterSeq ####
quality_log_1 <- loadLogTable(file.path(sample_path, "QualityLogR1_table.tab"))
quality_log_2 <- loadLogTable(file.path(sample_path, "QualityLogR2_table.tab"))
plotFilterSeq(quality_log_1, quality_log_2, titles=c("Read 1", "Read 2"), font=font)

#### Plot MaskPrimers ####
primer_log_1 <- loadLogTable(file.path(sample_path, "PrimerLogR1_table.tab"))
primer_log_2 <- loadLogTable(file.path(sample_path, "PrimerLogR2_table.tab"))
plotMaskPrimers(primer_log_1, primer_log_2, titles=c("Read 1", "Read 2"), 
                style="hist", font=font)
plotMaskPrimers(primer_log_1, primer_log_2, titles=c("Read 1", "Read 2"), 
                style="count", font=font)
plotMaskPrimers(primer_log_1, primer_log_2, titles=c("Read 1", "Read 2"), 
                style="error", font=font)
## This data lacks PRSTART. Add some dummy values as an example.
#primer_log_1$PRSTART <- 17
#primer_log_2$PRSTART <- 0
#plotMaskPrimers(primer_log_1, primer_log_2, titles=c("Read 1", "Read 2"), 
#                style="pos", font=font)

#### Plot BuildConsensus ####
consensus_log_1 <- loadLogTable(file.path(sample_path, "ConsensusLogR1_table.tab"))
consensus_log_2 <- loadLogTable(file.path(sample_path, "ConsensusLogR2_table.tab"))
plotBuildConsensus(consensus_log_1, consensus_log_2, titles=c("Read 1", "Read 2"), 
                   style="size", font=font)
plotBuildConsensus(consensus_log_1, consensus_log_2, titles=c("Read 1", "Read 2"), 
                   style="error", font=font)
plotBuildConsensus(consensus_log_1, consensus_log_2, titles=c("Read 1", "Read 2"), 
                   style="prfreq", font=font)
plotBuildConsensus(consensus_log_1, consensus_log_2, titles=c("Read 1", "Read 2"), 
                   style="prsize", font=font)
plotBuildConsensus(consensus_log_1, consensus_log_2, titles=c("Read 1", "Read 2"), 
                   style="prerror", font=font)

#### Plot AssemblePairs ####
assembly_log_1 <- loadLogTable(file.path(sample_path, "AssembleAlignLog_table.tab"))
assembly_log_2 <- loadLogTable(file.path(sample_path, "AssembleReferenceLog_table.tab"))
plotAssemblePairs(assembly_log_1, assembly_log_2, titles=c("Align", "Reference"), 
                  style="error", font=font)
plotAssemblePairs(assembly_log_1, assembly_log_2, titles=c("Align", "Reference"), 
                  style="pvalue", font=font)
plotAssemblePairs(assembly_log_1, assembly_log_2, titles=c("Align", "Reference"), 
                  style="length", font=font)
plotAssemblePairs(assembly_log_1, assembly_log_2, titles=c("Align", "Reference"), 
                  style="overlap", font=font)
plotAssemblePairs(assembly_log_1, assembly_log_2, titles=c("Align", "Reference"), 
                  style="hex", font=font)
plotAssemblePairs(assembly_log_1, assembly_log_2, titles=c("Align", "Reference"), 
                  style="field", field="PRCONS", font=font)
plotAssemblePairs(assembly_log_1, assembly_log_2, titles=c("Align", "Reference"), 
                  style="field", field="CONSCOUNT", font=font)

#### Plot ParseHeaders ####
parse_log_1 <- loadLogTable(file.path(sample_path, "Unique_headers.tab"))
parse_log_2 <- loadLogTable(file.path(sample_path, "UniqueAtleast2_headers.tab"))
plotParseHeaders(parse_log_1, parse_log_2, titles=c("Total", "Atleast 2 Reads"), 
                 style="primer", font=font) 
plotParseHeaders(parse_log_1, parse_log_2, titles=c("Total", "Atleast 2 Reads"), 
                 style="count", count="DUPCOUNT", font=font) 
plotParseHeaders(parse_log_1, parse_log_2, titles=c("Total", "Atleast 2 Reads"), 
                 style="count", count="CONSCOUNT", font=font) 

