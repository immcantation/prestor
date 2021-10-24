# Build AbSeq report
# dir("~/workspace/immcantation/tests/data")
# dir("~/workspace/immcantation/tests/data/logs-abseq")
buildReport(input_dir="~/workspace/immcantation/tests/data/logs-abseq", 
            output_dir="~/workspace/immcantation/tests/run/prestor-2021.10.24",
            sample="A7VDM",
            template="AbSeqV3", 
            config="~/workspace/immcantation/tests/data/report.yaml",
            quiet=F)
