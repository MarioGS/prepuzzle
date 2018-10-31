devtools::install_github("MarioGS/prepuzzle")
library(prepuzzle)
pk = as.data.frame(puzzle_pk(df = df_pc1,only_observations = T, lower_case = T))
dose = as.data.frame(puzzle_dose(df = df_ex1))
dm = as.data.frame(puzzle_dm(df = df_dm1, covariates = c("age","sex","race"),time_dependent_cov = T))
vs = as.data.frame(puzzle_vs(df = df_vs1,time_dependent_cov = T))
cov = puzzle_cov(dm=dm,
                 vs=vs)
library(puzzle)
nm = list()
nm$pk=list(drug=pk)
head(puzzle(directory=file.path(getwd()),
            order=c(1),
            datetimeformat = "%Y-%m-%dT%H:%M:%S",
            pk=list(data=nm$pk), 
            dose=list(data=dose),
            cov=list(data=cov),
            initialindex = 1,
            optionalcolumns = c("PERIOD","VISIT","STUDY"),
            fillcolumns = c("PERIOD","VISIT","STUDY")))

