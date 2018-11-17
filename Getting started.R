devtools::install_github("MarioGS/prepuzzle")
library(prepuzzle)

#Example 1
pk = as.data.frame(prepuzzle_pk(df = df_pc1,only_observations = F, lower_case = F))
dose = as.data.frame(prepuzzle_dose(df = df_ex1))
dm = as.data.frame(prepuzzle_dm(df = df_dm1, covariates = c("age","sex","race"),include_time = T))
vs = as.data.frame(prepuzzle_vs(df = df_vs1,include_time = T))
cov = prepuzzle_cov(dm=dm,
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

#Example 2
pk = prepuzzle_pk(df = df_pc4,only_observations = F, lower_case = T)
dose = as.data.frame(prepuzzle_dose(df = df_ex4, lower_case = T))
dm = as.data.frame(prepuzzle_dm(df = df_dm4, covariates = c("age","sex","race"),include_time = T, lower_case = T))
cov = prepuzzle_cov(dm=dm)

library(puzzle)
head(puzzle(directory=file.path(getwd()),
            order=c(1),
            datetimeformat = "%Y-%m-%dT%H:%M",
            pk=list(data=pk), 
            dose=list(data=dose),
            cov=list(data=cov),
            initialindex = 1,
            optionalcolumns = c("ENTITY","PERIOD","VISIT","STUDY"),
            fillcolumns = c("ENTITY","PERIOD","VISIT","STUDY")))

nonmem = puzzle(directory=file.path(getwd()),
                order=c(1),
                datetimeformat = "%Y-%m-%dT%H:%M",
                pk=list(data=pk), 
                dose=list(data=dose),
                cov=list(data=cov),
                initialindex = 1,
                optionalcolumns = c("ENTITY","PERIOD","VISIT","STUDY"),
                fillcolumns = c("ENTITY","PERIOD","VISIT","STUDY"))

