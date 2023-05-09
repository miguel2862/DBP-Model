##############################################################################
# Simulation
# create NPEs and connections data frames

NPEs <- create.NPEs(c(
  "US,    Excitatory, US,                 0, 0.1, 0.1",
  "S.SS,  Excitatory, PrimarySensory,     0, 0.1, 0.1",
  "S.ctx, Excitatory, PrimarySensory,     0, 0.1, 0.1",
  "S.LL,  Excitatory, PrimarySensory,     0, 0.1, 0.1",
  "S..1,  Excitatory, AssociativeSensory, 0, 0.1, 0.1",
  "M..1,  Excitatory, AssociativeMotor,   0, 0.1, 0.1",
  "M.1,   Excitatory, PrimaryMotor,       0, 0.1, 0.1",
  "H1,    Excitatory, Hippocampal,        0, 0.1, 0.1",
  "S..2,  Excitatory, AssociativeSensory, 0, 0.1, 0.1",
  "M..2,  Excitatory, AssociativeMotor,   0, 0.1, 0.1",
  "M.2,   Excitatory, PrimaryMotor,       0, 0.1, 0.1",
  "M..3,  Inhibitory, PrimaryMotor,       0, 0.1, 0.1",
  "M..4,  Inhibitory, PrimaryMotor,       0, 0.1, 0.1",
  "H2,    Excitatory, Hippocampal,        0, 0.1, 0.1",
  "D,     Excitatory, Dopaminergic,       0, 0.1, 0.1"
))


Connections <- create.Connections(c(
  "S.SS, S..1, 0.1, 0.5, 0.1, 0.5, 0.1",
  "S..1, M..1, 0.1, 0.5, 0.1, 0.5, 0.1",
  "M..1, M.1,  0.1, 0.5, 0.1, 0.5, 0.1",
  "S..1, H1,   0.1, 0.5, 0.1, 0.5, 0.1",
  "M..1, D,    0.1, 0.5, 0.1, 0.5, 0.1",
  "US,   D,    1,   0.5, 0.1, 0.5, 0.1",
  "S.LL, S..2, 0.1, 0.5, 0.1, 0.5, 0.1",
  "S..2, M..2, 0.1, 0.5, 0.1, 0.5, 0.1",
  "M..2, M.2,  0.1, 0.5, 0.1, 0.5, 0.1",
  "M..2, D,    0.1, 0.5, 0.1, 0.5, 0.1",
  "S..2, H2,   0.1, 0.5, 0.1, 0.5, 0.1",
  "S.ctx,S..1, 0.1, 0.5, 0.1, 0.5, 0.1",
  "S.ctx,S..2, 0.1, 0.5, 0.1, 0.5, 0.1",
  "M..3, M.2,  0.1, 0.5, 0.1, 0.5, 0.1",
  "M..4, M.1,  0.1, 0.5, 0.1, 0.5, 0.1",
  "M.1,  M..3, 0.1, 0.5, 0.1, 0.5, 0.1",
  "M.2,  M..4, 0.1, 0.5, 0.1, 0.5, 0.1"
),NPEs[,1])

###############################################################################
#time steps creation

trials<- list()

trials[["SS"]] <- c(
  "S.SS,1,S.ctx,0.65,S.LL,0,US,0,True",
  "S.SS,1,S.ctx,0.65,S.LL,0,US,0,True",
  "S.SS,1,S.ctx,0.65,S.LL,0,US,0.7,True")
trials[["LL"]] <- c(
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,1,True")
trials[["Prueba de preferencia 1"]] <- c(
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False",
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False",
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False",
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False",
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False")
trials[["SS huella"]] <- c(
  "S.SS,1,S.ctx,0.65,S.LL,0,US,0,True",
  "S.SS,1,S.ctx,0.65,S.LL,0,US,0,True",
  "S.SS,1,S.ctx,0.65,S.LL,0,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,0,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,0,US,0.7,True")
trials[["LL huella"]] <- c(
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,1,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,0,US,0,True",
  "S.SS,0,S.ctx,0.65,S.LL,0,US,1,True")
trials[["Prueba de preferencia 2"]] <- c(
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False",
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False",
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False",
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False",
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False")
trials[["Prueba de preferencia 3"]] <- c(
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False",
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False",
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False",
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False",
  "S.SS,1,S.ctx,0.65,S.LL,1,US,0,False")
trials[["ITI entrenamiento"]]<- c("S.SS,0,S.ctx,0.65,S.LL,0,US,0,True")

for(i in 1:length(names(trials))){
  configuration.step <- unlist(strsplit(trials[[i]][1],","))
  
  if(any(!configuration.step[seq(1,length(configuration.step)-1,2)] %in% NPEs[,1])) stop("One or more NPEs in a trial configuration is not present in the NPEs data frame")
  
}


contingencies <-   c(
  "Random,SS/LL,100-100,True,30,30,ITI entrenamiento",
  "In bulk,Prueba de preferencia 1,25,False,30,30,ITI entrenamiento",
  "Random,SS huella/LL huella,100-100,True,30,30,ITI entrenamiento",
  "In bulk,Prueba de preferencia 2,25,False,30,30,ITI entrenamiento",
  "Random,SS/LL,100-100,True,30,30,ITI entrenamiento",
  "In bulk,Prueba de preferencia 3,25,False,30,30,ITI entrenamiento"
  )

HasITI <- sapply(contingencies, function(x) as.logical(unlist(strsplit(x,","))[4]))

TimeSteps <- Create.Phases(contingencies, trials)

datos.miguel<- Simulate.DBP(NPEs,Connections,TimeSteps,HasITI,
                                    mu.gaussian = 0.2,
                                    sigma.gaussian = 0.15,
                                    mu.log = 0.5,
                                    sigma.log = 0.1)

######################################################################

## plots

barplot(c(
  mean(datos.miguel[datos.miguel$Phase== "Prueba de preferencia 1" & datos.miguel$TimeStep == 4,"M.1"]),
  mean(datos.miguel[datos.miguel$Phase== "Prueba de preferencia 1" & datos.miguel$TimeStep == 4,"M.2"]),
  mean(datos.miguel[datos.miguel$Phase== "Prueba de preferencia 2" & datos.miguel$TimeStep == 4,"M.1"]),
  mean(datos.miguel[datos.miguel$Phase== "Prueba de preferencia 2" & datos.miguel$TimeStep == 4,"M.2"]),
  mean(datos.miguel[datos.miguel$Phase== "Prueba de preferencia 3" & datos.miguel$TimeStep == 4,"M.1"]),
  mean(datos.miguel[datos.miguel$Phase== "Prueba de preferencia 3" & datos.miguel$TimeStep == 4,"M.2"])
), ylim = c(0,1), col = c("gray", "black"), names.arg = rep(c("SS","LL"),3))

segments(x0 = c(2.5,4.9), y0 = c(0,0), x1 = c(2.5,4.9), y1= c(1,1), lty = 2)

#####################################################################
# Memory cleaning

rm(i)
rm(NPEs)
rm(Connections)
rm(HasITI)
rm(contingencies)
rm(TimeSteps)
rm(trials)

# All cleaning
rm(list = ls())


