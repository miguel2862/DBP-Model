
Simulate.DBP <- function(NPEs, Connections, TimeSteps, HasITI,
                         mu.gaussian = 0.2,
                         sigma.gaussian = 0.15,
                         mu.log = 0.5,
                         sigma.log = 0.1){
  
  setClass("NPE", slots = c(Activation = "numeric",
                            PreviousActivation = "numeric",
                            ActivationDecay = "numeric",
                            ExcitatoryInput = "numeric",
                            InhibitoryInput="numeric",
                            PreviousExcitatoryInput = "numeric",
                            TemporalSummation = "numeric",
                            Name = "character",
                            Type = "character",
                            Layer = "character",
                            Threshold = "numeric",
                            InputConnections = "list",
                            r = "numeric"))
  
  setClass("Connection", slots = c(weight = "numeric",
                                   Name = "character",
                                   alpha = "numeric",
                                   beta = "numeric",
                                   alpha_prime = "numeric",
                                   beta_prime = "numeric",
                                   preSinapticNPE = "character",
                                   p = "numeric"))
  
  ComputeEXCInput <- function(npe){
    
    if(length(npe@InputConnections)>0){
      value <- 0
      
      for(i in 1:length(npe@InputConnections)){
        if(npe@InputConnections[[i]]@preSinapticNPE@Type == "Excitatory"){
          value = value + npe@InputConnections[[i]]@preSinapticNPE@Activation * npe@InputConnections[[i]]@weight
        }
      }
      
      return(value)
    }
    
  }
  
  ComputeInhInput <- function(npe){
    
    if(length(npe@InputConnections)>0){
      value <- 0
      
      for(i in 1:length(npe@InputConnections)){
        if(npe@InputConnections[[i]]@preSinapticNPE@Type == "Inhibitory"){
          value = value + npe@InputConnections[[i]]@preSinapticNPE@Activation * npe@InputConnections[[i]]@weight
        }
      }
      
      return(value)
    }
    
    
  }
  
  L <- function(x){
    
    return(1 / (1 + exp((-x + mu.log) / sigma.log)))
    
  }
  
  dVTA <- function(){
    
    d = 0
    n = 0
    for(i in 1:length(network)){
      
      if(network[[i]]@Layer == "Dopaminergic"){
        d = d + abs(network[[i]]@Activation-network[[i]]@PreviousActivation)
        n=n+1
      }
      
    }
    
    return(d/n)
  }
  
  dCA1 <- function(){
    d = 0
    n = 0
    for(i in 1:length(network)){
      
      if(network[[i]]@Layer == "Hippocampal"){
        d = d + abs(network[[i]]@Activation-network[[i]]@PreviousActivation)
        n=n+1
      }
      
    }
    
    return(d/n+dVTA()*(1-PreviousdCA1))
  }
  # are r and p computed after activations have been updated?
  Compute.r <- function(npe){
    
    
    sum.weights.exc <- 0
    
    sum.weights.inh<- 0
    
    #if(network[[npe]]@Name == "M.1" |network[[npe]]@Name == "D") browser()
    
    if(length(network[[npe]]@InputConnections)>0){
      for(i in 1:length(network[[npe]]@InputConnections)){
        pre <- network[[npe]]@InputConnections[[i]]@preSinapticNPE
        
        if (network[[pre]]@Type == "Excitatory"){
          sum.weights.exc <-  sum.weights.exc + ifelse(network[[pre]]@Layer == "US",0,network[[npe]]@InputConnections[[i]]@weight)
        }else{
          sum.weights.inh <-sum.weights.inh + network[[npe]]@InputConnections[[i]]@weight
        }
        
        
      }
    }
    
    return(c(1-sum.weights.exc, 1-sum.weights.inh))
  }
  
  Compute.p <- function(i,j){
    value <- c(0,0)
    for(conn in 1:length(network[[i]]@InputConnections)){
      pre <- network[[i]]@InputConnections[[conn]]@preSinapticNPE
      if(network[[pre]]@Type == "Excitatory"){
        
        if(network[[pre]]@Layer != "US"){
          value[1] = value[1] + network[[pre]]@Activation * network[[i]]@InputConnections[[conn]]@weight  
        }
        
        
      }else{
        value[2] = value[2] + network[[pre]]@Activation * network[[i]]@InputConnections[[conn]]@weight
      }
    }
    network[[i]]@ExcitatoryInput = value[1]
    network[[i]]@InhibitoryInput = value[2]
    
    value<-0
    pre <- network[[i]]@InputConnections[[j]]@preSinapticNPE
    
    if(network[[pre]]@Type== "Excitatory"){
      if(network[[i]]@ExcitatoryInput > 0){
        value <- network[[pre]]@Activation*network[[i]]@InputConnections[[j]]@weight/network[[i]]@ExcitatoryInput
      }
    }else{
      if(network[[i]]@InhibitoryInput > 0){
        value <- network[[pre]]@Activation*network[[i]]@InputConnections[[j]]@weight/network[[i]]@InhibitoryInput
      }
    }
    
    return(value)
  }
  
  data.sim <- as.data.frame(matrix(nrow = nrow(TimeSteps), ncol = nrow(NPEs) + nrow(Connections) + 3))
  
  colnames(data.sim) <- c("Phase","Trials","TimeSteps",NPEs[,1],paste(Connections[,1],Connections[,2], sep = "-"))
  
  ### Initialize network with NPEs and connections
  network <- list()
  
  for(i in 1:nrow(NPEs)){
    
    network[[NPEs[i,1]]]<- new("NPE", Name=NPEs[i,1],
                               Type = NPEs[i,2],
                               Layer = NPEs[i,3],
                               Activation = NPEs[i,4],
                               TemporalSummation = NPEs[i,5],
                               ActivationDecay = NPEs[i,6],
                               PreviousExcitatoryInput = 0,
                               ExcitatoryInput =0,
                               InhibitoryInput = 0)
    
  }
  
  for(i in 1:nrow(Connections)){
    connection.name <- paste(Connections[i,1],Connections[i,2],sep = "-")
    currentPostSinapticNPE <- Connections[i,2]
    network[[currentPostSinapticNPE]]@InputConnections[[connection.name]]<- new("Connection", Name=connection.name,
                                                                                weight = Connections[i,3],
                                                                                alpha = Connections[i,4],
                                                                                beta = Connections[i,5],
                                                                                alpha_prime = Connections[i,6],
                                                                                beta_prime = Connections[i,7],
                                                                                preSinapticNPE = Connections[i,1])
    
  }
  
  LearningRuleIsActive = F
  PreviousdCA1 = 0
  
  pb <- txtProgressBar(max = nrow(TimeSteps), style = 3)
  
  current.phase<- 1
  
  for(ts in 1:nrow(TimeSteps)){
    
    ### copy phase, trial and timestep
    
    data.sim[ts,1]<- TimeSteps[ts,1]
    
    if(ts>1 && TimeSteps[ts-1,1]!=TimeSteps[ts,1]){
      current.phase=current.phase+1
    }
    
    data.sim[ts,2]<- TimeSteps[ts,2]
    data.sim[ts,3]<- TimeSteps[ts,3]
    ##################
    # reset activations if HasITI[current.phase] is false and trial onset
    
    if(TimeSteps[ts,3]==1 & !HasITI[current.phase]){
      for(i in 1:length(network)){
        
        network[[i]]@Activation<- L(0)
      }
    }
    # set inputs
    
    LearningRuleIsActive <- as.logical(TimeSteps[ts,ncol(TimeSteps)])
    
    for(unit in seq(4,ncol(TimeSteps)-1,2)){
      
      network[[TimeSteps[ts,unit]]]@Activation <- TimeSteps[ts,unit+1]
      
    }
    
    
    #################
    # update activations to all members of the network list (scrambled)
    
    scrambledNPEs <- sample(1:length(network),length(network),replace = F)
    
    for(i in scrambledNPEs){
      # conditional activation
      if(network[["US"]]@Activation == 0){
        
        network[[i]]@Threshold <- rnorm(1,mu.gaussian,sigma.gaussian)
        
        network[[i]]@PreviousActivation <- network[[i]]@Activation
        
        if(length(network[[i]]@InputConnections)>0){
          value <- c(0,0)
          
          for(j in 1:length(network[[i]]@InputConnections)){
            pre <- network[[i]]@InputConnections[[j]]@preSinapticNPE
            if(network[[pre]]@Type == "Excitatory"){
              value[1] = value[1] + network[[pre]]@Activation * network[[i]]@InputConnections[[j]]@weight
            }else{
              value[2] = value[2] + network[[pre]]@Activation * network[[i]]@InputConnections[[j]]@weight
            }
          }
          network[[i]]@ExcitatoryInput = value[1]
          network[[i]]@InhibitoryInput = value[2]
        }else{
          network[[i]]@ExcitatoryInput = 0
          network[[i]]@InhibitoryInput = 0
        }
        
        
        p_epsp = L(network[[i]]@ExcitatoryInput)
        
        p_ipsp = ifelse(network[[i]]@InhibitoryInput == 0, 0, L(network[[i]]@InhibitoryInput))
        
        if(network[[i]]@Layer!= "PrimarySensory" & network[[i]]@Layer!= "US"){
          
          if(network[[i]]@ExcitatoryInput>network[[i]]@InhibitoryInput){
            
            if(p_epsp > network[[i]]@Threshold){
              #Reactivation
              #warning("reactivation")
              network[[i]]@Activation = p_epsp + network[[i]]@TemporalSummation * L(network[[i]]@PreviousExcitatoryInput) * (1 - p_epsp) - p_ipsp
            } else{
              # decay
              #warning("decay")
              network[[i]]@Activation = L(network[[i]]@PreviousExcitatoryInput) - network[[i]]@ActivationDecay * L(network[[i]]@PreviousExcitatoryInput)
            }
            
          }else{
            #inhibition
            #warning("inhibition")
            network[[i]]@Activation = 0
          }
          
          if(length(network[[i]]@Activation)==0) stop()
          
        }else{
          network[[i]]@Activation = network[[i]]@Activation
        }
      }
      else{
        #unconditional activation
        
        if(network[[i]]@Layer== "PrimaryMotor" | network[[i]]@Layer== "Dopaminergic"){
          
          is.connected.to.US <- F
          
          for(j in 1:length(network[[i]]@InputConnections)){
            pre <- network[[i]]@InputConnections[[j]]@preSinapticNPE
            if(network[[pre]]@Layer == "US"){
              is.connected.to.US <- T
              break
            }
          }
          
          if (is.connected.to.US){
            # unconditional activation
            network[[i]]@Activation = network[["US"]]@Activation
          }
          
          else{
            # conditional activation
            network[[i]]@Threshold <- rnorm(1,mu.gaussian,sigma.gaussian)
            
            network[[i]]@PreviousActivation <- network[[i]]@Activation
            
            if(length(network[[i]]@InputConnections)>0){
              value <- c(0,0)
              
              for(j in 1:length(network[[i]]@InputConnections)){
                pre <- network[[i]]@InputConnections[[j]]@preSinapticNPE
                if(network[[pre]]@Type == "Excitatory"){
                  value[1] = value[1] + network[[pre]]@Activation * network[[i]]@InputConnections[[j]]@weight
                }else{
                  value[2] = value[2] + network[[pre]]@Activation * network[[i]]@InputConnections[[j]]@weight
                }
              }
              network[[i]]@ExcitatoryInput = value[1]
              network[[i]]@InhibitoryInput = value[2]
            }else{
              network[[i]]@ExcitatoryInput = 0
              network[[i]]@InhibitoryInput = 0
            }
            
            p_epsp = L(network[[i]]@ExcitatoryInput)
            
            p_ipsp = ifelse(network[[i]]@InhibitoryInput == 0, 0, L(network[[i]]@InhibitoryInput))
            
            if(network[[i]]@Layer!= "PrimarySensory" & network[[i]]@Layer!= "US"){
              
              if(network[[i]]@ExcitatoryInput>network[[i]]@InhibitoryInput){
                
                if(p_epsp > network[[i]]@Threshold){
                  #Reactivation
                  #warning("reactivation")
                  network[[i]]@Activation = p_epsp + network[[i]]@TemporalSummation * L(network[[i]]@PreviousExcitatoryInput) * (1 - p_epsp) - p_ipsp
                } else{
                  # decay
                  #warning("decay")
                  network[[i]]@Activation = L(network[[i]]@PreviousExcitatoryInput) - network[[i]]@ActivationDecay * L(network[[i]]@PreviousExcitatoryInput)
                }
                
              }else{
                #inhibition
                #warning("inhibition")
                network[[i]]@Activation = 0
              }
              
            }else{
              network[[i]]@Activation = network[[i]]@Activation
            }
            
          }

        }
        else{
          # conditional activation
          network[[i]]@Threshold <- rnorm(1,mu.gaussian,sigma.gaussian)
          
          network[[i]]@PreviousActivation <- network[[i]]@Activation
          
          if(length(network[[i]]@InputConnections)>0){
            value <- c(0,0)
            
            for(j in 1:length(network[[i]]@InputConnections)){
              pre <- network[[i]]@InputConnections[[j]]@preSinapticNPE
              if(network[[pre]]@Type == "Excitatory"){
                value[1] = value[1] + network[[pre]]@Activation * network[[i]]@InputConnections[[j]]@weight
              }else{
                value[2] = value[2] + network[[pre]]@Activation * network[[i]]@InputConnections[[j]]@weight
              }
            }
            network[[i]]@ExcitatoryInput = value[1]
            network[[i]]@InhibitoryInput = value[2]
          }else{
            network[[i]]@ExcitatoryInput = 0
            network[[i]]@InhibitoryInput = 0
          }
          
          p_epsp = L(network[[i]]@ExcitatoryInput)
          
          p_ipsp = ifelse(network[[i]]@InhibitoryInput == 0, 0, L(network[[i]]@InhibitoryInput))
          
          if(network[[i]]@Layer!= "PrimarySensory" & network[[i]]@Layer!= "US"){
            
            if(network[[i]]@ExcitatoryInput>network[[i]]@InhibitoryInput){
              
              if(p_epsp > network[[i]]@Threshold){
                #Reactivation
                #warning("reactivation")
                network[[i]]@Activation = p_epsp + network[[i]]@TemporalSummation * L(network[[i]]@PreviousExcitatoryInput) * (1 - p_epsp) - p_ipsp
              } else{
                # decay
                #warning("decay")
                network[[i]]@Activation = L(network[[i]]@PreviousExcitatoryInput) - network[[i]]@ActivationDecay * L(network[[i]]@PreviousExcitatoryInput)
              }
              
            }else{
              #inhibition
              #warning("inhibition")
              network[[i]]@Activation = 0
            }
            
          }else{
            network[[i]]@Activation = network[[i]]@Activation
          }
        }
        
      }
      
      network[[i]]@PreviousExcitatoryInput = network[[i]]@ExcitatoryInput
      
      data.sim[ts,network[[i]]@Name] <- network[[i]]@Activation
      
    }
    
    
    ########################
    # update weights to all members of the inputConnections list of all the NPEs
    
    if(LearningRuleIsActive){
      
      dD= dVTA()
      
      dH = dCA1()
      
      PreviousdCA1 = dH
      
      scrambledNPEs <- sample(1:length(network),length(network),replace = F)
      
      for(i in scrambledNPEs){
        
        network[[i]]@r <- Compute.r(i)
        
        scrambleConnections <- sample(1:length(network[[i]]@InputConnections),length(network[[i]]@InputConnections),replace = F)
        
        for (j in scrambleConnections){
          
          network[[i]]@InputConnections[[j]]@p <- Compute.p(i,j)
          
          if(network[[i]]@Layer == "AssociativeMotor" | 
             network[[i]]@Layer == "PrimaryMotor" | 
             network[[i]]@Layer == "Dopaminergic"){
            d = dD
          }else{
            d = dH
          }
          
          pre <- network[[i]]@InputConnections[[j]]@preSinapticNPE
          
          if(network[[pre]]@Layer != "US"){
            
            if(LearningRuleIsActive){
              
              if(d>0.001){
                
                #if(network[[i]]@Name == "D") browser()
                
                network[[i]]@InputConnections[[j]]@weight = network[[i]]@InputConnections[[j]]@weight + 
                  ifelse(network[[pre]]@Type == "Excitatory", 
                         network[[i]]@InputConnections[[j]]@alpha*network[[i]]@r[1], 
                         network[[i]]@InputConnections[[j]]@alpha_prime*network[[i]]@r[2])*
                  network[[i]]@Activation*d*network[[i]]@InputConnections[[j]]@p
                
              }
              else{
                
                network[[i]]@InputConnections[[j]]@weight = network[[i]]@InputConnections[[j]]@weight - 
                  ifelse(network[[i]]@Type == "Excitatory",
                         network[[i]]@InputConnections[[j]]@beta, network[[i]]@InputConnections[[j]]@beta_prime)*
                  network[[i]]@InputConnections[[j]]@weight * network[[pre]]@Activation * 
                  network[[i]]@Activation
              }
              
            }
   
          }
          
          
          data.sim[ts,network[[i]]@InputConnections[[j]]@Name] <- network[[i]]@InputConnections[[j]]@weight
        }
        
      }
      
    }
    else{
      
      for(i in scrambledNPEs){
        
        scrambleConnections <- sample(1:length(network[[i]]@InputConnections),length(network[[i]]@InputConnections),replace = F)
        
        for (j in scrambleConnections){
          
          data.sim[ts,network[[i]]@InputConnections[[j]]@Name] <- network[[i]]@InputConnections[[j]]@weight
  
        }
          
          
          
      }
        
    }
    
    setTxtProgressBar(pb, ts) 
  }
  return(data.sim)
}
  

Create.Phases <- function(phases, trials){
  
  # phases is a character vector with comma delimited characters. For example,
  # "Random,A+/X-,100-100,False,30,30,ITI entrenamiento"
  # "In bulk,AX+,100,False,30,30,ITI entrenamiento"
  # "In bulk,Prueba X,25,False,30,30,ITI entrenamiento"
  
  #trials is a list with named elements
  # each element is a named vector of comma delimited characters
  # for example, trials[["A+"]] might be:
  
  #   [1] "S.1,1,S.2,0.65,S.3,0,US,0,True"
  #   [2] "S.1,1,S.2,0.65,S.3,0,US,0,True"
  #   [3] "S.1,1,S.2,0.65,S.3,0,US,0,True"
  #   [4] "S.1,1,S.2,0.65,S.3,0,US,0,True"
  #   [5] "S.1,1,S.2,0.65,S.3,0,US,1,True"
  
  timesteps <- as.data.frame(matrix(nrow=0,ncol = 3 + length(unlist(strsplit(trials[[1]][1],",")))))
  
  for(i in 1:length(phases)){
    
    current.ts <- 1
    current.trial <- 1
    
    current.phase <- trimws(unlist(strsplit(phases[i],",")))
    
    if(current.phase[1]== "In bulk"){
      
      trial.types <- unlist(strsplit(current.phase[2],"/"))
      
      if(sum(!trial.types %in% names(trials))>0) stop("One of more trial names do not match trial names in phases")
      
      trial.numbers <- unlist(strsplit(current.phase[3],"-"))
      
      for(j in 1:length(trial.types)){
        
        for(k in 1:trial.numbers[j]){
          
          if(current.phase[4] == "True"){
            
            # Has ITI
            
            current.ITI <- runif(1,as.numeric(current.phase[5]), as.numeric(current.phase[6]))
            
            for (ts in 1:current.ITI){
              
              timesteps<- rbind(timesteps,c(current.phase[2],current.trial, current.ts,trimws(unlist(strsplit(trials[[current.phase[7]]],",")))))
              current.ts=current.ts+1
            }
            
          }else{current.ts <- 1}
          
          for(ts in 1:length(trials[[trial.types[j]]])){
            timesteps<- rbind(timesteps,c(current.phase[2],current.trial, current.ts, trimws(unlist(strsplit(trials[[trial.types[j]]][ts],",")))))
            current.ts=current.ts+1
          }
          current.trial<- current.trial+1
          current.ts <- 1
        }
        
      }
      
      
    }else if(current.phase[1]== "Alternated"){
      
      trial.types <- unlist(strsplit(current.phase[2],"/"))
      if(sum(!trial.types %in% names(trials))>0) stop("One of more trial names do not match trial names in phases")
      trial.numbers <- unlist(strsplit(current.phase[3],"-"))
      
      for(j in 1:trial.numbers[1]){
        
        for(k in 1:length(trial.types)){
          if(current.phase[4] == "True"){
            
            # Has ITI
            
            current.ITI <- runif(1,as.numeric(current.phase[5]), as.numeric(current.phase[6]))
            
            for (ts in 1:current.ITI){
              
              timesteps<- rbind(timesteps,c(current.phase[2],current.trial, current.ts,trimws(unlist(strsplit(trials[[current.phase[7]]],",")))))
              current.ts=current.ts+1
            }
            
          }else{current.ts <- 1}
          
          for(ts in 1:length(trials[[trial.types[k]]])){
            timesteps<- rbind(timesteps,c(current.phase[2],current.trial, current.ts, trimws(unlist(strsplit(trials[[trial.types[k]]][ts],",")))))
            current.ts=current.ts+1
          }
          current.trial<- current.trial+1
          current.ts <- 1
        }
        
      }
      
    }else if(current.phase[1]== "Random"){
      
      trial.types <- unlist(strsplit(current.phase[2],"/"))
      if(sum(!trial.types %in% names(trials))>0) stop("One of more trial names do not match trial names in phases")
      trial.numbers <- unlist(strsplit(current.phase[3],"-"))
      
      trial.sequence <- vector(mode = "integer")
      
      for(n in 1:length(trial.numbers)){
        trial.sequence <- c(trial.sequence,rep(n,trial.numbers[n]))
      }
      
      trial.sequence <- sample(trial.sequence,length(trial.sequence),replace = F)
      
      for(j in trial.sequence){
        if(current.phase[4] == "True"){
          
          # Has ITI
          
          current.ITI <- runif(1,as.numeric(current.phase[5]), as.numeric(current.phase[6]))
          
          for (ts in 1:current.ITI){
            
            timesteps<- rbind(timesteps,c(current.phase[2],current.trial, current.ts,trimws(unlist(strsplit(trials[[current.phase[7]]],",")))))
            current.ts=current.ts+1
          }
          
        }else{current.ts <- 1}
        
        for(ts in 1:length(trials[[trial.types[j]]])){
          timesteps<- rbind(timesteps,c(current.phase[2],current.trial,current.ts, trimws(unlist(strsplit(trials[[trial.types[j]]][ts],",")))))
          current.ts=current.ts+1
        }
        current.trial<- current.trial+1
        current.ts <- 1
        
      }
      
    }
    
    
  }
  
  columns <- suppressWarnings(which(!is.na(sapply(timesteps[1,], as.numeric))==T))
  
  timesteps[,columns]<-as.data.frame(apply(timesteps[,columns],2,as.numeric))
  
  return(timesteps)
  
}

create.NPEs <- function(npe){
  
  NPEs <- as.data.frame(matrix(nrow = length(npe), ncol = 6))
  
  for(n in 1:length(npe)){
    NPEs[n,] <- trimws(unlist(strsplit(npe[n],",")))
  }
  columns <- suppressWarnings(which(!is.na(sapply(NPEs[1,], as.numeric))==T))
  
  NPEs[,columns]<-as.data.frame(apply(NPEs[,columns],2,as.numeric))
  colnames(NPEs) <- c("NPE", "Type", "Layer","Activation", "data.simoral.Summation","Activation.Decay" )
  return(NPEs)
}

create.Connections <- function(conn, NPEs){
  
  Connections <- as.data.frame(matrix(nrow = length(conn), ncol = 7))
  
  for(n in 1:length(conn)){
    Connections[n,] <- trimws(unlist(strsplit(conn[n],",")))
    
    if(any(!Connections[n,1:2] %in% NPEs)) stop("Either the preSinaptic or the postSinaptic NPE is not a member of the NPEs")
    
  }
  columns <- suppressWarnings(which(!is.na(sapply(Connections[1,], as.numeric))==T))
  
  Connections[,columns]<-as.data.frame(apply(Connections[,columns],2,as.numeric))
  colnames(Connections) <- c("PreSinapticNPE", "PostSinapticNPE", "Weight", "alpha", "beta", "alpha_prime", "beta_prime")
  return(Connections)
}


