
#Determine Shape of Tableaux
Shape <- function(){
  
  skel_lamda <- as.numeric(readline(prompt = "# of rows in the shape? : "))
  lamda = matrix(,nrow=1,ncol=skel_lamda)
  
  for(i in 1:skel_lamda){
    
    if(i == 1){
      row_size <- as.numeric(readline(paste0(prompt = "length of " ,i,"th row? :")))
      lamda[1,i] = row_size
    }
    
    else{
      
      repeat {
        row_size <- as.numeric(readline(paste0(prompt = "length of " ,i,"th row? :")))
        lamda[1,i] = row_size
        if(lamda[1,i-1] < lamda[1,i]){
          print(paste0("length has to be <= ",lamda[1,i-1]))
        }
        if(lamda[1,i-1] >= lamda[1,i]){
          break
        }
      }
    }

    

  }
  
  skel_tableaux = matrix(,nrow=skel_lamda,ncol=lamda[1,1])
  
  for(j in 1:length(lamda)){
    
    for(k in 1:lamda[1,j]){
      
      if(j==1){
        
        if(k == 1){
          skel_tableaux[j,k] <- as.numeric(readline(paste0(prompt = "put the value in (",j,",",k,")"," out of (",j,",",lamda[1,j],") : ")))
        }
        else{
          repeat{
            skel_tableaux[j,k] <- as.numeric(readline(paste0(prompt = "put the value in (",j,",",k,")"," out of (",j,",",lamda[1,j],") : ")))
            if(skel_tableaux[j,k-1]<=skel_tableaux[j,k]){
              break
            }
            else{
              print(paste0("value has to be <= ",skel_tableaux[j,k-1]))
            }
          }
        }
      }
      else{
        
        if(k == 1){
          repeat{
            skel_tableaux[j,k] <- as.numeric(readline(paste0(prompt = "put the value in (",j,",",k,")"," out of (",j,",",lamda[1,j],") : ")))
            if(skel_tableaux[j-1,k]>=skel_tableaux[j,k]){
              print("NO!")
              print(skel_tableaux)
            }
            else{
              break
            }
          }
        }
        else{
          repeat{
            skel_tableaux[j,k] <- as.numeric(readline(paste0(prompt = "put the value in (",j,",",k,")"," out of (",j,",",lamda[1,j],") : ")))
            if(skel_tableaux[j-1,k]>=skel_tableaux[j,k]|skel_tableaux[j,k-1]>skel_tableaux[j,k]){
              print("No!")
              print(skel_tableaux)
            }
            else{
              break
            }
          }
        }
      }
    }
  }
  skel_tableaux[is.na(skel_tableaux)]<-0
  return(skel_tableaux)
}

#Determine range
Range <- function(){
  value <- as.numeric(readline(prompt = "What is range of value ?:"))
  return(value)
}

Shape <- Shape()
Range <- Range()


sjdt_Algorithm <- function(tableaux,range){
  
  unsplit <- tableaux
  print(unsplit)
  uni_unsplit <-unique(unsplit)
  #print(uni_unsplit)
  
  I <-list()
  J <-list()
  A <-list()
  B <-list()
  C <-list()
  D <-list()
  

  
  for(l in 1:ncol(unsplit)){
    
    count = 0
    #full-slot
    I[[l]] <- matrix(,nrow=1,ncol=ncol(unsplit))
    #empty-slot
    J[[l]] <- matrix(,nrow=1,ncol=ncol(unsplit))
    

    for(m in 1:range){
      
      if(sum(uni_unsplit[,l]==m)==2){
        I[[l]][1,count] <- m
        count = count+1
      }
      else if(sum(uni_unsplit[,l]==m)==0){
        J[[l]][1,count] <- m
        count = count+1
      }
    }

  }
  
  #A
  A[[l]] <- matrix(,nrow=1,ncol=ncol(unsplit))
  #B
  B[[l]] <- matrix(,nrow=1,ncol=ncol(unsplit))
  #C
  C[[l]] <- matrix(,nrow=1,ncol=ncol(unsplit))
  #D
  D[[l]] <- matrix(,nrow=1,ncol=ncol(unsplit))
  
  
  for(n in 1:ncol(unsplit)){
    
    
    #Extract A and D
    A[[n]] <- subset(unsplit[,n],unsplit[,n]<0)
    D[[n]] <- subset(unsplit[,n],unsplit[,n]>0)
    
    #Extract B and C
    B[[n]] <- subset(unsplit[,n],unsplit[,n]<0)
    C[[n]] <- subset(unsplit[,n],unsplit[,n]>0)
    for(o in 1:ncol(I[[n]])){
      
      if(-I[[n]][,o] %in% B[[n]]){
        
        for(q in 1:col(J[[n]])){
          
          if(I[[n]][,o]<J[[n]][,q]){
            
            B[[n]][B[[n]] == -I[[n]][,o]] <- -J[[q]]
            C[[n]][C[[n]] == I[[n]][,o]] <- J[[q]]
            
          }
        }
      }
    }
    sort(B[[n]],decreasing = FALSE)
    sort(C[[n]],decreasing = FALSE)
    
  }
  
  

  print(B)
  print(C)
  return(J)
}

a <- sjdt_Algorithm(Shape,Range)

