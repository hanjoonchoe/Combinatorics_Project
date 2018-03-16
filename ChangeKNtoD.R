#components
components <- function(tableaux,setorder){
  
  tableaux[is.na(tableaux)] <- 0
  components <- list()
  abs_tableaux <- abs(tableaux)
  
  #Extract each properties
  I <-list()
  J <-list()
  A <-list()
  B <-list()
  C <-list()
  D <-list()
  
  #Extract I and J property
  for(l in 1:ncol(tableaux)){
    
    a <- c()
    b <- c()
    for(m in 1:setorder){
      
      if(sum(abs_tableaux[,l]==m)==2){
        a <- c(a,m)
      }
      else if(sum(abs_tableaux[,l]==m)==0){
        b <- c(b,m)
      }
    }
    I[[l]] <- matrix(,nrow=1,ncol=length(a))
    J[[l]] <- matrix(,nrow=1,ncol=length(b))
    a<-sort(a,decreasing=TRUE)
    b<-sort(b,decreasing=TRUE)
    I[[l]][1,] <- a
    J[[l]][1,] <- b
    
    A[[l]] <- matrix(,nrow=1,ncol=ncol(tableaux))
    B[[l]] <- matrix(,nrow=1,ncol=ncol(tableaux))
    C[[l]] <- matrix(,nrow=1,ncol=ncol(tableaux))
    D[[l]] <- matrix(,nrow=1,ncol=ncol(tableaux))
    
  }
  
  for(n in 1:ncol(tableaux)){
    
    #Extract A and D
    A[[n]] <- subset(tableaux[,n],tableaux[,n]<0)
    D[[n]] <- subset(tableaux[,n],tableaux[,n]>0)
    #Extract B and C
    B[[n]] <- subset(tableaux[,n],tableaux[,n]<0)
    C[[n]] <- subset(tableaux[,n],tableaux[,n]>0)
    if(length(I[[n]][1,])!=0&&length(J[[n]][1,])!=0){
      
      # Reconstruct column of T_n
      for(o in 1:ncol(I[[n]])){
        
        if(-I[[n]][1,o] %in% B[n]){
          
          for(h in 1:ncol(J[[n]])){
            
            if(I[[n]][1,o]>J[[n]][1,h]&&J[[n]][1,h]<=setorder){
              
              B[[n]][B[[n]] == -I[[n]][1,o]] <- -J[[n]][1,h]
              
              C[[n]][C[[n]] == I[[n]][,o]] <- J[[n]][1,h]
              
              J[[n]] <- J[[n]][J[[n]] != J[[n]][1,h]]
              break
            }
          }
        }
      }
    }
    sort(B[[n]],decreasing = FALSE)
    sort(C[[n]],decreasing = FALSE)
  }
  
  return(list("A"= A,"B" = B,"C" = C,"D" = D,"I" = I,"J" = J))
  
}

#Make splited form
ChangeToD <- function(tableaux,setorder){
  
  components <- components(tableaux,setorder)
  tableaux[is.na(tableaux)] <- 0
  abs_co_col <- abs(tableaux)
  
  Co_I <- list()
  Co_J <- list()
  new_A <- list()
  new_D <- list()
  
  

  for(i in 1:ncol(tableaux)){
    
    new_A[[i]] <- matrix(,nrow=1,ncol=1)
    new_D[[i]] <- matrix(,nrow=1,ncol=1)
    Co_I[[i]] <- matrix(,nrow=1,ncol=1)
    Co_J[[i]] <- matrix(,nrow=1,ncol=1)
    
    new_A[[i]] <- subset(tableaux[,i],tableaux[,i]<0)
    new_D[[i]] <- subset(tableaux[,i],tableaux[,i]>0)
    
    a <- c()
    b <- c()
    
    for(k in 1:setorder){
      
      if(sum(abs_co_col[,i]==k)==2){
        a <- c(a,k)

      }
      else if(sum(abs_co_col[,i]==k)==0){
        b <- c(b,k)
      }
    }
    Co_I[[i]] <- matrix(,nrow=1,ncol=length(a))
    Co_J[[i]] <- matrix(,nrow=1,ncol=length(b))
    a<-sort(a,decreasing=FALSE)
    b<-sort(b,decreasing=FALSE)
    Co_I[[i]][1,] <- a
    Co_J[[i]][1,] <- b


    if(length(Co_I[[i]][1,])!=0&&length(Co_J[[i]][1,])!=0){
      
      
      for(o in 1:ncol(Co_I[[i]])){
        
        
        if(-Co_I[[i]][1,o] %in% new_A[[i]]){

          
          for(h in 1:ncol(Co_J[[i]])){
            
            
            if(Co_I[[i]][1,o]<Co_J[[i]][1,h]&&Co_J[[i]][1,h]<=setorder){
              
              new_A[[i]][new_A[[i]] == -Co_I[[i]][1,o]] <- -Co_J[[i]][1,h]
              
              new_D[[i]][new_D[[i]] == Co_I[[i]][1,o]] <- Co_J[[i]][1,h]

              
              Co_J[[i]] <- Co_J[[i]][Co_J[[1]] != Co_J[[i]][1,h]]
              
              break
            }
          }
        }
      }
      new_A[[i]] <- sort(new_A[[i]],decreasing = FALSE)
      new_D[[i]] <- sort(new_D[[i]],decreasing = FALSE)
      co_col <- append(new_A[[i]],new_D[[i]])
      co_col <- matrix(co_col, nrow=1, ncol=length(co_col))
      

      while(ncol(co_col)!=nrow(tableaux)){
        co_col <- cbind(co_col,0)
      }
      tableaux[,i] <-  co_col
    }
  }
  
  
  print(tableaux)
  return(tableaux)
}

#ChangeToD(Matrix[[182]],3)
D_Matrix <- list()
count <- 1
for(i in 1:length(Matrix)){
  print(count)
  D_Matrix[[i]] <- ChangeToD(Matrix[[i]],5)
  count <- count+1
  
}
