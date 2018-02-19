##Movement function
subtableaux <- function(tableaux,rows,columns,setorder){
  
  tableaux[rows,2*columns] <- -setorder
  intermediate_unsplit <- matrix(,nrow=nrow(tableaux),ncol=(ncol(tableaux)/2))
  
  sub_A <-list()
  sub_B <-list()
  sub_C <-list()
  sub_D <-list()
  
  for(i in 1:(ncol(tableaux)/2)){
    
    for(j in 1:nrow(tableaux)){
      
      if(columns==i){
        
        if(columns==i&&rows<=j){
          
          intermediate_unsplit[j,i] <- tableaux[j,2*i]
          
        }
        
        else if(columns==i&&rows>j){
          
          if(tableaux[j,2*i-1]<0){
            
            intermediate_unsplit[j,i] <- tableaux[j,2*i-1]
            
          }
          else{
            
            intermediate_unsplit[j,i] <- tableaux[j,2*i]
            
          }
        }
      }
      
      else{
        
        if(tableaux[j,2*i-1]<0){
          
          intermediate_unsplit[j,i] <- tableaux[j,2*i-1]
          
        }
        else{
          intermediate_unsplit[j,i] <- tableaux[j,2*i]
        }
      }
      
    }
    
  }

  print(intermediate_unsplit)
  
}
vertical_move <- function(tableaux,rows,columns){
  
  #A/C column move
  tableaux[rows,2*columns-1] <- tableaux[rows+1,2*columns-1]
  tableaux[rows+1,2*columns-1] <- 0
  
  #B/D column move
  tableaux[rows,2*columns] <- tableaux[rows+1,2*columns]
  tableaux[rows+1,2*columns] <- 0

  return(tableaux)

}

horizontal_move <- function(tableaux,rows,columns,setorder,A,B,C,D){

  #deleted part vertical move
  # Case : bar_n
  if(tableaux[rows,2*columns+1]<0){
    
    #T_n Col B/D insert
    
    B <- append(B,tableaux[rows,2*columns+1])
    #T_n+1 Col A/C insert
    
    A <- A[A != tableaux[rows,2*columns+1]]
    
    #reorder B component
    sort(B,decreasing = FALSE)
    
    #Co-admissible column of T_n
    combined_right_T_n <- append(B,C)
    abs_combined_right_T_n <- abs(combined_right_T_n)
    Co_I <- list()
    Co_J <- list()
    new_A <- list()
    new_D <- list()
    
    Co_I[[1]] <- matrix(,nrow=1,ncol=1)
    Co_J[[1]] <- matrix(,nrow=1,ncol=1)
    new_A[[1]] <- matrix(,nrow=1,ncol=1)
    new_D[[1]] <- matrix(,nrow=1,ncol=1)
    
    #re-combine besed on co-admissible column part
    for(k in 1:setorder){
      
      if(sum(abs_combined_right_T_n==k)==2){
        Co_I[[1]][1,] <- k
        
      }
      else if(sum(abs_combined_right_T_n==k)==0){
        Co_J[[1]][1,] <- k
        
      }
    }
    
    sort(Co_I[[1]],decreasing=FALSE)
    sort(Co_J[[1]],decreasing=FALSE)
    
    new_A[[1]] <- B
    
    new_D[[1]] <- C
    
    # Reconstruct column of T_n
    for(o in 1:ncol(Co_I[[1]])){
      
      if(-Co_I[[1]][1,o] %in% B){
        
        for(h in 1:ncol(Co_J[[1]])){
          if(Co_I[[1]][1,o]<Co_J[[1]][1,h]&&Co_J[[1]][1,h]<=setorder){
            
            new_A[[1]][new_A[[1]] == -Co_I[[1]][1,o]] <- -Co_J[[1]][1,h]
            
            new_D[[1]][new_D[[1]] == Co_I[[1]][,o]] <- Co_J[[1]][1,h]
            
            Co_J[[1]] <- Co_J[[1]][Co_J[[1]] != Co_J[[1]][1,h]]
            
          }
        }
      }
    }
    sort(new_A[[1]],decreasing = FALSE)
    sort(new_D[[1]],decreasing = FALSE)
    
    new_left_Col_T_n <- append(new_A[[1]],C)
    
    new_left_Col_T_n <- matrix(new_left_Col_T_n, nrow=1, ncol=length(new_left_Col_T_n))
    
    new_right_Col_T_n <- append(B,new_D[[1]])
    
    new_right_Col_T_n <- matrix(new_right_Col_T_n, nrow=1, ncol=length(new_right_Col_T_n))
    
    #insert new value into the Column
    for(q in 1:ncol(new_left_Col_T_n)){
      
      tableaux[q,2*columns-1] <-  new_left_Col_T_n[1,q]
      tableaux[q,2*columns] <-  new_right_Col_T_n[1,q]
      
    }
    
  }
  
  # Case : n
  else if(tableaux[rows,2*columns+1]>0){
    
  }
  
  return(tableaux)
  
}


##Making Tableaux

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


sjdt_Algorithm <- function(tableaux,range,setorder){
  
  repeat{
    
    #Import tableaux
    unsplit <- tableaux
    uni_unsplit <-abs(unsplit)
    
    #seperate De-Concini's part to reduce order
    length_rows <- matrix(,nrow=1,ncol=length(subset(unsplit[1,], unsplit[1,] < setorder)))
    instant <- unsplit
    instant[1,][instant[1,]==-setorder] <-0
    abs_instant <-abs(instant)
    # rows of sub_tab of de concini
    
    for(j in 1:nrow(unsplit)){
      
      for(i in 1:ncol(length_rows)){
        
        if(j==1){
          length_rows[,i] <- length(subset(instant[j,i],instant[j,i]<setorder,instant[j,i] !=0))
        }
        else{
          length_rows[,i] <- length_rows[,i]+length(subset(abs_instant[j,i],abs_instant[j,i]<setorder,abs_instant[j,i] !=0))
        }
      }
    }

    print(unsplit)
    print(length_rows)

    # cols of sub_tab of de concini
    length_cols <- matrix(,nrow=1,ncol=1)
    length_cols <- ncol(unsplit)
    
    
    ########## #################### #################### #################### #################### ##########
    #Extract each properties
    I <-list()
    J <-list()
    A <-list()
    B <-list()
    C <-list()
    D <-list()
    
    #Extract I and J property
    for(l in 1:ncol(unsplit)){
      
      #full-slot
      I[[l]] <- matrix(,nrow=1,ncol=1)
      #empty-slot
      J[[l]] <- matrix(,nrow=1,ncol=1)
      
      
      for(m in 1:range){
        
        if(sum(uni_unsplit[,l]==m)==2){
          I[[l]][1,] <- m
        }
        else if(sum(uni_unsplit[,l]==m)==0){
          J[[l]][1,] <- m
        }
      }
    }
    
    A[[l]] <- matrix(,nrow=1,ncol=ncol(unsplit))
    B[[l]] <- matrix(,nrow=1,ncol=ncol(unsplit))
    C[[l]] <- matrix(,nrow=1,ncol=ncol(unsplit))
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
          
          for(q in 1:ncol(J[[n]])){
            
            if(I[[n]][,o]>J[[n]][,q]){
              
              B[[n]][B[[n]] == -I[[n]][,o]] <- -J[[n]][,q]
              
              C[[n]][C[[n]] == I[[n]][,o]] <- J[[n]][,q]
            }
          }
        }
      }
      sort(B[[n]],decreasing = FALSE)
      sort(C[[n]],decreasing = FALSE)
    }
    
    
    split <- matrix(,nrow=nrow(unsplit),ncol=2*ncol(unsplit))
    
    for(u in 1:ncol(unsplit)){
      
      combined_left <- append(A[[u]],C[[u]])
      combined_right <- append(B[[u]],D[[u]])
      while(length(combined_left)!=nrow(unsplit)){
        combined_left <- append(combined_left,0)
      }
      while(length(combined_right)!=nrow(unsplit)){
        combined_right <- append(combined_right,0)
      }
      split[,2*u-1] <- combined_left
      split[,2*u] <- combined_right
    }
    
    print(split)
    
    for(i in 1:ncol(unsplit)){
      
      A[[i]] <-A[[i]][A[[i]] != -setorder]
      B[[i]] <-B[[i]][B[[i]] != -setorder]
      C[[i]] <-C[[i]][C[[i]] != -setorder]
      D[[i]] <-D[[i]][D[[i]] != -setorder]
      I[[i]] <-I[[i]][C[[i]] != -setorder]
      J[[i]] <-J[[i]][D[[i]] != -setorder]
    }
    
    print(split)
    split[split == -setorder] <- 0
    unsplit[unsplit == -setorder] <- 0
    
    # column 이 1일 경우에 예외상황 고려 안함.
    first_row_unsplit <- unsplit[1,]
    index <- which(unsplit==0,arr.ind=TRUE)
    index <- subset(index, index[,1]==1)
    print(index)
    
    #sorting에 문제 있을수도 있음
    index_cord <- nrow(index)
    change_cord <- matrix(,nrow=1,ncol=1)
    change_cord <- index
    print(change_cord)
    
    ##Movement Algorithm  
    if(setorder == 0){
      break
    }
    
    else{
      
      repeat{
        
        #vertical move-primeter
        if(change_cord[1,1]<length_rows[,change_cord[1,2]]&&change_cord[1,2]==length_cols){
          
          split<- vertical_move(split,change_cord[1,1],change_cord[1,2])
          print("111")
          print(split)
          change_cord[1,1] <- change_cord[1,1]+1
          print(change_cord)

        }
        
        #vertical move-not perimeter
        else if((change_cord[1,2]!=length_cols)&&(change_cord[1,2]<length_rows[,change_cord[1,2]])&&(split[change_cord[1,1]+1,2*change_cord[1,2]]<=split[change_cord[1,1],2*change_cord[1,2]+1])){
          
          split<- vertical_move(split,change_cord[1,1],change_cord[1,2])
          print("111")
          print(split)
          change_cord[1,1] <- change_cord[1,1]+1
          print(length_cols)
          print(change_cord)
          while(TRUE){
            
          }
          
        }
        
        
        #break part
        else if(change_cord[1,1]==length_rows[,change_cord[1,2]]&&change_cord[1,2]==length_cols){
          
          split <- subtableaux(split,change_cord[1,1],change_cord[1,2],setorder)
          sjdt_Algorithm(split,Range,3)
          print(split)
          
          #여기서 다시시작해야됨
          while(TRUE){
            
          }
          
          break
 
        }
        
        #horizontal move
        else{
          split<- horizontal_move(split,change_cord[1,1],change_cord[1,2],setorder,A[[change_cord[1,2]]],B[[change_cord[1,2]]],C[[change_cord[1,2]]],D[[change_cord[1,2]]])
          change_cord[1,2] <- change_cord[1,2]+1
          
        }
      }
      ##
      ##
    }
  }
}

sjdt_Algorithm(Shape,Range,3)