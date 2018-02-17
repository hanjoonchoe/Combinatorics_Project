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
    
    if(setorder==0){
      break
    }
    
    unsplit <- tableaux
    print(unsplit)
    uni_unsplit <-abs(unsplit)
    #De-concini's part
    sub_tableaux <-matrix(,nrow=1,ncol=1)
    sub_tableaux <- subset(unsplit, uni_unsplit>0&&uni_unsplit<setorder+1)
    length_rows <- matrix(,nrow=1,ncol=ncol(sub_tableaux))
    
    # rows of sub_tab
    for(i in 1:ncol(sub_tableaux)){
      
      length_rows[,i] <- length(sub_tableaux[,i])
      
    }
    
    # cols of sub_tab
    length_cols <- matrix(,nrow=1,ncol=1)
    length_cols <- ncol(unsplit)

    
    I <-list()
    J <-list()
    A <-list()
    B <-list()
    C <-list()
    D <-list()
    
    
    
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
    
    
    #Movement section
    for(i in 1:ncol(unsplit)){
      
      A[[i]] <-A[[i]][A[[i]] != -setorder]
      B[[i]] <-B[[i]][B[[i]] != -setorder]
      C[[i]] <-C[[i]][C[[i]] != -setorder]
      D[[i]] <-D[[i]][D[[i]] != -setorder]
      I[[i]] <-I[[i]][C[[i]] != -setorder]
      J[[i]] <-J[[i]][D[[i]] != -setorder]
      
      split[split == -setorder] <- 0
    }
    
    print(split)
    
    unsplit[unsplit == -setorder] <- 0
    
    # column 이 1일 경우에 예외상황 고려 안함.
    first_row_unsplit <- unsplit[1,]
    index <- which(unsplit==0,arr.ind=TRUE)
    index <- subset(index, index[,1]==1)
    
    print(nrow(index))
    # if the elements in the list that has to be eliminated are exist
    if(nrow(index)!=0){
      
      #sorting에 문제 있을수도 있음
      index_cord <- nrow(index)
      change_cord <- matrix(,nrow=1,ncol=1)
      change_cord <- index

      
      #movement algorithm
      
      repeat{
        #vertical move
        if(split[change_cord[1,1]+1,2*change_cord[1,2]]<split[change_cord[1,1],2*change_cord[1,2]+1]){
          
          #A/C column move
          split[change_cord[1,1],2*change_cord[1,2]-1] <- split[change_cord[1,1]+1,2*change_cord[1,2]-1]
          split[change_cord[1,1]+1,2*change_cord[1,2]-1] <- 0
          #D[[l]]
          #B/D column move
          split[change_cord[1,1],2*change_cord[1,2]] <- split[change_cord[1,1]+1,2*change_cord[1,2]]
          split[change_cord[1,1]+1,2*change_cord[1,2]] <- 0
          
          # vertical coordinate +1
          change_cord[1,2] < - change_cord[1,2]+1
          
        }
        #horizontal move
        else {
          
          # Case : bar_n
          if(split[change_cord[1,1],2*change_cord[1,2]+1]<0){
            
            #T_n Col B/D insert
            
            
            B[[change_cord[1,2]]] <- append(B[[change_cord[1,2]]],split[change_cord[1,1],2*change_cord[1,2]+1])
            #T_n+1 Col A/C insert
            
            A[[change_cord[1,2]]] <- A[[change_cord[1,2]]][A[[change_cord[1,2]]] != split[change_cord[1,1],2*change_cord[1,2]+1]]

            
            #reorder B component
            sort(B[[change_cord[1,2]]],decreasing = FALSE)

            
            #Co-admissible column of T_n
            combined_right_T_n <- append(B[[change_cord[1,2]]],C[[change_cord[1,2]]])
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
            


            new_A[[1]] <- B[[change_cord[1,2]]]

            new_D[[1]] <- C[[change_cord[1,2]]]


            
            # Reconstruct column of T_n
            for(o in 1:ncol(Co_I[[1]])){
              
              if(-Co_I[[1]][1,o] %in% B[[change_cord[1,2]]]){
                
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
            
            
            
            new_left_Col_T_n <- append(new_A[[1]],C[[1]])
            
            new_left_Col_T_n <- matrix(new_left_Col_T_n, nrow=1, ncol=length(new_left_Col_T_n))
            
            new_right_Col_T_n <- append(B[[1]],new_D[[1]])
            
            new_right_Col_T_n <- matrix(new_right_Col_T_n, nrow=1, ncol=length(new_right_Col_T_n))
            
            
            print(new_left_Col_T_n)
            print(new_right_Col_T_n)
            #insert new value into the Column
            for(q in 1:ncol(new_left_Col_T_n)){
              
              split[q,2*change_cord[1,2]-1] <-  new_left_Col_T_n[1,q]
              split[q,2*change_cord[1,2]] <-  new_right_Col_T_n[1,q]
              
            }
            
            change_cord[1,1] < - change_cord[1,1]+1

            print(split)
            while(TRUE){
              
            }
            
          }
          
          # Case : n
          else if(split[change_cord[1,1],2*change_cord[1,2]+1]>0){
            
          }
        }
        #exit with vertical or horizontal move
        if(change_cord[1,1]>=length_rows[,change_cord[1,2]]||change_cord[1,2]>=length_cols){
          
          if(change_cord[1,1]>=length_rows[,change_cord[1,2]]||change_cord[1,2]>=length_cols){
            #-setorder값 넣기
          }
          else{
            break
          }
        }
      }
      
      
      
    }
    print(split)
    sjdt_Algorithm(Shape,Range,setorder-1)
    
    
    
    return(split)
  }
  
}

a <- sjdt_Algorithm(Shape,Range,3)
