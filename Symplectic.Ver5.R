coco <-1
#initial shape
Determine_Shape <- function(unsplit,setorder,coordinate){
  
  #first put values in coordinate box for instance
  if(nrow(coordinate)!=0){
    for(i in 1:nrow(coordinate)){
      unsplit[coordinate[i,1],coordinate[i,2]] <- setorder
    }
  }
  length_rows <- matrix(,nrow=1,ncol=length(subset(unsplit[1,], unsplit[1,] !=0)))
  instant <- unsplit
  abs_instant <-abs(instant)
  
  for(j in 1:length(subset(unsplit[1,], unsplit[1,] !=0))){
    
    length_rows[,j] <- length(subset(unsplit[,j],unsplit[,j] !=0))
  }
  
  return(length_rows)
}
#initial Shape
Determine_Innershape <- function(unsplit,setorder,coordinate,cord_counting){
  
  #first put values in coordinate box for instance
  if(cord_counting!=0){
    for(i in 1:cord_counting){
      unsplit[coordinate[i,1],coordinate[i,2]] <- setorder
    }
  }
  length_rows <- matrix(,nrow=1,ncol=length(subset(unsplit[1,], unsplit[1,] !=0)))
  instant <- unsplit
  abs_instant <-abs(instant)
  
  for(j in 1:length(subset(unsplit[1,], unsplit[1,] !=0))){
    
    length_rows[,j] <- length(subset(unsplit[,j],unsplit[,j] !=0))
  }
  
  return(length_rows)
}

#horizontal move
horizontal_move <- function(tableaux,setorder,components,coordinate){
  
  Co_I <- list()
  Co_J <- list()
  Ad_I <- list()
  Ad_J <- list()
  new_A <- list()
  new_B <- list()
  new_C <- list()
  new_D <- list()
  
  Co_I[[1]] <- matrix(,nrow=1,ncol=1)
  Co_J[[1]] <- matrix(,nrow=1,ncol=1)
  new_A[[1]] <- matrix(,nrow=1,ncol=1)
  new_D[[1]] <- matrix(,nrow=1,ncol=1)
  new_B[[1]] <- matrix(,nrow=1,ncol=1)
  new_C[[1]] <- matrix(,nrow=1,ncol=1)
  #deleted part vertical move
  
  # Case : bar_n
  if(tableaux[coordinate[,1],2*coordinate[,2]+1]<0){
    #T_n Col B
    components$B[[coordinate[,2]]] <- append(components$B[[coordinate[,2]]],tableaux[coordinate[,1],2*coordinate[,2]+1])
    #reorder B component
    sort(components$B[[coordinate[,2]]],decreasing = FALSE)
    #Co-admissible column of T_n
    combined_right_T_n <- append(components$B[[coordinate[,2]]],components$C[[coordinate[,2]]])
    abs_combined_right_T_n <- abs(combined_right_T_n)

    #REMOVE T_n+1 element
    tableaux[coordinate[,1],2*coordinate[,2]+1] <- 0
    tableaux[coordinate[,1],2*coordinate[,2]+2] <- 0
    
    #re-combine besed on admissible column part
    a <- c()
    b <- c()
    for(k in 1:setorder){
      
      if(sum(abs_combined_right_T_n==k)==2){
        a <- c(a,k)
      }
      else if(sum(abs_combined_right_T_n==k)==0){
        b <- c(b,k)
      }
    }
    Co_I[[1]] <- matrix(,nrow=1,ncol=length(a))
    Co_J[[1]] <- matrix(,nrow=1,ncol=length(b))
    a<-sort(a,decreasing=FALSE)
    b<-sort(b,decreasing=FALSE)
    Co_I[[1]][1,] <- a
    Co_J[[1]][1,] <- b
    
    
    new_A[[1]] <- components$B[[coordinate[,2]]]
    new_D[[1]] <- components$C[[coordinate[,2]]]

    ###
    if(length(Co_I[[1]][1,])!=0&&length(Co_J[[1]][1,])!=0){
      
      # Reconstruct column of T_n
      for(o in 1:ncol(Co_I[[1]])){
        
        if(-Co_I[[1]][1,o] %in% components$B[[coordinate[,2]]]){
          
          for(h in 1:ncol(Co_J[[1]])){
            
            if(Co_I[[1]][1,o]<Co_J[[1]][1,h]&&Co_J[[1]][1,h]<=setorder){
              
              new_A[[1]][new_A[[1]] == -Co_I[[1]][1,o]] <- -Co_J[[1]][1,h]
              
              new_D[[1]][new_D[[1]] == Co_I[[1]][,o]] <- Co_J[[1]][1,h]
              
              Co_J[[1]] <- Co_J[[1]][Co_J[[1]] != Co_J[[1]][1,h]]
              
              break
            }
          }
        }
      }
    }

    ###
  
    
    new_A[[1]] <- sort(new_A[[1]],decreasing = FALSE)
    new_D[[1]] <- sort(new_D[[1]],decreasing = FALSE)
    components$B[[coordinate[,2]]] <- sort(components$B[[coordinate[,2]]],decreasing = FALSE)
    components$C[[coordinate[,2]]] <- sort(components$C[[coordinate[,2]]],decreasing = FALSE)

    new_left_Col_T_n <- append(new_A[[1]],components$C[[coordinate[,2]]])
    
    new_left_Col_T_n <- matrix(new_left_Col_T_n, nrow=1, ncol=length(new_left_Col_T_n))
    
    new_right_Col_T_n <- append(components$B[[coordinate[,2]]],new_D[[1]])
    
    new_right_Col_T_n <- matrix(new_right_Col_T_n, nrow=1, ncol=length(new_right_Col_T_n))

    #insert new value into the Column
    for(q in 1:ncol(new_left_Col_T_n)){
      
      tableaux[q,2*coordinate[,2]-1] <-  new_left_Col_T_n[1,q]
      tableaux[q,2*coordinate[,2]] <-  new_right_Col_T_n[1,q]
      
    }
  }
  
  # Case : n
  else if(tableaux[coordinate[,1],2*coordinate[,2]+1]>0){
    #T_n Col D insert
    components$D[[coordinate[,2]]] <- append(components$D[[coordinate[,2]]],tableaux[coordinate[,1],2*coordinate[,2]+1])
    #reorder D component
    sort(components$D[[coordinate[,2]]],decreasing = FALSE)
    #admissible column of T_n
    combined_left_T_n <- append(components$A[[coordinate[,2]]],components$D[[coordinate[,2]]])
    abs_combined_left_T_n <- abs(combined_left_T_n)
    #REMOVE T_n+1 element
    tableaux[coordinate[,1],2*coordinate[,2]+1] <- 0
    tableaux[coordinate[,1],2*coordinate[,2]+2] <- 0
    
    #re-combine besed on admissible column part
    a <- c()
    b <- c()
    for(k in 1:setorder){
      
      if(sum(abs_combined_left_T_n==k)==2){
        a <- c(a,k)
      }
      else if(sum(abs_combined_left_T_n==k)==0){
        b <- c(b,k)
      }
    }
    Ad_I[[1]] <- matrix(,nrow=1,ncol=length(a))
    Ad_J[[1]] <- matrix(,nrow=1,ncol=length(b))
    a<-sort(a,decreasing=TRUE)
    b<-sort(b,decreasing=TRUE)
    Ad_I[[1]][1,] <- a
    Ad_J[[1]][1,] <- b
    
    
    new_B[[1]] <- components$A[[coordinate[,2]]]
    new_C[[1]] <- components$D[[coordinate[,2]]]
    
    if(length(Ad_I[[1]][1,])!=0&&length(Ad_J[[1]][1,])!=0){
      # Reconstruct column of T_n
      for(o in 1:ncol(Ad_I[[1]])){
        
        if(-Ad_I[[1]][1,o] %in% components$A[[coordinate[,2]]]){
          
          for(h in 1:ncol(Ad_J[[1]])){
            if(Ad_I[[1]][1,o]>Ad_J[[1]][1,h]){
              new_B[[1]][new_B[[1]] == -Ad_I[[1]][1,o]] <- -Ad_J[[1]][1,h]
              
              new_C[[1]][new_C[[1]] == Ad_I[[1]][,o]] <- Ad_J[[1]][1,h]
              Ad_J[[1]] <- Ad_J[[1]][Ad_J[[1]] != Ad_J[[1]][1,h]]
              
            }
          }
        }
        break
      }
    }
    
    new_B[[1]] <- sort(new_B[[1]],decreasing = FALSE)
    new_C[[1]]<- sort(new_C[[1]],decreasing = FALSE)
    components$A[[coordinate[,2]]] <- sort(components$A[[coordinate[,2]]],decreasing = FALSE)
    components$D[[coordinate[,2]]] <- sort(components$D[[coordinate[,2]]],decreasing = FALSE)
    
    new_left_Col_T_n <- append(components$A[[coordinate[,2]]],new_C[[1]])
    
    new_left_Col_T_n <- matrix(new_left_Col_T_n, nrow=1, ncol=length(new_left_Col_T_n))
    
    new_right_Col_T_n <- append(new_B[[1]],components$D[[coordinate[,2]]])
    
    new_right_Col_T_n <- matrix(new_right_Col_T_n, nrow=1, ncol=length(new_right_Col_T_n))
    
    #insert new value into the Column
    for(q in 1:ncol(new_left_Col_T_n)){
      
      tableaux[q,2*coordinate[,2]-1] <-  new_left_Col_T_n[1,q]
      tableaux[q,2*coordinate[,2]] <-  new_right_Col_T_n[1,q]
      
    }
    
  }

  return(tableaux)
  
}

#components
components <- function(tableaux,setorder){
  
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
    
    #re-combine besed on admissible column part
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
    ###
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
    ##
    sort(B[[n]],decreasing = FALSE)
    sort(C[[n]],decreasing = FALSE)
  }
  
  return(list("A"= A,"B" = B,"C" = C,"D" = D,"I" = I,"J" = J))
  
}

#Vertical move
vertical_move <- function(tableaux,coordinate){
  
  #A/C column move
  tableaux[coordinate[,1],2*coordinate[,2]-1] <- tableaux[coordinate[,1]+1,2*coordinate[,2]-1]
  tableaux[coordinate[,1]+1,2*coordinate[,2]-1] <- 0
  
  #B/D column move
  tableaux[coordinate[,1],2*coordinate[,2]] <- tableaux[coordinate[,1]+1,2*coordinate[,2]]
  tableaux[coordinate[,1]+1,2*coordinate[,2]] <- 0
  
  return(tableaux)
  
}

#Make splited form
##여기서 자기전에 coordinate손좀 봐야될듯
Splited_Form <- function(tableaux,setorder,coordinate){

  components <- components(tableaux,setorder)

  abs_tableaux <- abs(tableaux)
  
  split <- matrix(,nrow=nrow(tableaux),ncol=2*ncol(tableaux))
  
  for(u in 1:ncol(tableaux)){
    
    combined_left <- append(components$A[[u]],components$C[[u]])
    combined_right <- append(components$B[[u]],components$D[[u]])
    
    if(u<=nrow(coordinate)){
      if(tableaux[coordinate[u,1],coordinate[u,2]]==0){
        
        combined_left <- append(0,combined_left)
        combined_right <- append(0,combined_right)
      }
      
    }
    
    
    while(length(combined_left)<nrow(tableaux)){
      combined_left <- append(combined_left,0)
    }
    while(length(combined_right)<nrow(tableaux)){
      combined_right <- append(combined_right,0)
    }
    
    split[,2*u-1] <- combined_left
    split[,2*u] <- combined_right
  }
  return(split)
}

#Make unsplited form
Unsplited_Form <- function(split_tableaux){
  
  unsplit_col <- ncol(split_tableaux)/2
  unsplit <- matrix(,nrow=nrow(split_tableaux),ncol=unsplit_col)
  for(u in 1:unsplit_col){
    
    for(v in 1:nrow(split_tableaux)){
      
      if(v==1){
        unsplit[v,u] <- split_tableaux[v,2*u-1]
        
      }
      else if(split_tableaux[v,2*u-1]<0){
        unsplit[v,u] <- split_tableaux[v,2*u-1]
      }
      else{
        unsplit[v,u] <- split_tableaux[v,2*u]
      }
      
    }
    #무한루프
  }
  
  return(unsplit)
}

#########
#Start value
reducesizecount <- 1
#########
absolute_king <- matrix(,nrow=nrow(Shape),ncol=ncol(Shape))

sjdt_Algorithm <- function(tableaux,range,setorder){
  
  unsplit <- tableaux
  uni_unsplit <-abs(unsplit)
  
  #setorder가 1일 경우 알고리즘 적용 필요 없으므로 out
  while(setorder>1){

    #setorder element 없을때 나감
    if((setorder %in% abs(unsplit))==FALSE){
      setorder <- setorder-1
    }

    
    #Determine coordinate and length of set
    else if(reducesizecount ==1){
      ###
      reducesizecount <- reducesizecount+1
      ####
      #uni_unsplit
      uni_unsplit <- abs(unsplit)
      
      coordinate <- which(unsplit==-setorder,arr.ind=TRUE)
      coordinate <- subset(coordinate, coordinate[,1]==1)
      length_setorder <- nrow(coordinate)

      #Creating Sub-Tableaux and Skew-Tableaux
      #De-concini's part
      deconcini_part <- matrix(,nrow=nrow(unsplit),ncol=ncol(unsplit))
      deconcini_part <- unsplit
      for(i in 1:nrow(unsplit)){
        if(i==1){
          deconcini_part[i,][uni_unsplit[i,] >= setorder] <- 0
          deconcini_part[i,][unsplit[i,] == setorder] <- 0
        }
        else{
          deconcini_part[i,][uni_unsplit[i,] >= setorder] <- 0
        }
      }

      #king's part
      king_part <- matrix(,nrow=nrow(unsplit),ncol=ncol(unsplit))
      king_part <- unsplit
      king_part[uni_unsplit<setorder] <- 0
      #이동에 문제생김 다시 봐야됨 
      
      if(nrow(coordinate)!=0){
        for(c in 1:nrow(coordinate)){
          king_part[coordinate[c,1],coordinate[c,2]] <-0
        }
      }
      
      king_coordinate <- matrix(,nrow=1,ncol=1)
      #absolute king part에 king부분 넣어주기
      king_coordinate <- which(king_part != 0,arr.ind=TRUE)
      
      if(nrow(king_coordinate)!=0){
        for(d in 1:nrow(king_coordinate)){
          absolute_king[king_coordinate[d,1],king_coordinate[d,2]] <- unsplit[king_coordinate[d,1],king_coordinate[d,2]]
        }
      }
      
      #flexible coordinate
      change_cord <- matrix(,nrow=1,ncol=1)
      change_cord <- coordinate
      
      ###############################################################################################
      #초기 tableaux seperation shape
      if(all(deconcini_part==0)==FALSE){
        length_rows <- Determine_Shape(deconcini_part,setorder,coordinate)
        length_cols <- ncol(length_rows)
        deconcini_part[uni_unsplit == setorder] <- 0
      }
      else{
        length_rows <- matrix(,nrow=1,ncol=1)
        length_rows <-0
        length_cols <-0
        deconcini_part[uni_unsplit == setorder] <- 0
      }
      
    }
    #일단 좌표 정하는 것까지는 성공함!
    
    else{
      #-setorder 지점 재지정
      #k빈칸이 없을 경우 out
      within <- 1
      cord_counting <- nrow(coordinate)
      
      while(cord_counting>0){
        
        change_cord <- matrix(,nrow=1,ncol=2)
        
        #이자리에 새로운 coordinate를 첨가해야됨(넣었음)
        change_cord[1,] <- coordinate[cord_counting,]

        #setorder가 없을때
        #setorder 부분을 어떻게 해야할지 방법을 찾아야됨(임시 방편으로 해결됨)
        while((-setorder %in% unsplit)==TRUE){
          
          instant_unsplit <- unsplit
          for(i in 1:cord_counting){
            instant_unsplit[coordinate[i,1],coordinate[i,2]]<-0
          }
          new_cord <- which(instant_unsplit==-setorder,arr.ind=TRUE)
          if(nrow(new_cord)!=0){
            coordinate <- rbind(coordinate,new_cord)
            cord_counting <- nrow(coordinate)

          }
          
          #그자리에 생성된 setorder좌표 
          change_cord[1,] <- coordinate[cord_counting,]

          #작업하기 위한 splited form
          if(within==1){
            split <- Splited_Form(deconcini_part,setorder,coordinate)
            unsplit <- deconcini_part
            within <- within+1
            

          }
          
          else if(within!=1){

            #새롭게 sub-tableaux 결정하기 전에 skew-part걸러내기
            uni_unsplit <- abs(unsplit)
            
            #king's part
            king_part <- matrix(,nrow=nrow(unsplit),ncol=ncol(unsplit))
            king_part <- unsplit
            king_part[uni_unsplit<setorder] <- 0

            for(c in 1:nrow(coordinate)){
              king_part[coordinate[c,1],coordinate[c,2]] <-0
            }

            king_coordinate <- matrix(,nrow=1,ncol=1)
            #absolute king part에 king부분 넣어주기
            king_coordinate <- which(king_part != 0,arr.ind=TRUE)
            
            if(nrow(king_coordinate)!=0){
              for(d in 1:nrow(king_coordinate)){
                absolute_king[king_coordinate[d,1],king_coordinate[d,2]] <- unsplit[king_coordinate[d,1],king_coordinate[d,2]]
                unsplit[king_coordinate[d,1],king_coordinate[d,2]] <-0
              }
            }


            #skew part잘라 냈으니 이제 쪼개기
            length_rows <- Determine_Innershape(unsplit,setorder,coordinate,cord_counting)
            length_cols <- ncol(length_rows)
            unsplit[uni_unsplit == setorder] <- 0
            split <- Splited_Form(unsplit,setorder,coordinate)

          }
          
          if(all(unsplit==0)==TRUE){
            
            absolute_king[change_cord[1,1],change_cord[1,2]] <- -setorder

            break
          }
          
          else{
            #oversize checker
            horizon_oversize <- FALSE
            vertical_oversize <- FALSE
            oversize <- FALSE
            
            while(horizon_oversize==FALSE&&vertical_oversize==FALSE&&oversize==FALSE){

              ads <- ads+1
              
              if(length_cols != 1){
                #row and  col point 둘다 끝일때
                if(change_cord[1,1]==length_rows[1,change_cord[1,2]]&&change_cord[1,2]==length_cols){
                  oversize <- TRUE
                }
                
                ##row 끝지점일 때 horizontal move
                else if(change_cord[1,1]==length_rows[,change_cord[1,2]]&&change_cord[1,1]<=length_rows[,change_cord[1,2]+1]){

                  #unsplit 부분 이동
                  unsplit<-Unsplited_Form(split)
                  unsplit[change_cord[1,1],change_cord[1,2]] <- unsplit[change_cord[1,1],change_cord[1,2]+1]
                  unsplit[change_cord[1,1],change_cord[1,2]+1]<-0
                  
                  #split 부분 이동
                  split[change_cord[1,1],2*change_cord[1,2]] <- split[change_cord[1,1],2*(change_cord[1,2]+1)]
                  split[change_cord[1,1],2*(change_cord[1,2]+1)] <- 0
                  split[change_cord[1,1],2*change_cord[1,2]-1] <- split[change_cord[1,1],2*(change_cord[1,2]+1)-1]
                  split[change_cord[1,1],2*(change_cord[1,2]+1)-1] <- 0
                  
                  ##coord change
                  change_cord[1,2] <- change_cord[1,2]+1
                  #split <- Splited_Form(unsplit,setorder,coordinate)
                  unsplit <- Unsplited_Form(split)
                  
                  if((change_cord[1,1]>length_rows[1,change_cord[1,2]])||((change_cord[1,1]==length_rows[1,change_cord[1,2]])&&(change_cord[1,2]==length_cols))||((change_cord[1,1]==length_rows[1,change_cord[1,2]])&&(change_cord[1,1]>length_rows[1,change_cord[1,2]+1]))){
                    horizon_oversize <- TRUE
                  }
                  
                }
                
                #column 끝지점일때 그리고 옆 column의 길이가 더 짧을때 vertical move만 허용됨 그냥
                else if(change_cord[1,2]==length_cols||change_cord[1,1]>length_rows[1,change_cord[1,2]+1]){
                  
                  unsplit[change_cord[1,1],change_cord[1,2]] <- unsplit[change_cord[1,1]+1,change_cord[1,2]]
                  unsplit[change_cord[1,1]+1,change_cord[1,2]] <- 0
                  
                  change_cord[1,1] <- change_cord[1,1]+1
                  if((change_cord[1,2]==length_cols)&&(change_cord[1,1]==length_rows[1,change_cord[1,2]])){
                    horizon_oversize <- TRUE
                  }
                  else if(change_cord[1,1]==length_rows[1,change_cord[1,2]]&&change_cord[1,1]>length_rows[1,change_cord[1,2]+1]){
                    horizon_oversize <- TRUE
                  }
                  
                }
                
                ##vertical_move
                #이부분 손봐야됨
                else if((change_cord[1,2]!=length_cols)&&(change_cord[1,1]<length_rows[,change_cord[1,2]])&&(split[change_cord[1,1]+1,2*change_cord[1,2]]<=split[change_cord[1,1],2*change_cord[1,2]+1])){
                  split<- vertical_move(split,change_cord)
                  unsplit <- Unsplited_Form(split)

                  change_cord[1,1] <- change_cord[1,1]+1

                  if((change_cord[1,1]>length_rows[1,change_cord[1,2]])||((change_cord[1,1]==length_rows[1,change_cord[1,2]])&&(change_cord[1,2]==length_cols))||(change_cord[1,1]==length_rows[1,change_cord[1,2]])&&(change_cord[1,1]>length_rows[1,change_cord[1,2]+1])){
                    vertical_oversize <- TRUE
                  }
                }
                
                ##horizontal_move
                else{

                  components<-components(unsplit,setorder)
                  split <- horizontal_move(split,setorder,components,change_cord)
                  change_cord[1,2] <- change_cord[1,2]+1
                  unsplit <- Unsplited_Form(split)

                  #이거하면 이상하게 생겨짐
                  #split <- Splited_Form(unsplit,setorder,coordinate)
                  
                  if((change_cord[1,2]==length_cols)&&(change_cord[1,1]==length_rows[1,change_cord[1,2]])||change_cord[1,1]==length_rows[1,change_cord[1,2]]&&change_cord[1,1]>length_rows[1,change_cord[1,2]+1]){
                    horizon_oversize <- TRUE
                  }
                }
              }
              
              #column이 하나일때 vertical move 해야됨
              else if(length_cols==1){
                split<- vertical_move(split,change_cord)
                unsplit <- Unsplited_Form(split)
                
                change_cord[1,1] <- change_cord[1,1]+1
                if((change_cord[1,1]>length_cols)){
                  vertical_oversize <- TRUE
                }
              }
            }
          }
          
          if(horizon_oversize==TRUE){
            
            if(change_cord[1,2]>length_cols){
              absolute_king[change_cord[1,1]-1,change_cord[1,2]] <- -setorder
            }
            
            else{
              absolute_king[change_cord[1,1],change_cord[1,2]] <- -setorder
            }
          }
          else if(vertical_oversize==TRUE){
            
            if(length_cols==1){
              absolute_king[change_cord[1,1],change_cord[1,2]] <- -setorder
            }
            else{
              
              if(change_cord[1,1]>length_rows[1,change_cord[1,2]+1]){
                absolute_king[change_cord[1,1],change_cord[1,2]] <- -setorder
              }
              
              else if((change_cord[1,1]==length_rows[1,change_cord[1,2]])&&(change_cord[1,2]==length_cols)){
                absolute_king[change_cord[1,1],change_cord[1,2]] <- -setorder
              }
            }
          }
          else if(oversize == TRUE){
            absolute_king[change_cord[1,1],change_cord[1,2]] <- -setorder

          }

          coco <- coco+1
          

        }
        #coordinate <- coordinate[-1,]
        cord_counting <- cord_counting-1
        if(cord_counting!=0){
          unsplit[coordinate[cord_counting,1],coordinate[cord_counting,2]] <- -setorder
        }
      }
      reducesizecount <- 1
      setorder <- setorder-1
    }
  }
  #setorder가 1일때 unsplit파트 다 채우기
  deconcini_coordinate <- which(unsplit != 0,arr.ind=TRUE)
    
  if(nrow(deconcini_coordinate)!=0){
    for(d in 1:nrow(deconcini_coordinate)){
      absolute_king[deconcini_coordinate[d,1],deconcini_coordinate[d,2]] <- unsplit[deconcini_coordinate[d,1],deconcini_coordinate[d,2]]
    }
  }
  
  print(absolute_king)
  print("king's tableaux")
  print(Shape)
  print("De concini's tableaux")
  return(absolute_king)
  
} #End point of sjdt-Algorithm

newShape<-sjdt_Algorithm(Shape,Range,3)

