#Determine initial Shape
Determine_Shape <- function(unsplit,setorder){
  
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
    
    #re-combine besed on co-admissible column part
    
    #re-combine besed on admissible column part
    Co_I_comp <-1
    Co_J_comp <-1
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
    a<-sort(a,decreasing=TRUE)
    b<-sort(b,decreasing=TRUE)
    Co_I[[1]][1,] <- a
    Co_J[[1]][1,] <- b

    new_A[[1]] <- components$B[[coordinate[,2]]]
    
    new_D[[1]] <- components$C[[coordinate[,2]]]
    
    if(length(Co_I[[1]][1,])!=0&&length(Co_J[[1]][1,])!=0){
      # Reconstruct column of T_n
      for(o in 1:ncol(Co_I[[1]])){
        
        if(-Co_I[[1]][1,o] %in% components$B[[coordinate[,2]]]){
          
          for(h in 1:ncol(Co_J[[1]])){
            if(Co_I[[1]][1,o]<Co_J[[1]][1,h]&&Co_J[[1]][1,h]<=setorder){
              
              new_A[[1]][new_A[[1]] == -Co_I[[1]][1,o]] <- -Co_J[[1]][1,h]
              
              new_D[[1]][new_D[[1]] == Co_I[[1]][,o]] <- Co_J[[1]][1,h]
              
              Co_J[[1]] <- Co_J[[1]][Co_J[[1]] != Co_J[[1]][1,h]]
              
            }
          }
        }
      }
    }

    sort(new_A[[1]],decreasing = FALSE)
    sort(new_D[[1]],decreasing = FALSE)
    
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
    print("INSERT")
    #T_n Col D insert
    components$D[[coordinate[,2]]] <- append(components$D[[coordinate[,2]]],tableaux[coordinate[,1],2*coordinate[,2]+1])
    print(components$D[[coordinate[,2]]])
    print(tableaux)
    #reorder D component
    sort(components$D[[coordinate[,2]]],decreasing = FALSE)
    #admissible column of T_n
    combined_left_T_n <- append(components$A[[coordinate[,2]]],components$D[[coordinate[,2]]])
    abs_combined_left_T_n <- abs(combined_left_T_n)
    #REMOVE T_n+1 element
    tableaux[coordinate[,1],2*coordinate[,2]+1] <- 0
    tableaux[coordinate[,1],2*coordinate[,2]+2] <- 0

    #re-combine besed on admissible column part
    Ad_I_comp <-1
    Ad_J_comp <-1
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
      }
    }
    
    sort(new_B[[1]],decreasing = FALSE)
    sort(new_C[[1]],decreasing = FALSE)
    
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
    
    #full-slot
    I[[l]] <- matrix(,nrow=1,ncol=1)
    #empty-slot
    J[[l]] <- matrix(,nrow=1,ncol=1)
    
    A[[l]] <- matrix(,nrow=1,ncol=ncol(tableaux))
    B[[l]] <- matrix(,nrow=1,ncol=ncol(tableaux))
    C[[l]] <- matrix(,nrow=1,ncol=ncol(tableaux))
    D[[l]] <- matrix(,nrow=1,ncol=ncol(tableaux))
    
    setorder <- setorder-1
    for(m in 1:setorder){
      if(sum(abs_tableaux[,l]==m)==2){
        I[[l]][1,] <- m
      }
      else if(sum(abs_tableaux[,l]==m)==0){
        J[[l]][1,] <- m
      }
    }
  }
  


  
  for(n in 1:ncol(tableaux)){
    
    #Extract A and D
    A[[n]] <- subset(tableaux[,n],tableaux[,n]<0)
    D[[n]] <- subset(tableaux[,n],tableaux[,n]>0)
    #Extract B and C
    B[[n]] <- subset(tableaux[,n],tableaux[,n]<0)
    C[[n]] <- subset(tableaux[,n],tableaux[,n]>0)
    
    if(is.na(I[[1]])==TRUE&&is.na(J[[1]])==TRUE){
      
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
    }
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
  print("Start")
  
  components <- components(tableaux,setorder)
  
  abs_tableaux <- abs(tableaux)
  
  split <- matrix(,nrow=nrow(tableaux),ncol=2*ncol(tableaux))
  
  for(u in 1:ncol(tableaux)){
    
    combined_left <- append(components$A[[u]],components$C[[u]])
    combined_right <- append(components$B[[u]],components$D[[u]])
    
    if(tableaux[coordinate[1,1],coordinate[1,2]]==0&&coordinate[,2]==u){
      
      combined_left <- append(0,combined_left)
      combined_right <- append(0,combined_right)
      
    }
    
    #무한루프
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
    
    #Determine coordinate and length of set
    if(reducesizecount ==1){
      
      ###
      reducesizecount <- reducesizecount+1
      ####
      
      #uni_unsplit
      uni_unsplit <- abs(unsplit)
      
      coordinate <- which(uni_unsplit==setorder,arr.ind=TRUE)
      coordinate <- subset(coordinate, coordinate[,1]==1)
      print(coordinate)
      print(nrow(coordinate))
      length_setorder <- nrow(coordinate)
      
      #Creating Sub-Tableaux and Skew-Tableaux
      #De-concini's part
      deconcini_part <- matrix(,nrow=nrow(unsplit),ncol=ncol(unsplit))
      deconcini_part <- unsplit
      for(i in 1:nrow(unsplit)){
        if(i!=1){
          deconcini_part[i,][uni_unsplit[i,] >= setorder] <- 0
        }
      }
      print("deconcini")
      print(deconcini_part)

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
          }
        }
      print(absolute_king)
      
      #flexible coordinate
      change_cord <- matrix(,nrow=1,ncol=1)
      change_cord <- coordinate

      ###############################################################################################
      #초기 tableaux seperation shape

      length_rows <- Determine_Shape(deconcini_part,setorder)
      length_cols <- ncol(length_rows)
      deconcini_part[uni_unsplit == setorder] <- 0
      


    }
    #일단 좌표 정하는 것까지는 성공함!
    
    else{

      change_cord <- matrix(,nrow=1,ncol=2)
      count <- 0
      #k빈칸이 없을 경우 out

        #-setorder 지점 재지정
        count <- count+1
        #이자리에 새로운 coordinate를 첨가해야됨(넣었음)
        change_cord[1,] <- coordinate[count,]

        #-setorder가 없을때 알고리즘 적용 필요 없으므로 out

        within <- 1
        #@@Setorder 부분을 어떻게 해야할지 방법을 찾아야됨(임시 방편으로 해결됨)
          while((-setorder %in% unsplit)==TRUE){
            
            #그자리에 생성된 setorder좌표 
            change_cord[1,] <- coordinate[count,]

            #작업하기 위한 splited form
            if(within==1){
              split <- Splited_Form(deconcini_part,setorder,coordinate)
              unsplit <- deconcini_part
              print(split)
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
              length_rows <- Determine_Shape(unsplit,setorder)
              length_cols <- ncol(length_rows)
              unsplit[uni_unsplit == setorder] <- 0
              
              split <- Splited_Form(unsplit,setorder,coordinate)

            }


            #oversize checker
            horizon_oversize <- FALSE
            vertical_oversize <- FALSE

           while(horizon_oversize==FALSE&&vertical_oversize==FALSE){

              if(length_cols != 1){

              ##row 끝지점일 때 horizontal move
              if(change_cord[1,1]==length_rows[,change_cord[1,2]]&&change_cord[1,1]<=length_rows[,change_cord[1,2]+1]){


                unsplit<-Unsplited_Form(split)

                unsplit[change_cord[1,1],change_cord[1,2]] <- unsplit[change_cord[1,1],change_cord[1,2]+1]
                unsplit[change_cord[1,1],change_cord[1,2]+1]<-0
                change_cord[1,2] <- change_cord[1,2]+1
                split <- Splited_Form(unsplit,setorder,coordinate)
                unsplit <- Unsplited_Form(split)

                if((change_cord[1,1]>length_rows[1,change_cord[1,2]])||((change_cord[1,1]==length_rows[1,change_cord[1,2]])&&(change_cord[1,2]==length_cols))){
                  horizon_oversize <- TRUE
                }
                
              }
              
              #column 끝지점일때
              else if(change_cord[1,2]==length_cols){

                split<- vertical_move(split,change_cord)
                
                unsplit <- Unsplited_Form(split)
                
                change_cord[1,1] <- change_cord[1,1]+1
                if((change_cord[1,2]==length_cols)&&(change_cord[1,1]==length_rows[1,change_cord[1,2]])){
                  horizon_oversize <- TRUE
                }
                
              }
              
              ##vertical_move
              else if((change_cord[1,2]!=length_cols)&&(change_cord[1,2]<length_rows[,change_cord[1,2]])&&(split[change_cord[1,1]+1,2*change_cord[1,2]]<=split[change_cord[1,1],2*change_cord[1,2]+1])){
                
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

                if((change_cord[1,2]==length_cols)&&(change_cord[1,1]==length_rows[1,change_cord[1,2]])){
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

          
       print(absolute_king)
       print(unsplit)
        
      }

      setorder <- setorder-1
      reducesizecount <- 1
    }
    
  }

  #setorder가 2일때 unsplit파트 다 채우기
  if(setorder ==1){
    deconcini_coordinate <- which(unsplit != 0,arr.ind=TRUE)

    
    if(nrow(deconcini_coordinate)!=0){
      for(d in 1:nrow(deconcini_coordinate)){
        absolute_king[deconcini_coordinate[d,1],deconcini_coordinate[d,2]] <- unsplit[deconcini_coordinate[d,1],deconcini_coordinate[d,2]]
      }
    }

  }

  print(absolute_king)
  print("king's tableaux")
  return(absolute_king)
} #End point of sjdt-Algorithm


newShape<-sjdt_Algorithm(Shape,Range,3)