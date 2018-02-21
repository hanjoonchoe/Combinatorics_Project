#Determine initial Shape
Determine_Shape <- function(unsplit,setorder){
  
  length_rows <- matrix(,nrow=1,ncol=length(subset(unsplit[1,], unsplit[1,] < setorder)))
  instant <- unsplit
  instant[1,][instant[1,]==-setorder] <-0
  abs_instant <-abs(instant)
  
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
  
  return(length_rows)
}

#Vertical move
vertical_move <- function(tableaux,rows,columns){
  
  #A/C column move
  tableaux[rows,2*columns-1] <- tableaux[rows+1,2*columns-1]
  tableaux[rows+1,2*columns-1] <- 0
  
  #B/D column move
  tableaux[rows,2*columns] <- tableaux[rows+1,2*columns]
  tableaux[rows+1,2*columns] <- 0
  
  return(tableaux)
  
}

#Make splited column
Splited_Form <- function(tableaux,setorder,coordinate){
  print("Start")
  for(c in 1:nrow(coordinate)){
    
    tableaux[coordinate[c,1],coordinate[c,2]] <-0
  }
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
    
    for(m in 1:setorder-1){
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
  
  split <- matrix(,nrow=nrow(tableaux),ncol=2*ncol(tableaux))
  for(u in 1:ncol(tableaux)){
    
    combined_left <- append(A[[u]],C[[u]])
    combined_right <- append(B[[u]],D[[u]])
 
    if(coordinate[,2]==u){
      
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

#########
#Start value
reducesizecount <- 1
#########

sjdt_Algorithm <- function(tableaux,range,setorder){
  
  # Setorder 사이즈 줄이는 recursive
 repeat{
   
   #Import tableaux
   unsplit <- tableaux
   uni_unsplit <-abs(unsplit)
   
   #setorder가 1일 경우 알고리즘 적용 필요 없으므로 out
   if(setorder==1){
     
     break
     
   }
   
   #-setorder가 없을때 알고리즘 적용 필요 없으므로 out
   else if((-setorder %in% Shape)==FALSE){
     
     setorder = setorder-1
     
     split<- sjdt_Algorithm(tableaux,range,setorder)
       
   }
   
   #Determine coordinate and length of set
   else if(reducesizecount ==1){
     coordinate <- which(unsplit==-setorder,arr.ind=TRUE)
     length_setorder <- nrow(coordinate)
     reducesizecount == reducesizecount+1
     
     #De-concini's part
     deconcini_part <- matrix(,nrow=nrow(unsplit),ncol=ncol(unsplit))
     deconcini_part <- unsplit
     for(i in 1:nrow(unsplit)){
       if(i!=1){
         deconcini_part[i,][uni_unsplit[i,] == 3] <- 0
       }
     }
     print(deconcini_part)
     
     #king's part
     king_part <- matrix(,nrow=nrow(unsplit),ncol=ncol(unsplit))
     king_part <- unsplit
     king_part[uni_unsplit<3] <- 0
     
     print(king_part)
     
     
     ###############################################################################################
     
     #초기 tableaux seperation shape
     
     length_rows <- Determine_Shape(unsplit,setorder)
     length_cols <- ncol(length_rows)
     
     split<-Splited_Form(deconcini_part,setorder,coordinate)
     print("1")
     print(split)
     while(TRUE){
       
     }
   }
   
   else if(reducesizecount!=1){
     #De-concini's part
     deconcini_part <- matrix(,nrow=nrow(unsplit),ncol=ncol(unsplit))
     deconcini_part <- unsplit
     deconcini_part[uni_unsplit == 3] <- 0
     
     #king's part
     king_part <- matrix(,nrow=nrow(unsplit),ncol=ncol(unsplit))
     king_part <- unsplit
     king_part[uni_unsplit<3] <- 0
     
     
     print(deconcini_part)
     print(king_part)
   }
   
   
   
   # 그 단계 빈칸 k가 다 사라질 때 까지
   repeat{
     
     #k빈칸이 없을 경우 out
     if(nrow(coordinate)==0){
       
       reducesizecount <- 1
       
       break
     }
     
     else {
       
       ##vertical_move
       
       ##horizontal_move
       
       coordinate <- coordinate[-1,]
     }
     
     
     #알고리즘 적용후에 -setorder가 남아 있을 때
     repeat{
       
       if(FALSE){
         
         break
         
       }
       
       
       #알고리즘 적용후에 -setorder가 남아 있을 때 <end지점>
     }
     
     # 그 단계 빈간이 다 사라질 때 까지 <end지점>
   }
   
   # Setorder 사이즈 줄이는 recursive <end지점>   
 }
  
  #최종 output
  return(split)
}

sjdt_Algorithm(Shape,Range,3)