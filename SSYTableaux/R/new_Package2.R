#Copyright 2018, HANJOON CHOE, All rights reserved.
King_Generator <- function(my.colSize,Range){

  if(which(rownames(installed.packages())=="gtools",arr.ind=TRUE)!=0){
    library(gtools)
  }
  else if(which(rownames(installed.packages())=="gtools",arr.ind=TRUE)==0){
    #install if necessary
    install.packages('gtools')
    #load library
    library(gtools)
  }

  my.numList <- -Range:Range
  my.numList <- my.numList[my.numList != 0]

  mydataK<-list()
  for(k in 1:length(my.colSize)){

    if(my.colSize[k]==1){

      KCol = matrix(, nrow=0, ncol=my.colSize[k])

      for(z in 1:length(my.numList)){

        KCol = rbind(KCol,my.numList[z])

      }

      mydataK[[k]] <-KCol

    }

    if (my.colSize[k]>1){

      permutation_result <- permutations(n=length(my.numList), r=my.colSize[k], v=my.numList, repeats.allowed =T)

      if(k == 1){


        SelectedPermutation_result = matrix(, nrow=0,ncol=my.colSize[k])


        for(i in 1:nrow(permutation_result)){

          j=1
          while(j<= my.colSize[k]-1&&abs(permutation_result[i,j])<=abs(permutation_result[i,j+1])){

            if(j==my.colSize[k]-1){

              co = j+1
              kj=2
              for(ki in 2:co){

                if(abs(permutation_result[i,ki])>=ki){

                  if(abs(permutation_result[i,ki-1])!=abs(permutation_result[i,ki])){

                    kj = kj+1

                  }

                  if(abs(permutation_result[i,ki-1])==abs(permutation_result[i,ki])){

                    if(permutation_result[i,ki-1]<permutation_result[i,ki]){

                      kj = kj+1
                    }
                  }
                }

                if(kj  == my.colSize[k]+1){

                  SelectedPermutation_result= rbind(SelectedPermutation_result,permutation_result[i,])
                }
              }
            }
            j=j+1
          }
        }
      }



      if(k>1){
        SelectedPermutation_result = matrix(, nrow=0,ncol=my.colSize[k])

        for(i in 1:nrow(permutation_result)){

          j=1
          while(j<= my.colSize[k]-1&&abs(permutation_result[i,j])<=abs(permutation_result[i,j+1])){

            if(j==my.colSize[k]-1){
              co = j+1
              kj=2
              for(ki in 2:co){

                if(abs(permutation_result[i,ki])>=ki){

                  if(abs(permutation_result[i,ki-1])!=abs(permutation_result[i,ki])){

                    kj = kj+1

                  }

                  if(abs(permutation_result[i,ki-1])==abs(permutation_result[i,ki])){

                    if(permutation_result[i,ki-1]<permutation_result[i,ki]){
                      kj = kj+1
                    }
                  }
                }

                if(kj  == my.colSize[k]+1){

                  SelectedPermutation_result= rbind(SelectedPermutation_result,permutation_result[i,])

                }
              }
            }
            j=j+1
          }
        }
      }

      mydataK[[k]]<-SelectedPermutation_result

    }
  }

  my.colNum = length(my.colSize)
  x = my.colNum

  My.K_Recording <- list()
  K_Combined <-list()

  if(x==1){

    K_Combined <- list()

  }

  if(x>1){

    u=1

    while(x>1){

      KCol_Prev = matrix(,nrow=0,ncol=ncol(mydataK[[x-1]]))
      KCol_Next = matrix(,nrow=0,ncol=ncol(mydataK[[x]]))

      if(x==my.colNum){

        K_Recording = matrix(, nrow=0,ncol=0)
        for(r in 1:nrow(mydataK[[x-1]])){

          for(v in 1:nrow(mydataK[[x]])){

            s = 1
            so = 1
            while(s<=ncol(mydataK[[x]])&&abs(mydataK[[x-1]][r,s])<=abs(mydataK[[x]][v,s])){

              if(abs(mydataK[[x-1]][r,s])!=abs(mydataK[[x]][v,s])){
                so=so+1

              }
              else if(abs(mydataK[[x-1]][r,s])==abs(mydataK[[x]][v,s])){
                if(mydataK[[x-1]][r,s]==mydataK[[x]][v,s]){
                  so=so+1
                }
                else if(mydataK[[x-1]][r,s]<mydataK[[x]][v,s]){
                  so=so+1
                }
              }
              if(so==ncol(mydataK[[x]])+1){

                KCol_Prev = rbind(KCol_Prev,mydataK[[x-1]][r,])
                KCol_Next = rbind(KCol_Next,mydataK[[x]][v,])
                K_Recording = c(K_Recording,r)
              }
              s = s+1
            }
          }
        }
        K_Recording = as.matrix(K_Recording)
        My.K_Recording[[u]] = K_Recording
        K_Combined[[u]] = cbind(KCol_Prev,KCol_Next)
        u = u+1
      }

      if(x!=my.colNum){

        Instant_KCombined = matrix(,nrow=0,ncol=ncol(K_Combined[[u-1]]))
        K_Recording = matrix(,nrow=0,ncol=0)
        for(w in 1:nrow(My.K_Recording[[u-1]])){

          for(v in 1:nrow(mydataK[[x-1]])){

            s=1
            so=1

            while(s<=ncol(mydataK[[x]])&&abs(mydataK[[x-1]][v,s])<=abs(mydataK[[x]][My.K_Recording[[u-1]][w,],s])){

              if(abs(mydataK[[x-1]][v,s])!=abs(mydataK[[x]][My.K_Recording[[u-1]][w,],s])){
                so=so+1
              }
              else if(abs(mydataK[[x-1]][v,s])==abs(mydataK[[x]][My.K_Recording[[u-1]][w,],s])){
                if(mydataK[[x-1]][v,s]==mydataK[[x]][My.K_Recording[[u-1]][w,],s]){
                  so=so+1
                }
                else if(mydataK[[x-1]][v,s]<mydataK[[x]][My.K_Recording[[u-1]][w,],s]){
                  so=so+1
                }
              }

              if(so==ncol(mydataK[[x]])+1){

                KCol_Prev = rbind(KCol_Prev,mydataK[[x-1]][v,])
                KCol_Next = rbind(KCol_Next,mydataK[[x]][My.K_Recording[[u-1]][w,],])
                Instant_KCombined = rbind(Instant_KCombined,K_Combined[[u-1]][w,])
                K_Combined[[u]] = Instant_KCombined
                #print(paste0("Processing ",LoadingNum))

                #LoadingNum = LoadingNum+1

                K_Recording = c(K_Recording,v)
              }
              s = s+1

            }
          }
        }
        K_Recording = as.matrix(K_Recording)
        My.K_Recording[[u]] = K_Recording
        K_Combined[[u]] = cbind(KCol_Prev,K_Combined[[u]])
        u = u+1
      }
      x=x-1
    }
  }

  K_Matrix <- list()

  for(i in 1:nrow(K_Combined[[length(my.colSize)-1]])){
    K_Matrix[[i]] <- matrix(,nrow=my.colSize[1],ncol=length(Shape))
    count <- 1
    for(j in 1:length(my.colSize)){
      for(k in 1:my.colSize[j]){
        K_Matrix[[i]][k,j] <- K_Combined[[length(my.colSize)-1]][i,count]
        count<-count+1

      }
    }
    K_Matrix[[i]][is.na(K_Matrix[[i]])] <- 0
  }

  return(K_Matrix)

}


DeConcini_Generator <- function(Shape,Range){

  if(which(rownames(installed.packages())=="gtools",arr.ind=TRUE)!=0){
    library(gtools)
  }
  else if(which(rownames(installed.packages())=="gtools",arr.ind=TRUE)==0){
    #install if necessary
    install.packages('gtools')
    #load library
    library(gtools)
  }

  #Generating permutation column Set
  permu_col <- list()
  ascenper_col <- list()
  numlist <- -Range:Range
  numlist <- numlist[numlist!=0]
  for(i in 1:length(Shape)){

    permu_col[[i]] <- permutations(n=length(numlist),r=Shape[i],v=numlist,repeats.allowed=F)
    ascenper_col[[i]] <- matrix(, nrow=0,ncol=Shape[i])

  }

  for(k in 1:length(Shape)){

    for(o in 1:nrow(permu_col[[k]])){
      count <-1
      while(count<=(ncol(permu_col[[k]])-1)&&permu_col[[k]][o,count]<permu_col[[k]][o,count+1]){
        count <- count+1
      }
      if(count==ncol(permu_col[[k]])){
        ascenper_col[[k]] <- rbind(ascenper_col[[k]],permu_col[[k]][o,])
      }
    }
  }

  D_Tableaux <- list()

  if(length(Shape)==1){

    List <- Admissible_Checker(ascenper_col,Range)


    for(d in 1:nrow(List$middle[[1]])){

      D_Tableaux[[d]] <- matrix(,nrow=max(Shape),ncol=length(Shape))

      for(e in 1:length(List$middle[[1]][d,])){

        D_Tableaux[[d]][e,1] <- List$middle[[1]][d,e]

      }
    }

  }

  else{

    List <- Admissible_Checker(ascenper_col,Range)

    D_Record <- DeC_Tableaux(List,Shape)

    for(d in 1:nrow(D_Record[[length(D_Record)]])){

      D_Tableaux[[d]] <- matrix(,nrow=max(Shape),ncol=length(Shape))

      for(e in 1:ncol(D_Record[[length(D_Record)]])){

        for(g in 1:ncol(List$middle[[e]])){

          D_Tableaux[[d]][g,e] <- List$middle[[e]][D_Record[[length(D_Record)]][d,e],g]

        }
      }
    }

  }

  return(D_Tableaux)
}

Admissible_Checker <- function(ascenper_col,Range){

  left_col <-list()
  right_col <-list()
  middle_col <- list()
  abs_ascenper_col <- list()
  for(i in 1:length(ascenper_col)){
    middle_col[[i]] <- matrix(,nrow=0,ncol=ncol(ascenper_col[[i]]))
    left_col[[i]] <- matrix(,nrow=0,ncol=ncol(ascenper_col[[i]]))
    right_col[[i]] <- matrix(,nrow=0,ncol=ncol(ascenper_col[[i]]))
    abs_ascenper_col[[i]] <- abs(ascenper_col[[i]])

    for(j in 1:nrow(ascenper_col[[i]])){

      inst <- ascenper_col[[i]][j,]
      inst_left <- ascenper_col[[i]][j,]
      inst_right <- ascenper_col[[i]][j,]

      a <- c()
      b <- c()
      check <-0
      count <- 0

      for(k in 1:Range){

        if(sum(abs_ascenper_col[[i]][j,]==k)==2){
          a <- c(a,k)
        }
        else if(sum(abs_ascenper_col[[i]][j,]==k)==0){
          b <- c(b,k)
        }
      }
      a <- sort(a,decreasing=TRUE)
      b <- sort(b, decreasing=TRUE)

      if(length(b)>=length(a)&&length(a)!=0){

        for(n in 1:length(a)){

          for(m in 1:length(b)){

            if(a[n]>b[m]){
              count <- count+1
              inst_left[inst_left == a[n]] <- b[m]
              inst_right[inst_right == -a[n]] <- -b[m]
              inst_left <- sort(inst_left,decreasing=FALSE)
              inst_right <- sort(inst_right,decreasing=FALSE)
              b[m] <- max(a)+1
              a[n] <- 0
            }
            if(count==length(a)&&check==0){
              middle_col[[i]] <- rbind(middle_col[[i]],inst)
              left_col[[i]] <- rbind(left_col[[i]],inst_left)
              right_col[[i]] <- rbind(right_col[[i]],inst_right)
              check<-check+1
            }
          }
        }
      }

      else if(length(a)==0){
        middle_col[[i]] <- rbind(middle_col[[i]],ascenper_col[[i]][j,])
        left_col[[i]] <- rbind(left_col[[i]],ascenper_col[[i]][j,])
        right_col[[i]] <- rbind(right_col[[i]],ascenper_col[[i]][j,])
      }
    }
  }

  return(list("left"= left_col,"right" = right_col,"middle" = middle_col))
}

DeC_Tableaux <- function(List,Shape){

  Recording <- list()
  combined_rec <- list()

  for(i in 1:(length(Shape)-1)){
    Recording[[i]] <- matrix(,nrow=0,ncol=2)

    for(j in 1:nrow(List$middle[[i]])){

      for(k in 1:nrow(List$middle[[i+1]])){

        m<-0
        for(r in 1:ncol(List$middle[[i+1]])){

          if(List$right[[i]][j,r]<=List$left[[i+1]][k,r]){

            m<-m+1
            if(m==ncol(List$middle[[i+1]])){
              instant <- matrix(,nrow=1,ncol=2)
              instant[1,1]<-j
              instant[1,2]<-k
              Recording[[i]] <- rbind(Recording[[i]],instant)
            }
          }
        }
      }
    }
  }


  if(length(Recording)==1){

    count<-0

    combined_rec[[length(Recording)]] <- Recording[[1]]

  }

  else{

    for(x in 1:(length(Recording)-1)){

      count<-0
      #컬럼이 두개일 경우에는 제어가 안됨 에러 처리

      if(x==1){

        combined_rec[[x]] <- matrix(,nrow=0,ncol=(ncol(Recording[[x]])+(ncol(Recording[[x+1]]))-1))

        for(y in 1:nrow(Recording[[x]])){

          for(z in 1:nrow(Recording[[x+1]])){

            if(Recording[[x]][y,2]==Recording[[x+1]][z,1]){
              instant <- t(as.matrix(c(Recording[[x]][y,],Recording[[x+1]][z,2])))
              combined_rec[[x]] <- rbind(combined_rec[[x]],instant)
              count<-count+1
            }
          }
        }
      }
      if(x!=1){

        combined_rec[[x]] <- matrix(,nrow=0,ncol=(ncol(combined_rec[[x-1]])+(ncol(Recording[[x+1]]))-1))
        for(y in 1:nrow(combined_rec[[x-1]])){

          for(z in 1:nrow(Recording[[x+1]])){

            if(combined_rec[[x-1]][y,ncol(combined_rec[[x-1]])]==Recording[[x+1]][z,1]){

              instant <- t(as.matrix(c(combined_rec[[x-1]][y,],Recording[[x+1]][z,2])))
              combined_rec[[x]] <- rbind(combined_rec[[x]],instant)
            }
          }
        }
      }
    }

  }

  return(combined_rec)
}

options(warn=-1)
coco <- 1
ads <-0
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
  Co_I1 <- list()
  Co_J1 <- list()
  Ad_I <- list()
  Ad_J <- list()
  new_A <- list()
  new_B <- list()
  new_C <- list()
  new_D <- list()
  new_A1 <- list()
  new_B1 <- list()
  new_C1 <- list()
  new_D1 <- list()

  Co_I[[1]] <- matrix(,nrow=1,ncol=1)
  Co_J[[1]] <- matrix(,nrow=1,ncol=1)
  Co_I1[[1]] <- matrix(,nrow=1,ncol=1)
  Co_J1[[1]] <- matrix(,nrow=1,ncol=1)
  new_A[[1]] <- matrix(,nrow=1,ncol=1)
  new_D[[1]] <- matrix(,nrow=1,ncol=1)
  new_B[[1]] <- matrix(,nrow=1,ncol=1)
  new_C[[1]] <- matrix(,nrow=1,ncol=1)
  new_A1[[1]] <- matrix(,nrow=1,ncol=1)
  new_D1[[1]] <- matrix(,nrow=1,ncol=1)
  new_B1[[1]] <- matrix(,nrow=1,ncol=1)
  new_C1[[1]] <- matrix(,nrow=1,ncol=1)
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

              Co_J[[1]][Co_J[[1]] == Co_J[[1]][1,h]] <- min(Co_I[[1]])-1
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

    #coadmissible column of T_n+1
    components$C[[coordinate[1,2]+1]] <- components$C[[coordinate[1,2]+1]][components$C[[coordinate[1,2]+1]] != tableaux[coordinate[,1],2*coordinate[,2]+1]]
    coadmissible_col <- append(components$B[[coordinate[,2]+1]],components$C[[coordinate[,2]+1]])
    abs_coadmissible_col <- abs(coadmissible_col)
    #delete element in T_n+1
    tableaux[coordinate[,1],2*coordinate[,2]+1] <- 0
    tableaux[coordinate[,1],2*coordinate[,2]+2] <- 0

    #Delete element from component C to make co-admissible
    a <- c()
    b <- c()
    for(k in 1:setorder){

      if(sum(abs_coadmissible_col==k)==2){
        a <- c(a,k)
      }
      else if(sum(abs_coadmissible_col==k)==0){
        b <- c(b,k)
      }
    }
    Co_I1[[1]] <- matrix(,nrow=1,ncol=length(a))
    Co_J1[[1]] <- matrix(,nrow=1,ncol=length(b))
    a<-sort(a,decreasing=FALSE)
    b<-sort(b,decreasing=FALSE)
    Co_I1[[1]][1,] <- a
    Co_J1[[1]][1,] <- b


    new_A1[[1]] <- components$B[[coordinate[,2]+1]]
    new_D1[[1]] <- components$C[[coordinate[,2]+1]]


    if(length(Co_I1[[1]][1,])!=0&&length(Co_J1[[1]][1,])!=0){
      # Reconstruct column of T_n
      for(o in 1:ncol(Co_I1[[1]])){

        if(-Co_I1[[1]][1,o] %in% components$B[[coordinate[,2]+1]]){

          for(h in 1:ncol(Co_J1[[1]])){
            if(Co_I1[[1]][1,o]<Co_J1[[1]][1,h]){

              new_A1[[1]][new_A1[[1]] == -Co_I1[[1]][1,o]] <- -Co_J1[[1]][1,h]
              new_D1[[1]][new_D1[[1]] == Co_I1[[1]][,o]] <- Co_J1[[1]][1,h]
              #Co_J1[[1]] <- Co_J1[[1]][Co_J1[[1]] != Co_J1[[1]][1,h]]
              Co_J1[[1]][Co_J1[[1]] == Co_J1[[1]][1,h]] <-min(Co_I1[[1]])-1
              break
            }
          }
        }

      }
    }

    new_A1[[1]] <- sort(new_A1[[1]],decreasing = FALSE)
    new_D1[[1]] <- sort(new_D1[[1]],decreasing = FALSE)

    components$B[[coordinate[,2]]] <- sort(components$B[[coordinate[,2]+1]],decreasing = FALSE)
    components$C[[coordinate[,2]]] <- sort(components$C[[coordinate[,2]+1]],decreasing = FALSE)

    new_coadmissible_left_col <- append(new_A1[[1]],components$C[[coordinate[,2]+1]])

    new_coadmissible_left_col <- matrix(new_coadmissible_left_col, nrow=1, ncol=length(new_coadmissible_left_col))

    new_coadmissible_right_col <- append(components$B[[coordinate[,2]+1]],new_D1[[1]])

    new_coadmissible_right_col <- matrix(new_coadmissible_right_col, nrow=1, ncol=length(new_coadmissible_right_col))

    #insert new value into the Column

    if(ncol(new_coadmissible_left_col)!=0){
      r<-1
      for(q in 1:ncol(new_coadmissible_left_col)){

        if(tableaux[q,2*(coordinate[,2]+1)-1]!=0){
          tableaux[q,2*(coordinate[,2]+1)-1] <-  new_coadmissible_left_col[1,r]
          r<-r+1
        }
      }
    }

    if(ncol(new_coadmissible_right_col)!=0){
      r <- 1
      for(q in 1:ncol(new_coadmissible_right_col)){

        if(tableaux[q,2*(coordinate[,2]+1)-1]!=0){
          tableaux[q,2*(coordinate[,2]+1)] <-  new_coadmissible_right_col[1,r]
          r<- r+1
        }
      }
    }

    #re-combine T_n besed on admissible column part
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
              #Ad_J[[1]] <- Ad_J[[1]][Ad_J[[1]] != Ad_J[[1]][1,h]]
              Ad_J[[1]][Ad_J[[1]] == Ad_J[[1]][1,h]] <-max(Ad_I[[1]])+1
              break
            }
          }
        }

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


        if(-I[[n]][1,o] %in% B[[n]]){


          for(h in 1:ncol(J[[n]])){

            if(I[[n]][1,o]>J[[n]][1,h]&&J[[n]][1,h]<=setorder){

              B[[n]][B[[n]] == -I[[n]][1,o]] <- -J[[n]][1,h]

              C[[n]][C[[n]] == I[[n]][,o]] <- J[[n]][1,h]

              J[[n]][J[[n]] == J[[n]][1,h]] <-max(I[[n]])+1

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

#Vertical move
vertical_move <- function(tableaux,coordinate){

  tableaux[coordinate[,1],coordinate[,2]] <- tableaux[coordinate[,1]+1,coordinate[,2]]
  tableaux[coordinate[,1]+1,coordinate[,2]] <-0

  return(tableaux)

}

#Make splited form
Splited_Form <- function(tableaux,setorder,coordinate){

  components <- components(tableaux,setorder)

  abs_tableaux <- abs(tableaux)

  split <- matrix(,nrow=nrow(tableaux),ncol=2*ncol(tableaux))


  for(u in 1:ncol(tableaux)){


    components$A[[u]] <- sort(components$A[[u]],decreasing = FALSE)
    components$C[[u]] <- sort(components$C[[u]],decreasing = FALSE)
    components$B[[u]] <- sort(components$B[[u]],decreasing = FALSE)
    components$D[[u]] <- sort(components$D[[u]],decreasing = FALSE)

    combined_left <- append(components$A[[u]],components$C[[u]])
    combined_right <- append(components$B[[u]],components$D[[u]])


    w <- 1
    listing_right <- c()
    listing_left <- c()
    for(y in 1:length(tableaux[,u])){

      if(tableaux[y,u]==0){
        listing_right <- c(listing_right,0)
        listing_left <- c(listing_left,0)
      }
      else if(tableaux[y,u]!=0){
        listing_right <- c(listing_right,combined_right[w])
        listing_left <- c(listing_left,combined_left[w])
        w <- w+1
      }
    }

    split[,2*u-1] <- listing_left
    split[,2*u] <- listing_right

  }
  return(split)
}

#Make unsplited form
Unsplited_Form <- function(split_tableaux,sign,coordinate_col,setorder){

  unsplit_col <- ncol(split_tableaux)/2
  unsplit <- matrix(,nrow=nrow(split_tableaux),ncol=unsplit_col)
  alpha <- c()
  beta <- c()
  #when n in T_n+1
  if(sign == 1){

    for(u in 1:unsplit_col){
      if(coordinate_col+1==u){

        B <- subset(split_tableaux[,2*(coordinate_col+1)],split_tableaux[,2*(coordinate_col+1)]<0)
        C <- subset(split_tableaux[,2*(coordinate_col+1)-1],split_tableaux[,2*(coordinate_col+1)-1]>0)
        combined_col <- append(B,C)

        abs_combined_col <- abs(combined_col)
        for(l in 1:setorder){
          if(sum(abs_combined_col==l)==2){
            alpha <- c(alpha,l)
          }
          if(sum(abs_combined_col==l)==0){
            beta <- c(beta,l)
          }
        }

        if(length(alpha)!=0&&length(beta)!=0){
          for(i in 1:length(alpha)){

            for(j in 1:length(beta)){
              if(alpha[i]<beta[j]){

                B[B==-alpha[i]] <- -beta[j]
                C[C==alpha[i]] <- beta[j]

                #beta <- beta[beta != beta[j]]
                beta[beta == beta[j]] <-0

                break
              }
            }
          }
          B <- sort(B,decreasing=FALSE)
          C <- sort(C, decreasing=FALSE)
          combined_col <- c()
          combined_col <- append(B,C)
        }



        if(length(combined_col)!=0){
          r <- 1
          for(q in 1:nrow(split_tableaux)){
            if(split_tableaux[q,2*(coordinate_col+1)]!=0){
              unsplit[q,u] <-  combined_col[r]
              r<- r+1
            }
            else{
              unsplit[q,u] <-  0
            }
          }
        }
        else{
          for(q in 1:nrow(split_tableaux)){
            unsplit[q,u] <- 0
          }
        }

      }
      else{

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
      }
    }
  }
  #when bar_n
  else if(sign ==-1){
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
    }
  }
  else{
    while(TRUE){
      print("error")
    }
  }


  return(unsplit)
}

#########
#Start value
reducesizecount <- 1


D2K_Bijection <- function(tableaux,range,setorder){
  absolute_king <- matrix(,nrow=nrow(tableaux),ncol=ncol(tableaux))

  tableaux[is.na(tableaux)] <- 0
  unsplit <- tableaux
  uni_unsplit <-abs(unsplit)

  while(setorder>1){

    if((setorder %in% abs(unsplit))==FALSE){
      setorder <- setorder-1
    }

    else if(reducesizecount ==1){

      reducesizecount <- reducesizecount+1
      uni_unsplit <- abs(unsplit)

      coordinate <- which(unsplit==-setorder,arr.ind=TRUE)
      coordinate <- subset(coordinate, coordinate[,1]==1)
      length_setorder <- nrow(coordinate)

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

      king_part <- matrix(,nrow=nrow(unsplit),ncol=ncol(unsplit))
      king_part <- unsplit
      king_part[uni_unsplit<setorder] <- 0

      if(nrow(coordinate)!=0){
        for(c in 1:nrow(coordinate)){
          king_part[coordinate[c,1],coordinate[c,2]] <-0
        }
      }

      king_coordinate <- matrix(,nrow=1,ncol=1)
      king_coordinate <- which(king_part != 0,arr.ind=TRUE)
      if(nrow(king_coordinate)!=0){
        for(d in 1:nrow(king_coordinate)){
          absolute_king[king_coordinate[d,1],king_coordinate[d,2]] <- unsplit[king_coordinate[d,1],king_coordinate[d,2]]
        }
      }

      change_cord <- matrix(,nrow=1,ncol=1)
      change_cord <- coordinate

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

    else{

      within <- 1
      cord_counting <- nrow(coordinate)

      while(cord_counting>0){

        change_cord <- matrix(,nrow=1,ncol=2)

        change_cord[1,] <- coordinate[cord_counting,]

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

          change_cord[1,] <- coordinate[cord_counting,]

          if(within==1){
            split <- Splited_Form(deconcini_part,setorder,coordinate)
            unsplit <- deconcini_part
            within <- within+1

          }

          else if(within!=1){

            uni_unsplit <- abs(unsplit)

            king_part <- matrix(,nrow=nrow(unsplit),ncol=ncol(unsplit))
            king_part <- unsplit
            king_part[uni_unsplit<setorder] <- 0

            for(c in 1:nrow(coordinate)){
              king_part[coordinate[c,1],coordinate[c,2]] <-0
            }

            king_coordinate <- matrix(,nrow=1,ncol=1)
            king_coordinate <- which(king_part != 0,arr.ind=TRUE)

            if(nrow(king_coordinate)!=0){
              for(d in 1:nrow(king_coordinate)){
                absolute_king[king_coordinate[d,1],king_coordinate[d,2]] <- unsplit[king_coordinate[d,1],king_coordinate[d,2]]
                unsplit[king_coordinate[d,1],king_coordinate[d,2]] <-0
              }
            }


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
            horizon_oversize <- FALSE
            vertical_oversize <- FALSE
            oversize <- FALSE
            while(horizon_oversize==FALSE&&vertical_oversize==FALSE&&oversize==FALSE){

              ads <- ads+1

              if(length_cols != 1){

                if(change_cord[1,1]==length_rows[1,change_cord[1,2]]&&change_cord[1,2]==length_cols){
                  oversize <- TRUE

                }

                else if(change_cord[1,1]==length_rows[,change_cord[1,2]]&&change_cord[1,1]<=length_rows[,change_cord[1,2]+1]){

                  sign <- NA
                  if(split[change_cord[1,1],2*(change_cord[1,2]+1)-1]>0){
                    sign <-1
                  }
                  else{
                    sign <- -1
                  }

                  components<-components(unsplit,setorder)

                  split <- horizontal_move(split,setorder,components,change_cord)

                  unsplit <- Unsplited_Form(split,sign,change_cord[1,2],setorder)

                  change_cord[1,2] <- change_cord[1,2]+1
                  sign <- NA

                  if((change_cord[1,1]>length_rows[1,change_cord[1,2]])||((change_cord[1,1]==length_rows[1,change_cord[1,2]])&&(change_cord[1,2]==length_cols))||((change_cord[1,1]==length_rows[1,change_cord[1,2]])&&(change_cord[1,1]>length_rows[1,change_cord[1,2]+1]))){
                    horizon_oversize <- TRUE
                  }


                }

                else if(change_cord[1,2]==length_cols||change_cord[1,1]>length_rows[1,change_cord[1,2]+1]){


                  unsplit[change_cord[1,1],change_cord[1,2]] <- unsplit[change_cord[1,1]+1,change_cord[1,2]]
                  unsplit[change_cord[1,1]+1,change_cord[1,2]] <- 0

                  split[change_cord[1,1],2*change_cord[1,2]] <- split[change_cord[1,1]+1,2*change_cord[1,2]]
                  split[change_cord[1,1]+1,2*change_cord[1,2]] <- 0
                  split[change_cord[1,1],2*change_cord[1,2]-1] <- split[change_cord[1,1]+1,2*change_cord[1,2]-1]
                  split[change_cord[1,1]+1,2*change_cord[1,2]-1] <- 0

                  change_cord[1,1] <- change_cord[1,1]+1
                  if((change_cord[1,2]==length_cols)&&(change_cord[1,1]==length_rows[1,change_cord[1,2]])){
                    horizon_oversize <- TRUE
                  }
                  else if(change_cord[1,1]==length_rows[1,change_cord[1,2]]&&change_cord[1,1]>length_rows[1,change_cord[1,2]+1]){
                    horizon_oversize <- TRUE
                  }

                }

                else if((change_cord[1,2]!=length_cols)&&(change_cord[1,1]<length_rows[,change_cord[1,2]])&&(split[change_cord[1,1]+1,2*change_cord[1,2]]<=split[change_cord[1,1],2*change_cord[1,2]+1])){

                  unsplit<- vertical_move(unsplit,change_cord)
                  split[change_cord[1,1],2*change_cord[1,2]] <- split[change_cord[1,1]+1,2*(change_cord[1,2])]
                  split[change_cord[1,1]+1,2*(change_cord[1,2])] <- 0
                  split[change_cord[1,1],2*(change_cord[1,2])-1] <- split[change_cord[1,1]+1,2*(change_cord[1,2])-1]
                  split[change_cord[1,1]+1,2*(change_cord[1,2])-1] <- 0

                  #unsplit <- Unsplited_Form(split)
                  change_cord[1,1] <- change_cord[1,1]+1

                  if((change_cord[1,1]>length_rows[1,change_cord[1,2]])||((change_cord[1,1]==length_rows[1,change_cord[1,2]])&&(change_cord[1,2]==length_cols))||(change_cord[1,1]==length_rows[1,change_cord[1,2]])&&(change_cord[1,1]>length_rows[1,change_cord[1,2]+1])){
                    vertical_oversize <- TRUE
                  }

                }

                else{
                  sign <- NA
                  if(split[change_cord[1,1],2*(change_cord[1,2]+1)-1]>0){
                    sign <-1
                  }
                  else{
                    sign <- -1
                  }

                  components<-components(unsplit,setorder)
                  split <- horizontal_move(split,setorder,components,change_cord)

                  unsplit <- Unsplited_Form(split,sign,change_cord[1,2],setorder)

                  split <- Splited_Form(unsplit,setorder,coordinate)

                  change_cord[1,2] <- change_cord[1,2]+1
                  sign <- NA

                  if((change_cord[1,2]==length_cols)&&(change_cord[1,1]==length_rows[1,change_cord[1,2]])||change_cord[1,1]==length_rows[1,change_cord[1,2]]&&change_cord[1,1]>length_rows[1,change_cord[1,2]+1]){
                    horizon_oversize <- TRUE
                  }
                }
              }

              #column이 하나일때 vertical move 해야됨
              else if(length_cols==1){

                unsplit<- vertical_move(unsplit,change_cord)
                #unsplit <- Unsplited_Form(split)
                change_cord[1,1] <- change_cord[1,1]+1
                if((change_cord[1,1]>=length_rows[,change_cord[1,2]])){
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

  print.table(local({tableaux[tableaux==0] <- NA; tableaux}))
  print("De Concini tableau")
  absolute_king[is.na(absolute_king)]<-0
  print.table(local({absolute_king[absolute_king==0] <- NA; absolute_king}),row.names=FALSE, col.names=FALSE)
  print("King tableau")
  return(absolute_king)

}

D2K_Gen <- function(tableaux,Range){

  newShape <- list()


  for(i in 1:length(tableaux)){
    print("========================================================================")
    print(i)
    newShape[[i]]<-D2K_Bijection(tableaux[[i]],Range,Range)
    print("========================================================================")
    #Sys.sleep(5)
  }
  return(newShape)
}
