LoadingNum = 0
mydata <- list()
mydataLeft <-list()
mydataRight <-list()
my.Lamda <- function(){
  my.numCol <- as.numeric(readline(prompt = "Enter the number of columns : "))
  my.colSize <- matrix(,nrow=1,ncol=my.numCol)
  my.numSize <- matrix(,nrow=1,ncol=1)
  for(i in 1:my.numCol){
    my.instantColSize <-as.numeric(as.numeric(readline(paste0("what is the height of ",i,"th column ?:"))))
    my.colSize[,i] = my.instantColSize
  }
  return(my.colSize)
}

my.colSize <- my.Lamda()


my.numSize=0;
while(my.numSize==0){
  my.numSize <- readline(prompt = "Enter the range of number: ")
  my.numSize <- as.numeric(my.numSize)
  
  if(my.numSize==0){
    print("You have to put the value more than 0")
  }
}

my.numList <- -my.numSize:my.numSize
my.numList <- my.numList[my.numList != 0]


for(k in 1:length(my.colSize)){
  
  ###지워진 자리
  
  if(my.colSize[1,k]==1){
    KN_Admissible_1lengthCol = matrix(,nrow=0,ncol=my.colSize[1,k])
    KN_Admissible_1lengthRCol = matrix(, nrow=0,ncol=my.colSize[1,k])
    KN_Admissible_1lengthLCol = matrix(, nrow=0,ncol=my.colSize[1,k])
    for(z in 1:length(my.numList)){
      KN_Admissible_1lengthCol = rbind(KN_Admissible_1lengthCol,my.numList[z])
      KN_Admissible_1lengthRCol = rbind(KN_Admissible_1lengthRCol,my.numList[z])
      KN_Admissible_1lengthLCol = rbind(KN_Admissible_1lengthLCol,my.numList[z])
    }
    mydata[[k]] <- KN_Admissible_1lengthCol
    mydataLeft[[k]] <- KN_Admissible_1lengthRCol
    mydataRight[[k]] <- KN_Admissible_1lengthLCol
  }  
  
  if (my.colSize[1,k]>1) {
    permutation_result <- permutations(n=length(my.numList),r=my.colSize[1,k],v=my.numList,repeats.allowed=F)
    
    
    AscendingPermutation_result = matrix(, nrow=0,ncol=my.colSize[1,k])
    
    for(i in 1:nrow(permutation_result)){
      
      j=1;
      
      
      while(j<=my.colSize[1,k]-1&&permutation_result[i,j]<permutation_result[i,j+1]){
        
        if(j == my.colSize[1,k]-1){
          AscendingPermutation_result = rbind(AscendingPermutation_result,permutation_result[i,])
        }
        j = j+1;
      }
    }
    
    AbsoluteAscendingPermutation_result = abs(AscendingPermutation_result)
    AbsoluteAscendingPermutation_result_For_LeftCol = AscendingPermutation_result
    AbsoluteAscendingPermutation_result_For_RightCol = AscendingPermutation_result
    
    KN_Admissible_Col = matrix(,nrow=0,ncol=my.colSize[1,k])
    KN_Admissible_RCol = matrix(, nrow=0,ncol=my.colSize[1,k])
    KN_Admissible_LCol = matrix(, nrow=0,ncol=my.colSize[1,k])
    
    for(i in 1:nrow(AbsoluteAscendingPermutation_result)){
      
      UniqueElement =t(unique(AbsoluteAscendingPermutation_result[i,]))
      a = matrix(,nrow=1,ncol=0)
      b = matrix(,nrow=1,ncol=0)
      ##koko
      ##if(length(UniqueElement)<=my.numSize){
        
        j=1;
        while (j<=my.numSize) {
          
          if(is.element(j,UniqueElement)){
            if(sum(AbsoluteAscendingPermutation_result[i,]==j)==2){
              a = cbind(a,j)
            }
          }
          
          if(sum(AbsoluteAscendingPermutation_result[i,]==j)==0){
            b = cbind(b,j)
          }
        
          j = j+1
        }
        

        if(length(a) <= length(b) && length(a)!=0){
          
          
          if(max(a)<max(b)){
            
            KN_Admissible_Col = rbind(KN_Admissible_Col,AscendingPermutation_result[i,])
            
            for(l in 1:length(a)){
              
              for(m in 1:my.colSize[1,k]){
                
                if(AbsoluteAscendingPermutation_result[i,m]==a[l]){
                  
                  l1= 1
                  while(a[l]>b[l1]){
                    
                    l1 = l1+1
                  }
                  
                  if(-a[l] == AbsoluteAscendingPermutation_result_For_LeftCol[i,m]){
                    AbsoluteAscendingPermutation_result_For_LeftCol[i,m] = -b[l1]
                    KN_Admissible_LCol = rbind(KN_Admissible_LCol,AbsoluteAscendingPermutation_result_For_LeftCol[i,])
                    
                  }
                  
                  if(a[l] == AbsoluteAscendingPermutation_result_For_RightCol[i,m]){
                    AbsoluteAscendingPermutation_result_For_RightCol[i,m] = b[l1]
                    KN_Admissible_RCol = rbind(KN_Admissible_RCol,AbsoluteAscendingPermutation_result_For_RightCol[i,])
                  }
                }
              }
            }
          }
        }
        
        if(length(a)==0){
          KN_Admissible_Col = rbind(KN_Admissible_Col,AscendingPermutation_result[i,])
          KN_Admissible_RCol = rbind(KN_Admissible_RCol,AscendingPermutation_result[i,])
          KN_Admissible_LCol = rbind(KN_Admissible_LCol,AscendingPermutation_result[i,])
          
        }
    }
        
        mydata[[k]] <- KN_Admissible_Col
        mydataLeft[[k]] <- KN_Admissible_LCol
        mydataRight[[k]] <-KN_Admissible_RCol
        
  }
}

## Sorted by low to high(sequential)
for(o in 1:length(mydataRight)){
  
  if(ncol(mydataRight[[o]])!=1){
    
    for(p in 1:nrow(mydataRight[[o]])){
      
      for(q in 2:ncol(mydataRight[[o]])-1){
        
        if(mydataRight[[o]][p,q]>mydataRight[[o]][p,q+1]){
          
          temp = mydataRight[[o]][p,q]
          mydataRight[[o]][p,q]=mydataRight[[o]][p,q+1]
          mydataRight[[o]][p,q+1] = temp
        }
      }
    }
  }
}

for(o in 1:length(mydataLeft)){
  
  if(ncol(mydataLeft[[o]])!=1){
    for(p in 1:nrow(mydataLeft[[o]])){
      
      for(q in 2:ncol(mydataLeft[[o]])-1){
        
        if(mydataLeft[[o]][p,q]>mydataLeft[[o]][p,q+1]){
          
          temp = mydataLeft[[o]][p,q]
          mydataLeft[[o]][p,q]=mydataLeft[[o]][p,q+1]
          mydataLeft[[o]][p,q+1] = temp
        }
      }
    }
  }
}


##손좀 봐야될 부분



##Recursive 만들고 싶은 부분
mydata_Symplectic <- list()


my.colNum = length(my.colSize)

x = my.colNum


My.KN_Recording <- list()
KN_Admissible_Combined <-list()
if(x == 1){
  
  mydata_Symplectic[[x]] = mydata[[x]]
  
}

if(x > 1){
  u = 1
  while(x>1){
    KN_Admissible_SymPrev = matrix(,nrow=0,ncol(mydata[[x-1]]))
    KN_Admissible_SymNext = matrix(,nrow=0,ncol(mydata[[x]]))
    
    if(x==my.colNum){
      KN_Recording = matrix(,nrow=0,ncol=0)
      for(r in 1:nrow(mydata[[x-1]])){
        
        
        for(v in 1:nrow(mydata[[x]])){
          
          s = 1
          while(s<=ncol(mydata[[x]])&&mydataRight[[x-1]][r,s]<=mydataLeft[[x]][v,s]){
            
            if(s==ncol(mydata[[x]])){
              
              KN_Admissible_SymPrev = rbind(KN_Admissible_SymPrev,mydata[[x-1]][r,])
              KN_Admissible_SymNext = rbind(KN_Admissible_SymNext,mydata[[x]][v,])
              KN_Recording = c(KN_Recording, r)
            }
            s = s+1
          }
        }
      }
      KN_Recording = as.matrix(KN_Recording)
      My.KN_Recording[[u]] = KN_Recording
      KN_Admissible_Combined[[u]] = cbind(KN_Admissible_SymPrev,KN_Admissible_SymNext)
      u = u+1
    }
    
    ##여기
    if(x!=my.colNum){
      Instant_Combined = matrix(,nrow=0,ncol=ncol(KN_Admissible_Combined[[u-1]]))
      KN_Recording = matrix(,nrow=0,ncol=0)
      for(w in 1:nrow(My.KN_Recording[[u-1]])){
        
        for( v in 1:nrow(mydata[[x-1]])){
          
          s = 1
          while(s<=ncol(mydata[[x]])&&mydataRight[[x-1]][v,s]<=mydataLeft[[x]][My.KN_Recording[[u-1]][w,],s]){
            
            if(s==ncol(mydata[[x]])){
              KN_Admissible_SymPrev = rbind(KN_Admissible_SymPrev,mydata[[x-1]][v,])
              KN_Admissible_SymNext = rbind(KN_Admissible_SymNext,mydata[[x]][My.KN_Recording[[u-1]][w,],])
              ##여기를 어떻게 이해해야 할지...
              Instant_Combined = rbind(Instant_Combined,KN_Admissible_Combined[[u-1]][w,])
              KN_Admissible_Combined[[u]]= Instant_Combined
              
              print(paste0("Processing ",LoadingNum))
              
              LoadingNum = LoadingNum+1
              
              KN_Recording = c(KN_Recording,v)
            }
            s = s+1
          }
        }
      }
      ##KN_Admissible_Combined[[u]] = Instant_Combined
      KN_Recording = as.matrix(KN_Recording)
      My.KN_Recording[[u]] = KN_Recording
      KN_Admissible_Combined[[u]] = cbind(KN_Admissible_SymPrev,KN_Admissible_Combined[[u]])
      u = u+1
    }
    x=x-1
  }
}

for(o in 1:nrow(KN_Admissible_Combined[[u-1]])){
  
  for(p in 1:ncol(KN_Admissible_Combined[[u-1]])){
    
    a <-KN_Admissible_Combined[[u-1]][o,p]
    
    if(a>0){
      
      KN_Admissible_Combined[[u-1]][o,p] <- (KN_Admissible_Combined[[u-1]][o,p]-my.numSize-1)
      
    }
    
    if(a<0){
      
      KN_Admissible_Combined[[u-1]][o,p] <- (KN_Admissible_Combined[[u-1]][o,p]+my.numSize+1)
      
    }
  }
}


count = 0
ZeroCol = matrix(,nrow=nrow(KN_Admissible_Combined[[u-1]]),ncol=0)
for(o in 1:nrow(KN_Admissible_Combined[[u-1]])){
  
  a = unique(abs(my.numList))
  
  r=1
  while(r<=length(a)&&length(KN_Admissible_Combined[[u-1]][o,][KN_Admissible_Combined[[u-1]][o,]==a[r]])==length(KN_Admissible_Combined[[u-1]][o,][KN_Admissible_Combined[[u-1]][o,]==-a[r]])){
    
    if(r==length(a)){
      
      print(KN_Admissible_Combined[[u-1]][o,])
      count = count+1
    }
    
    r=r+1
  }
  
}
