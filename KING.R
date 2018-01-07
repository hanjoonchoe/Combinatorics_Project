LoadingNum = 0

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

mydataK<-list()
for(k in 1:length(my.colSize)){
  
  if(my.colSize[1,k]==1){
    
    KCol = matrix(, nrow=0, ncol=my.colSize[1,k])
    
    for(z in 1:length(my.numList)){
      
      KCol = rbind(KCol,my.numList[z])
      
    }
    
    mydataK[[k]] <-KCol
    
  }
  
  if (my.colSize[1,k]>1){
    
    permutation_result <- permutations(n=length(my.numList), r=my.colSize[1,k], v=my.numList, repeats.allowed = F)
    
    if(k == 1){
      
      
      SelectedPermutation_result = matrix(, nrow=0,ncol=my.colSize[1,k])
      
      for(i in 1:nrow(permutation_result)){
        
        j=1
        while(j<= my.colSize[1,k]-1&&abs(permutation_result[i,j])<=abs(permutation_result[i,j+1])){
          
          if(j==my.colSize[1,k]-1){
            
            co = j+1
            kj=2
            for(ki in 2:co){
              
              if(abs(permutation_result[i,ki])>=ki){
                
                if(abs(permutation_result[i,ki-1])!=abs(permutation_result[i,ki])){
                  
                  kj = kj+1
                  
                }
                
                if(abs(permutation_result[i,ki-1])==abs(permutation_result[i,ki])){
                  
                  if(permutation_result[i,ki-1]>permutation_result[i,ki]){
                    
                    kj = kj+1
                  }
                }
              }
              
              if(kj  == my.colSize[1,k]+1){
                
                SelectedPermutation_result= rbind(SelectedPermutation_result,permutation_result[i,])
                
              }
            }
          }
          j=j+1
        }
      }
    }
    
    
    
    if(k>1){
      
      SelectedPermutation_result = matrix(, nrow=0,ncol=my.colSize[1,k])
      
      for(i in 1:nrow(permutation_result)){
        
        j=1
        while(j<= my.colSize[1,k]-1&&abs(permutation_result[i,j])<=abs(permutation_result[i,j+1])){
          
          if(j==my.colSize[1,k]-1){
            print(permutation_result[i,])
            co = j+1
            kj=2
            for(ki in 2:co){
              
              if(abs(permutation_result[i,ki])>=ki){
                
                if(abs(permutation_result[i,ki-1])!=abs(permutation_result[i,ki])){
                  
                  kj = kj+1
                  
                }
                
                if(abs(permutation_result[i,ki-1])==abs(permutation_result[i,ki])){
                  
                  if(permutation_result[i,ki-1]>permutation_result[i,ki]){
                    
                    kj = kj+1
                  }
                }
              }
              
              if(kj  == my.colSize[1,k]+1){
                
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
              else if(mydataK[[x-1]][r,s]>mydataK[[x]][v,s]){
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
              else if(mydataK[[x-1]][v,s]>mydataK[[x]][My.K_Recording[[u-1]][w,],s]){
                so=so+1
              }
            }
            
            if(so==ncol(mydataK[[x]])+1){
              
              KCol_Prev = rbind(KCol_Prev,mydataK[[x-1]][v,])
              KCol_Next = rbind(KCol_Next,mydataK[[x]][My.K_Recording[[u-1]][w,],])
              Instant_KCombined = rbind(Instant_KCombined,K_Combined[[u-1]][w,])
              K_Combined[[u]] = Instant_KCombined
              print(paste0("Processing ",LoadingNum))
              
              LoadingNum = LoadingNum+1
              
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
