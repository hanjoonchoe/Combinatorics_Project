count <-1
for(i in 1:512){
  
  for(j in 1:512){
    
    if(i==j){
      
    }
    
    else{
      if(nrow(which((newShape[[i]]==newShape[[j]])!=TRUE,arr.ind=TRUE))==0){

        print(i)
        print(newShape[[i]])
        print(D_Matrix[[i]])
        print(j)
        print(newShape[[j]])
        print(D_Matrix[[j]])
        count<-count+1
        #newShape[[i]] <- newShape[[i]][newShape[[i]] != K_Matrix[[j]]]
        break
      }
    }
  }
}
print(count)