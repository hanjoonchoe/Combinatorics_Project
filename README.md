# Tutorial

### 1. Run new_Package2.R

### 2. Shape <- c(3,3,1) 
     * * *
     * *
     * *
     
    This determine the shape of tableaux

### 3. Range <- 3 
    This determine the elements filling tableaux
    You need to choose a proper range with respect to the shape carefully. otherwise, it will occur error!!

### 4. D_tab <- DeConcini_Generator(Shape,Range) 
    Generating DeConcini Tableaux given by Shape and Range

### 5. K_tab <- King_Generator(Shape,Range) 
    Generating King Tableaux given by Shape and Range

### 6. D2K_tab <- D2K_Gen(D_tab,Range)
    Bijection from DeConcini to King tableaux

 
### 7. D2K_tab <- D2K_Bijection(D_tab[[1]],Range) 
    When converting one tableau, you can use this function
    where D_tab[[1]] indicates single Deconcini tableau
