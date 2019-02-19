# Tutorial

### Run new_Package2.R

Shape <- c(3,3,1) # * * *
                  # * *
                  # * *
  #This determine the shape of tableaux

Range <- 3  # Must be larger than 2

  #This determine the elements filling tableaux

D_tab <- DeConcini_Generator(Shape,Range) 

  #Generating DeConcini Tableaux given by Shape and Range

K_tab <- King_Generator(Shape,Range) 

  #Generating King Tableaux given by Shape and Range

D2K_tab <- D2K_Gen(D_tab,Range)# Bijection from DeConcini to King tableaux

 #When converting one tableau, you can use following function
 
D2K_tab <- D2K_Bijection(D_tab[[1]],Range) 

 #where D_tab[[1]] indicates single Deconcini tableau
