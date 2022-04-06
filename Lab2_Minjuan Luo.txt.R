n_stay <- 0    
n_switch <- 0
n_choice <- 0
result <- rep("result: ")                                                       #initialize a string using rep() method

for ( i in 1:100) {
  door <- c(1,2,3)                                                              #set 3 options in order to let contestant to choose
  choose <- c(0,1)                                                              #set two choice represent stay and switch
  stay <- sample(choose, 1)                                                     #select a number from choose  
  cardoor <- sample(door,1)                                                     #set the correct door among the 3 options randomly
  choice <- sample(door,1)                                                      #contestant choose one of the three doors randomly
  goatdoors <- setdiff(door, cardoor)                                           #set the other 2 doors into goat door(note that the goat door must different from the car door)
  reveal_options <- setdiff(goatdoors, choice)                                  #reveal the another goat door if one of the goat door was chose by contestant
  if (choice == cardoor) { 
    reveal <- sample(reveal_options,1)  }  
  else {
    reveal <- reveal_options                                                    #reveal the chosen door
  }
  remaining_doors <-setdiff(door, reveal)                                       #reveal the another goat door
  newchoice <- setdiff(remaining_doors, choice)                                 #choose another option in the remaining 2 items
  
  if (choice == cardoor) {                                                      #if the user chose the correct door in the first time, stay++
      if(stay == 1){
        n_choice <- n_choice + 1                                                #if the user randomly pick stay, then n_choice++
      }
    n_stay <- n_stay + 1
    result <- append(result, "stay")                                            #append the "stay" string into the result if the user is in the stay situation
  }
  
  if (newchoice == cardoor) {                                                   #if the user chose the correct door in the second time, switch++
    if(stay == 0){
      n_choice <- n_choice + 1                                                  #if the user randomly pick switch, then n_choice++
    }
    n_switch <- n_switch + 1
    result <- append(result, "switch")                                          #append the "switch" string into the result if the user is in the switch situation
  }
}
print(n_stay/100)
print(n_switch/100)
print(n_choice/100)                                                             #represent the first player
print(1-n_choice/100)                                                           #represent the second player
print(result)                                                                   
