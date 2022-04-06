door <- c(1,2,3)                                         #set 3 options to let contestant to choose
cardoor <- sample(door,1)                                #set the correct door among the 3 options randomly
choice <- sample(door,1)                                 #contestant choose one of the three doors randomly
goatdoors <- setdiff(door, cardoor)                      #set the other 2 doors into goat door(note that the goat door must different from the car door)
reveal_options <- setdiff(goatdoors, choice)             #reveal the another goat door if one of the goat door was chose by contestant
if (choice == cardoor) { 
  reveal <- sample(reveal_options,1)                     #reveal the chosen door
}else {
  reveal <- reveal_options                               #reveal the other goat door
}
remaining_doors <-setdiff(door, reveal)
newchoice <- setdiff(remaining_doors, choice)            #choose another option in the remaining 2 items


if (choice == cardoor) {
  print("Stay: You got a car")
}else {
  print("Stay: You got a goat")
}
if (newchoice == cardoor) {
  print("Switch: You got a car")
}else {
  print("Switch: You got a goat")
}