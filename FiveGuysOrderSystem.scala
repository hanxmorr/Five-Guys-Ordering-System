// CSC 110 LAB – Semester Project
// Date: November 20th 2020
// Restaurant: Five Guys
// Website: https://www.fiveguys.com (https://www.fiveguys.com/menu)
// Team Members: Andrew Matos, Zachary Schneider, Hanna Morreale, Benjamin Boyle, Jack Handy

// IMPORTING LIBRARIES
import scala.io.StdIn._ // Used this library for readLine and other things
import scala.collection.immutable.List // Used this library for .distinct
// IMPORTANT VARIABLES
var typeList = List[String]()
var itemsList = List[String]()
var sizeList = List[String]()
//LIST OF FOOD CATEGORIES
var BurgersList = List[String]("Hamburger","Cheeseburger","Bacon Burger","Bacon Cheeseburger","Little Hamburger","Little Cheeseburger","Little Baconburger","Little Bacon Cheeseburger")
var HotDogsList = List[String]("Hot Dog","Bacon Dog","Cheese Dog","Bacon Cheese Dog")
var SandwichesList = List[String]("Grilled Cheese Sandwich","Veggie Sandwich","BLT Sandwich","Veggie Sandwich with Cheese")
var FriesList = List[String]("Fries","Cajun Fries")
var DrinksList = List[String]("Bottle Water","Honest Tea Bottle","Simply Lemonade Bottle","Coke Bottle","Diet Coke Bottle","Sprite Bottle")
// LIST OF FOOD THAT HAS SIZES
var foodWithSize = List[String]("Fries","Cajun Fries")


//FUNCTIONS
def calculatePrice(food:String, typeFood:String, sizeFood:String):Double = food match { // Calculates price by taking the food, food type and food size (if have any)
  case "Burgers" => typeFood match { // If this food is in this category, find the specific food
    case "Hamburger" => 7.19 // If this food matches with the food input, then return the cost
    case "Cheeseburger" => 7.89
    case "Bacon Burger" => 8.19
    case "Bacon Cheeseburger" => 8.99
    case "Little Hamburger" => 5.49
    case "Little Cheeseburger" => 6.19
    case "Little Baconburger" => 6.49
    case _ => 7.09
  }
  case "Hot Dogs" => typeFood match {
    case "Hot Dog" => 4.69
    case "Bacon Dog" => 5.69
    case "Cheese Dog" => 5.59
    case _ => 6.59
  }
  case "Sandwiches" => typeFood match {
    case "Grilled Cheese Sandwich" => 4.69
    case "Veggie Sandwich" => 4.09
    case "BLT Sandwich" => 5.99
    case _ => 4.79
  }
  case "Fries" => typeFood match {
    case "Fries" => sizeFood match {
      case "Little" => 3.29
      case "Regular" => 4.29
      case _ => 5.59
    }
    case _ => sizeFood match {
      case "Little" => 3.29
      case "Regular" => 4.29
      case _ => 5.59
    }
  }
  case "Drinks" => typeFood match {
    case "Bottle Water" => 2.29
    case "Honest Tea Bottle" => 2.99
    case "Simply Lemonade Bottle" => 2.99
    case "Coke Bottle" => 2.99
    case "Diet Coke Bottle" => 2.99
    case _ => 2.99
  }
  case _ => typeFood match { // Shakes
    case _ => 4.49
  }
}
def FoodCategory(item: String):String = { // Takes the food itself and finds out what category its from to calculate the price of it.
  if (BurgersList.contains(item) == true) { // If the food is in this category, then return this category
    return "Burgers"
  }else if (HotDogsList.contains(item) == true) {
    return "Hot Dogs"
  }else if (SandwichesList.contains(item) == true) {
    return "Sandwiches"
  }else if (FriesList.contains(item) == true) {
    return "Fries"
  }else if (DrinksList.contains(item) == true) {
    return "Drinks"
  }else return "Shakes"
}
def PlaceOrder(prevCost: Double, iList: List[String], tList:List[String], sList: List[String]):Unit = { // After the fisrt placement of order, this recursive function takes in effect to see any additional orders.
  println("Would you like to order anything else? (Y/N)") // Look below at beginning of execution for descriptions for this function
  val a2 = readLine
  if (a2 == "Y") {
    println("What would you like to order?")
    val a3 = readLine
    val res = foodWithSize.contains(a3)
    if (res == true){
      println("What size? (Little, Regular, Large)")
      val a4 = readLine
      var list1 = iList
      var Category = FoodCategory(a3)
      var list2 = tList
      var list3 = sList
      list1 = list1 :+ a4 + " " + a3
      list2 = list2 :+ Category
      list3 = list3 :+ a4
      var cost = prevCost + calculatePrice(a3,Category,a4) //Adds previous cost with new cost
    //  println(cost,list1,list2,list3)
      PlaceOrder(cost,list1,list2,list3) // Recursive function
    }else{
      var list1 = iList
      var Category = FoodCategory(a3)
      var list2 = tList
      var list3 = sList
      list1 = list1 :+ a3
      list2 = list2 :+ Category
      list3 = list3 :+ "None"
      var cost = prevCost + calculatePrice(a3,Category,"None")
      //println(cost,list1,list2,list3)
      PlaceOrder(cost,list1,list2,list3)
    }
  }else{
  //  println(prevCost,iList,tList,sList)
    var receiptList = List[String]() // I created a new list to filter out any duplicate values from the for loop below this line
    for (item <- iList) {
      receiptList = receiptList :+ (iList.count(_==item) + " " + item) // Merges the number of the times the food was mentioned + the food and puts into one list (Ex. List(Hamburger,Hamburger,Smoothie) = 2 Hamburgers, 1 Smoothie)
    }
  //  println(receiptList.distinct)
    println("Here is your receipt:")
    for (item <- receiptList.distinct) { // .distinct makes it remove any duplicate values within the list
      println("  " + item) // Makes an indent to make it pretty.
    }
    println("-------------------------")
    var FormattedCost = "%.2f".format(prevCost).toDouble // Makes the double value end at the tenths place. (Ex. 72.38000000004 = 72.38)
    println("Your total is $" + FormattedCost + ".") // Cost displayed on this line
    println("Delivery time will be within 45 minutes.") // I searched up delivery time for five guys, and it stated 45 minutes at this date (Source: https://www.doordash.com/business/five-guys-491/)
    println("Thank you for choosing Five Guys!") // End of order.
  }
}

// START OF PROGRAM
println("Welcome to Five Guys! Would you like to see our menu? (Y/N)") // Yes or No
val a1 = readLine // Y or N
if (a1 == "Y") {
  // Print out full menu
  println("Here is our menu:\nBurgers:\n   Hamburger - $7.19\n   Cheeseburger - $7.89\n   Bacon burger - $8.19\n   Bacon Cheeseburger - $8.99\n   Little Hamburger - $5.49\n   Little Cheeseburger - $6.19\n   Little Baconburger - $6.49\n   Little Bacon Cheeseburger - $7.09\n \nHot Dogs:\n   Hot Dog - $4.69\n   Bacon Dog - $5.69\n   Cheese Dog - $5.59\n   Bacon Cheese Dog - $6.59\n \nSandwiches:\n   Grilled Cheese Sandwich - $4.69\n   Veggie Sandwich - $4.09\n   BLT Sandwich - $5.99\n   Veggie Sandwich with Cheese – $4.79\n \nFries:\n   Fries - ($3.29, $4.29, $5.59)\n   Cajun Fries - ($3.29, $4.29, $5.59)\n \nDrinks:\n   Bottle Water - $2.29\n   Honest Tea Bottle - $2.99\n   Simply Lemonade Bottle - $2.99\n   Coke Bottle - $2.99\n   Diet Coke Bottle - $2.99\n   Sprite Bottle - $2.99\n \nShakes:\n   Milkshakes - $4.49 (Coffee, Oreo Creme, Salted Caramel, Double Stuf, Vanilla, Strawberry, Whipped Cream, Bacon, Oreo Cookies, Peanut Butter, Banana, Chocolate, Malted Milk) ")
}
println("Would you like to order? (Y/N)")
val a2 = readLine // Y or N
if (a2 == "Y") {
  println("What would you like to order?")
  val a3 = readLine // Food Name (NOT TYPE OR SIZE; ONLY FOOD NAME)
  val res = foodWithSize.contains(a3) // Checks if the food is in a list where food size matters (Ex. Little Fries, Large Cajun Fries, etc...)
  if (res == true){
    println("What size? (Little, Regular, Large)")
    val a4 = readLine // Size of food ^
    var list1 = itemsList // Reason I made a variable into a list is because there was a bug where it didnt save the list with values so this fixes it apparently.
    var Category = FoodCategory(a3) // Checks what category the specific food is in to calculate price
    var list2 = typeList
    var list3 = sizeList
    list1 = list1 :+ a4 + " " + a3 // Adds both the size (if any) and the food together into one string
    list2 = list2 :+ Category // Adds to the food type list
    list3 = list3 :+ a4 // Adds to the food size list
    var cost = calculatePrice(a3,Category,a4) // Calculates price of food from the food itself, type and size
    //println(cost,list1,list2,list3)
    PlaceOrder(cost,list1,list2,list3) // Start of recursive function (function has same code but just a few lines changed)
  }else{
    var list1 = itemsList
    var Category = FoodCategory(a3)
    var list2 = typeList
    var list3 = sizeList
    list1 = list1 :+ a3
    list2 = list2 :+ Category
    list3 = list3 :+ "None"
    var cost = calculatePrice(a3,Category,"None")
    //println(cost,list1,list2,list3)
    PlaceOrder(cost,list1,list2,list3) // Start of recursive function (function has same code but just a few lines changed)
  }
}else{
  println("Order Cancelled.") // If costumer decides not to order, it will print this.
}
