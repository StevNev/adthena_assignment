import java.text.DecimalFormat

object main {
  val ItemPrices = Map(
    "Apples" -> 1.00,
    "Bread" -> 0.80,
    "Milk" -> 1.30,
    "Soup" -> 0.65)

  def main(args: Array[String]) = {

    val shoppingList = getUserInput("PriceBasket ")

    val itemList = shoppingList.split(" ")

    val shoppingCount = countListItems(itemList)

    val (subTotal, newTotal, appleSavings, breadSavings) = getTotals(shoppingCount)

    printReceipt(subTotal, newTotal, appleSavings, breadSavings)
  }

  def getUserInput(prompt: String): String = {
    print(prompt)
    scala.io.StdIn.readLine()
  }

  def countListItems(inputArray: Array[String]): Map[String, Int] = {
    inputArray
      .groupBy(identity) // Group elements by identity (element itself)
      .map { case (key, values) => key -> values.length } // Map to key-value pairs with counts
  }

  def getPrice(item: String): Double = {
    ItemPrices.getOrElse(item, 0.0)
  }

  def getTotals(shoppingMap: Map[String, Int]): (Double, Double, Double, Double) = {
    /**
     * Function to retrieve all totals of the input shopping list.
     * Generates Total Cost with & without savings as well as amount saved per offer.
     *
     * @param shoppingList Map of all items entered into the shopping list alongside a count of the item
     * @return (Total Price,  Total Price with Savings, Savings by Apples offer, Savings by Soup + Bread offer)
     */
    val totalCostWithoutDiscounts = shoppingMap.foldLeft(0.0) {
      case (total, (item, count)) => total + getPrice(item) * count
    }

    val (totalCostWithDiscounts, appleSavings, breadSavings) = shoppingMap.foldLeft((0.0, 0.0, 0.0)) {
      case ((total, appleSavingsAcc, breadSavingsAcc), (item, count)) =>
        val (itemPrice, itemAppleSavings, itemBreadSavings): (Double, Double, Double) = item match {
          case "Apples" =>
            val price = getPrice(item) * 0.90 // 10% discount on Apples
            (price * count, (getPrice(item) - price) * count, 0.0)

          case "Bread" =>
            // Apply discount to bread if 2 soups are present
            if (shoppingMap.getOrElse("Soup", 0) < 2) {
              (getPrice(item) * count, 0.0, 0.0)
            } else {
              val price = (count - 1) * getPrice(item) + (getPrice(item) * 0.5)
              (price * count, 0.0, getPrice(item) * 0.5 * count)
            }
          case _ => (getPrice(item) * count, 0.0, 0.0)
        }
        (total + itemPrice, appleSavingsAcc + itemAppleSavings, breadSavingsAcc + itemBreadSavings)
    }

    (totalCostWithoutDiscounts, totalCostWithDiscounts, appleSavings, breadSavings)
  }

  def printReceipt(subTotal: Double, newTotal: Double, appleSavings: Double, breadSavings: Double): Unit = {
    println(s"Subtotal: £${"%.2f".format(subTotal)}")

    if (appleSavings > 0) {
      println(s"Apples 10% off: £${"%.2f".format(appleSavings)}")
    }

    if (breadSavings > 0) {
      println(s"2 Tins of Soup savings: £${"%.2f".format(breadSavings)}")
    }

    if (appleSavings == 0 && breadSavings == 0) {
      println("(No offers available)")
    }

    println(s"Total price: £${"%.2f".format(newTotal)}")
  }
}
