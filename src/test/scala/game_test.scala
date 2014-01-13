import org.scalatest.FlatSpec

class ShipSetting extends FlatSpec {

  "setShipOnField" should "only place one ship in the right size" in {
    val emptyField = (for (x <- (0 to 9); y <- (0 to 9)) yield (x, y, false)).toList
    val game = new Game("", 0, "", RandomShoot)
    val Field5 = game setShipOnField(emptyField, 0, 0, 0, 5)
    assert(Field5.foldLeft(0)((akk,f) => akk+(if(f._3) 1 else 0)) == 5)
    //printFieldList(Field5)
    val Field4 = game setShipOnField(emptyField, 0, 0, 0, 4)
    //printFieldList(Field4)
    assert(Field4.foldLeft(0)((akk,f) => akk+(if(f._3) 1 else 0)) == 4)
    val Field3 = game setShipOnField(emptyField, 0, 0, 1, 3)
    //printFieldList(Field3)
    assert(Field3.foldLeft(0)((akk,f) => akk+(if(f._3) 1 else 0)) == 3)
    val Field2 = game setShipOnField(emptyField, 0, 1, 0, 2)
    //printFieldList(Field2)
    assert(Field2.foldLeft(0)((akk,f) => akk+(if(f._3) 1 else 0)) == 2)
    //printFieldList(newField)
  }

  "getZoneKoords" should "get the right safety zone for a ship" in {
    val emptyField = (for (x <- (0 to 9); y <- (0 to 9)) yield (x, y, false)).toList
    val game = new Game("", 0, "", RandomShoot)
 
    val zoneKoords = game getZoneKoords (0, 1, 1, 2)
    //println(zoneKoords)
  }

  "fitting" should "calculate the \"safety\" zone in horizontal direction" in {
    val emptyField = (for (x <- (0 to 9); y <- (0 to 9)) yield (x, y, false)).toList
    val game = new Game("", 0, "", RandomShoot)
    val newField = game setShipOnField(emptyField, 0, 0, 0, 5)
    val newField2 = game setShipOnField(newField, 0, 1, 0, 4)
    //printFieldList(newField2)
    
    assert(game.fitting(newField, 0, 1, 0, 4) == false)
    assert(game.fitting(newField, 0, 2, 0, 4) == true)
  }

  it should "also calculate the \"safety\" zone in vertical direction" in {
    val emptyField = (for (x <- (0 to 9); y <- (0 to 9)) yield (x, y, false)).toList
    val game = new Game("", 0, "", RandomShoot)

    //val newField3 = emptyField
    val newField3 = game setShipOnField(emptyField, 0, 0, 1, 5)
    val newField4 = game setShipOnField(newField3, 1, 0, 1, 4)
    //printFieldList(newField4)
    
    assertResult(false) {
      game.fitting(newField3, 1, 0, 1, 4)
    }
    assertResult(true) {
      game.fitting(newField3, 2, 0, 1, 4)
    }
  }

  it should "also calculate diagonal connections" in {
    val emptyField = (for (x <- (0 to 9); y <- (0 to 9)) yield (x, y, false)).toList
    val game = new Game("", 0, "", RandomShoot)

    val newField = game setShipOnField(emptyField, 1, 1, 0, 5)
    //val nextField = game setShipOnField(newField, 6, 3, 1, 4)
    //printFieldList(nextField)
    assertResult(false) {
      game.fitting(newField, 6, 2, 1, 4)
    }
    assertResult(true) {
      game.fitting(newField, 6, 3, 0, 2)
    }
    assertResult(true) {
      game.fitting(newField, 7, 2 , 0, 2)
    }

    val newField2 = game setShipOnField(emptyField, 3, 3, 0, 4)
    val nextField2 = game setShipOnField((game setShipOnField(newField2, 1, 2, 0, 2)), 1, 4, 0, 2)
    //printFieldList(nextField2)

    assertResult(false) {
      game.fitting(newField2, 1, 2, 0, 2)
    }
    assertResult(false) {
      game.fitting(newField2, 1, 4, 0, 2)
    }

    val newField3 = game setShipOnField(emptyField, 1, 0, 0, 4)
    val nextField3 = game setShipOnField(newField3, 0, 1, 1, 2)
    //printFieldList(nextField3)

    assertResult(false) {
      game.fitting(newField3, 0, 1, 1, 2)
    }
    
    val newField4 = game setShipOnField(emptyField, 2, 7, 1, 3)
    val nextField4 = game setShipOnField(newField4, 3, 6, 0, 2)
    //printFieldList(nextField4)

    assertResult(false) {
      game.fitting(newField4, 3, 6, 0, 2)
    }
  }

  "A Field" must "be valid in amount of fields" in {
    val game = new Game("", 0, "", RandomShoot)
    //printField(game initPosition)
    assert((game initPosition).foldLeft(0)((akk,f) => akk+(if(f.enthaelt == "Schiff") 1 else 0)) == 30)
  }
  def printField(field: List[Field]) {
    field match {
      case head::tail => {
	if(head.y == 1) println()
	if (head.enthaelt == "Schiff") print("x") else print("o")
	printField(tail)}
      case Nil => println()
    }
  }

  def printFieldList(list: List[(Int, Int, Boolean)]) {
    list match {
      case head::tail => {
	if(head._2 == 0) println()
	if (head._3) print("x") else print("o")
	printFieldList(tail)}
      case Nil => println()
    }
  }
}
