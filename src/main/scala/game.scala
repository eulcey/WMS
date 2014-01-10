import scalaj.http._
import scala.util.Random
import scalaz._, Scalaz._
import argonaut._, Argonaut._

class Game(url: String, gameID: Int, secret_token: String, shootingMethod: ShootingMethod) {
  val rand = new Random(System.currentTimeMillis());

  def start() {
    val initShips = initPosition.asJson
    //println(initShips)
    val address = url + gameID + "/setup/" + secret_token
    val initAnswer = Http.postData(address, initShips.nospaces).method("PUT").asString
    val shipAnswer = initAnswer.decodeOption[InitAnswer]
    val shipsCorrect = shipAnswer match {case Some(x) => {//println(x.status);
      x.status == "OK"}; case None => false }
    //println("shipsCorrect = " + shipsCorrect)
    if(shipsCorrect) {
      val field = for ( c <- ('A' to 'J'); i <- (1 to 10)) yield Field(c, i, "Wasser", false)
      runGame(None, field.toList, url, gameID, secret_token)
    } else {
      println(shipAnswer match { case Some(x) => x.reason; case None => "no Ship Answer" })
    }
  }


  def runGame(endMessage: Option[String], field: List[Field], url: String, gameID: Int, secret_token: String): String = 
    endMessage match {
      case Some(message) => message
      case None => {
	val shot = shootingMethod.nextShot(field)
	runGame(endMessage, field, url, gameID, secret_token)
      }
    }

  def initPosition: List[Field] = {
    //println("start initPosition()")
    val shipList = List(5, 4, 4, 3, 3, 3, 2, 2, 2, 2)
    val emptyField = (for (x <- (0 to 9); y <- (0 to 9)) yield (x, y, false)).toList

    def setShips(field: List[(Int, Int, Boolean)], ships: List[Int]): List[(Int, Int, Boolean)] = 
      if(ships.empty) {field}
      else {
	setNextShip(field, ships.head) match {
	  case Some(x) => setShips(x, ships.tail)
	  case None => setShips(emptyField, shipList)
	}
      }

    def setNextShip(field: List[(Int, Int, Boolean)], ship: Int):  Option[List[(Int, Int, Boolean)]] = {
      //println("setNextShip(): " + ship)
      var fits = false
      var tries = 0
      var x = 0
      var y = 0
      var dir = 0
      val koords = while (!fits) {
	//println("koords search loop tries: " + tries)
	x = rand.nextInt(10)
	y = rand.nextInt(10)
	dir = rand.nextInt(2)
	fits = fitting(field, x, y, dir, ship) || tries == 10
	tries = tries + 1
	(x,y,dir)
      }
      if(tries < 10) {
	//println("trying t set on field")
	val erg = Some(setShipOnField(field, x, y, dir, ship))
	//println("set on field")
	erg
      } else {
	None
      }
    }
    (setShips(emptyField, shipList)) map (t => Field((t._1 + 65).toChar, t._2+1, if(t._3){"Schiff"}else{"Wasser"}, false))
  }

  // dir == 0 equals vertical direction
  def setShipOnField(field: List[(Int, Int, Boolean)], x: Int, y: Int, dir: Int, ship: Int): List[(Int, Int, Boolean)] = {
    val koords = if (dir == 0) { for (n_x <- (0 to ship-1)) yield (n_x+x,y)} else { for (n_y <-(0 to ship-1)) yield (x, n_y+y)}
    field map ( f => if (koords exists (k => k._1 == f._1 && k._2 == f._2)) {(f._1, f._2, true)} else {f})
  }
  def fitting(field: List[(Int, Int, Boolean)], x: Int, y: Int, dir: Int, ship: Int) = {
    val koords = (if (dir == 0) { for (n_x <- (0 to ship-1)) yield (n_x+x,y)} else { for (n_y <-(0 to ship-1)) yield (x, n_y+y)}).toList
    val z_koords = getZoneKoords(x, y, dir, ship) 
    koords.forall(k => k._1 >= 0 && k._1 < 10 && k._2 >= 0 && k._2 < 10) && field.forall(f => if(z_koords.exists(k => k._1 == f._1 && k._2 == f._2)) {!f._3} else {true})
  }

  def getZoneKoords(x: Int, y: Int, dir: Int, ship: Int): List[(Int, Int)] = {
    val koords = (if (dir == 0) { for (n_x <- (0 to ship-1)) yield (n_x+x,y)} else { for (n_y <-(0 to ship-1)) yield (x, n_y+y)}).toList
    val up = koords map (k => (k._1, k._2 -1))
    val down = koords map (k => (k._1, k._2 +1))
    val right = koords map (k => (k._1+1, k._2))
    val left = koords map (k => (k._1-1, k._2))
    val diags_h = (koords.head._1-1, koords.head._2-1) :: (koords.head._1, koords.head._2+1) :: (koords.head._1+1, koords.head._2-1) :: List.empty
    val diags_t = (koords.last._1+1, koords.last._2-1) :: (koords.last._1+1, koords.last._2+1) :: (koords.last._1-1, koords.last._2+1) :: List.empty
    diags_h ::: diags_t ::: koords ::: up ::: down ::: right ::: left
  }
}
