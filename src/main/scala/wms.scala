import scalaj.http._
import scala.util.Random
import scalaz._, Scalaz._
import argonaut._, Argonaut._

object WMS{
  val rand = new Random(System.currentTimeMillis());
  // val url = "http://192.168.1.16:1337/"

  val shootingMethod = RandomShoot
  val gameID = rand.nextInt()

  def main(args: Array[String]) {
    val url = "http://" + args(0) + "/"
    println("starting new game with ID: " + gameID)
    val answer = Http.post(url+gameID).option(HttpOptions.connTimeout(1000)).option(HttpOptions.readTimeout(5000)).method("PUT").asString
    println(answer)
    val connectAnswer = answer.decodeOption[InitAnswer]
    println(connectAnswer)
    val connected = connectAnswer match { case Some(x) => x.status == "OK"; case None => false }
    if(connected) {
      val token_option: Option[String] = connectAnswer match { case Some(x) => x.token; case None => None}
      val secret_token: String = token_option match {case Some(x) => x;
					      case None => ""}
      val game = new Game(url, gameID, secret_token, RandomShoot)
      game.start
   } else {
      println(connectAnswer match { case Some(x) => x.reason; case None => "No Answer"})
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
    println("start initPosition()")
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
      println("setNextShip()")
      var fits = false
      var tries = 0
      var x = 0
      var y = 0
      var dir = 0
      val koords = while (!fits) {
	println("koords search loop tries: " + tries)
	x = rand.nextInt(10)
	y = rand.nextInt(10)
	dir = rand.nextInt(1)
	fits = fitting(field, ship, x, y, dir) || tries == 10
	tries = tries + 1
	(x,y,dir)
      }
      if(tries < 10) {
	println("trying t set on field")
	val erg = Some(setShipOnField(field, x, y, dir, ship))
	println("set on field")
	erg
      } else {
	None
      }
    }
    (setShips(emptyField, shipList)) map (t => Field((t._1 + 65).toChar, t._2+1, if(t._3){"Schiff"}else{"Wasser"}, false))
  }
  
  def setShipOnField(field: List[(Int, Int, Boolean)], x: Int, y: Int, dir: Int, ship: Int): List[(Int, Int, Boolean)] = {
    val koords = if (dir == 0) { for (n_x <- (0 to ship)) yield (n_x,y)} else { for (n_y <-(0 to ship)) yield (x, n_y)}
    field map ( f => if (koords exists (k => k._1 == f._1 && k._2 == f._2)) {(f._1, f._2, true)} else {f})
  }
    //    val test = while(part< ship) {
    //     val p_x = x - (i * (dir - 1))
    //     val p_y = y + (dir * i)
    //    }
    //    var newField = Array.fill(10,10)(false)
//    (for (n_x <- (0 to 9); n_y <- (0 to 9)) 
//    yield if(n_x >= x && n_y >= y && n_x <= x - ship * (dir-1) && n_y <= y + (dir * ship)) {(n_x, n_y, true)}
//	  else {
//	    (field find {f => f._1 == n_x && f._2 == n_y}) match {case None => {println("shpis eror");throw new Exception }
//								  case Some(x) => x}
//	  }).toList
    

  def innerPossible(x: Char, y: Int, s: Int) = (x <= 'J' - s) && (y <= 11 - 2)

  def fitting(field: List[(Int, Int, Boolean)], ship: Int, x: Int, y: Int, dir: Int) = {
    //var n_x = if (dir == 0) { (x to (x+ship))} else (x to x)
    //var n_y = if (dir == 1) { (y to (y+ship) )} else (y to y)
    var koords = if (dir == 0) { for (n_x <- (0 to ship)) yield (n_x,y)} else { for (n_y <-(0 to ship)) yield (x, n_y)}
    // ! needed ! zone around ship
    field.forall(f => if(koords.exists(k => k._1 == f._1 && k._2 == f._2)) {!f._3} else {true})
  }
//    (for (i <- 0 to ship-1) yield (x - (i * (dir - 1)), y + (dir * i))).forall (
//      koord => !field(koord._1)(koord._2))
  
  def fillShipField(old_field: Array[Array[Boolean]], target: (Int, Int, Int), ship: Int) = {
    val x = target._1
    val y = target._2
    val dir = target._3
    old_field
    //	(for (i <- 0 to ship-1) yield (x - (i * (dir - 1)), y + (dir * i))).foreach (
    //	  koord => old_field(koord._1)(koord._2))
  }
  
  def getZoneKoord(x: Int, y: Int, dir: Int, ship: Int) {
    for (s <- 0 to ship) yield (x - (s * (dir - 1)), y+ (dir * s))
  }
}
