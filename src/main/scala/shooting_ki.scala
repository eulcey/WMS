import scala.util.Random

trait ShootingMethod {
  def nextShot(field: List[Field]): (Char, Int)
}

case object RandomShoot extends ShootingMethod {
  def nextShot(field: List[Field]) = {
    val rand = new Random(System.currentTimeMillis())
    val list = field filter (f => !f.beschossen)
    val random_index = rand.nextInt(list.size)
    val el = list(random_index)
    (el.x, el.y)
  }
}

case object CheckerShoot extends ShootingMethod {
  var lastShot = ('A', 0)
  var hits = 0
  def nextShot(field: List[Field]) = {
    val newhits = field.foldLeft(0)((acc, f) => acc + (if (f.enthaelt == "Schiff") 1 else 0))
    lastShot = if(newhits > hits){
      /*hits = newhits*/ 
      ('A',0)
    } else {
      val rest = (field filter (f => (((f.x toInt) - 65) % 2 == 1) && f.y%2 == 0)) // filter from lastShot needed
      (rest.head.x, rest.head.y)
    }
    lastShot
  }

  // gets called if x,y was a Shot that sunk a Ship
  def sunkShot(x: Char, y: Int) {
  }
}
