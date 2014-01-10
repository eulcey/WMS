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
  def nextShot(field: List[Field]) = {
    ('a', 0)
  }
}
