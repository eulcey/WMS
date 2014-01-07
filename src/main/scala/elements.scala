import scalaz._, Scalaz._
import argonaut._, Argonaut._
import scala.util.Random

//def shotStream(playfield: Array[Int]) = {
  //val nextShot = calcShot(playfield)
  //Stream.cons(nextShot, shotStream(playfield.map )
//}

//case class Shot(x: Int, y: Int)

//object Shot {
//  implicit def ShotCodecJson: CodecJson[Shot] =
//    casecodec2(Shot.apply, Shot.unapply)("x", "y")
//}

  //x: <Buchstabe von A bis J>,
  //y: <Zahl von 1 bis 10>,
  //enthaelt: <"Wasser", "Schiff", "unbekannt"},
  //beschossen: <true, false, "unbekannt">

case class Field(x: Char, y: Int, enthaelt: String, beschossen: Boolean)

object Field {
  implicit def FieldCodecJson: CodecJson[Field] =
    casecodec4(Field.apply, Field.unapply)("x", "y", "enthaelt", "beschossen")
}

//  enemyshot: {
//     x: <Buchstabe von A bis J>,
//     y: <Zahl von 1 bis 10>,
//   status: <"you are next" oder "you won" oder "you lost">

case class EnemyShot(x: Int, y: Int, status: String)

object EnemyShot {
  implicit def EnemyShotCodecJson: CodecJson[EnemyShot] = 
    casecodec3(EnemyShot.apply, EnemyShot.unapply)("x", "y", "status")
}

case class InitAnswer(status: String, token: Option[String], reason: Option[String])

object InitAnswer {
  implicit def InitAnswerCodecJson: CodecJson[InitAnswer] =
    casecodec3(InitAnswer.apply, InitAnswer.unapply)("status", "token", "reason")
}

case class ShotAnswer(status: String, result: Option[String], reason: Option[String])

object ShotAnswer {
  implicit def ShotAnswerCodecJson: CodecJson[ShotAnswer] =
    casecodec3(ShotAnswer.apply, ShotAnswer.unapply)("status", "result", "reason")
}

case class WaitAnswer(enemyshot: Option[EnemyShot], status: String, reason: Option[String])

object WaitAnswer {
  implicit def WaitAnswerCodecJson: CodecJson[WaitAnswer] =
    casecodec3(WaitAnswer.apply, WaitAnswer.unapply)("enemyshot", "status", "reason")
}

trait ShootingMethod {
  def nextShot(field: List[Field]): (Char, Int)
}

case object RandomShoot extends ShootingMethod {
  def nextShot(field: List[Field]) = {
    val rand = new Random(System.currentTimeMillis());
    val list = for (c <- ('A' to 'J'); i <- (1 to 10)) yield (c,i)
      val random_index = rand.nextInt(list.size);
    list(random_index);
  }
}
