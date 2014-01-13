import scalaj.http._
import scala.util.Random
import scalaz._, Scalaz._
import argonaut._, Argonaut._

object WMS{
  val rand = new Random(System.currentTimeMillis());
  // val url = "http://192.168.1.16:1337/"

  val shootingMethod = RandomShoot
  val gameID = rand.nextInt()

  //def main(args: Array[String]) {
  def start(ip: Int, games: Int) {
    val url = "http://" //+ args(0) + "/"
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
    
}
