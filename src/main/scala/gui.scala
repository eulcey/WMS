import swing._
import swing.event._
import java.awt.{ Graphics2D, Color }

object WMSGui  extends SimpleSwingApplication {
  val game = new Game("", 0, "", RandomShoot)
  val fieldList = game initPosition
    val canvas = new Canvas {
      preferredSize = new Dimension(550, 550)
    }
  val buttonList = fieldList map (f => (new Button))// { text = if(f.enthaelt=="Schiff") "X" else "" }))

  def top = new MainFrame {
    title = "Test"

    contents = new BoxPanel(Orientation.Horizontal) {
      contents += new GridPanel(10,10) {
	for (b <- buttonList)
	  contents += b
      }
      contents += new Button { text  = "test"}

      contents += canvas
    }
    canvas drawField fieldList
  }
}

class Canvas extends Panel {
  var centerColor = Color.yellow
  var field = List.empty[Field]

  override def paintComponent(g: Graphics2D) {
    val s_width = size.width - 50
    val s_height = size.height - 50
    val f_width = s_width/10
    val f_height = s_height/10
   
    // Start by erasing this Canvas
    g.clearRect(0, 0, size.width, size.height)
    
    // Draw background here
    g.setColor(Color.blue)
    g.fillRect(f_width, f_height, s_width, s_height)
    g.setColor(Color.red)
    for (c <- 0 to 9) {
      g.drawString( ((c + 65)toChar).toString, ((c+1.5) * f_width)toInt, f_height)//0 , ,f_width, f_height)
      g.drawString( (c + 1) toString, (f_width/2)toInt, ((c+1.5) * f_height)toInt)
      // g.fillRect(0, (c+1)*f_height, f_width, f_height)
    }
    //g.fillOval(20, 20, 60, 60)
    //g.setColor(centerColor)
    //g.fillOval(40, 40, 20, 20)
    for(f <- field) {
      if (f.enthaelt == "Schiff") {
	g.setColor(Color.black)
	g.fillRect((f.y)*f_width, ((f.x toInt)-64)*f_height, f_width, f_height)
      }
    }
    
  }

  def drawField(nfield: List[Field]) {
    field = nfield
    // Tell Scala that the display should be repainted
    repaint()
  }
}
