import scala.swing._
import scala.swing.event._

object WordPathGUI extends SimpleSwingApplication {
    def top = new MainFrame {

        title = "Word Path Finder"
        //object startw extends TextField { columns = 5 }
        val startw  = new TextField { columns = 5 }
        val targetw = new TextField { columns = 5 }
        val initb = new Button("Search")
        val path = new Label("Path: ")
        val resultField = new TextArea {
          rows = 10
          lineWrap = true
          wordWrap = true
          editable = false
        }

        val input = new FlowPanel {
            contents += startw
            contents += new Label(" Start    =    ")
            contents += targetw
            contents += new Label(" Target   =    ")
            contents += initb
            contents += path
            border = Swing.EmptyBorder(10, 10, 10, 10)
        }
        contents = new BoxPanel(Orientation.Vertical) {
            contents += input
            contents += Swing.VStrut(10)
            contents += new ScrollPane(resultField)
            border = Swing.EmptyBorder(10, 10, 10, 10)
          }

        listenTo(initb)

        reactions += {
            case ButtonClicked(`initb`) =>
                val s = startw.text.toString
                val t = targetw.text.toString
                if (s.length != t.length) resultField.text = "Word lengths don't match up"
                else {
                  WordPaths.init()
                  val res = WordPaths.findPath(s, t)
                  if (res != None) {
                    val lwords: List[String] = res.get
                    resultField.text  = lwords.mkString("\n")
                  }
                  else resultField.text = "No path found"
                }
        }
    }
}
