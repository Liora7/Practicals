import scala.swing._
import scala.swing.event._
import WordPaths._

object WordPathGUI extends SimpleSwingApplication {
    def top = new MainFrame {

        title = "Word Path Finder"
        //object startw extends TextField { columns = 5 }
        val startw  = new TextField { columns = 5 }
        object targetw extends TextField { columns = 5 }

        contents = new FlowPanel {
            contents += startw
            contents += new Label(" Start    =    ")
            contents += targetw
            contents += new Label(" Target")
            border = Swing.EmptyBorder(10, 10, 10, 10)
        }

        listenTo(startw, targetw)

        reactions += {
            case EditDone(field) =>
                val s = startw.text.toString
                val t = targetw.text.toString
                val res = findPath(s, t)
                if (!res == None) printPath(res.get)
                else print("No path found")
        }
    }
}
