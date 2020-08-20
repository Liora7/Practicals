import org.scalacheck._
import org.scalacheck.Prop._

object Q_Text extends org.scalacheck.Properties("Text") {
  property("insert at start") =
    forAll { (s: String) =>
        val t = new Text(); t.insert(0, s)
        t.toString() == s
      }
      property("insert at end") =
        forAll { (s1: String, s2: String) =>
          val t = new Text(s1); t.insert(t.length, s2)
          t.toString() == s1 + s2
        }
}
