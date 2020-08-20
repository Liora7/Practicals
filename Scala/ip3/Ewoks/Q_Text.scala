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
      property("insert in the middle") =
            forAll { (s1: String, s2: String, s3: String) =>
              val t = new Text(s1 + s2); t.insert(t.length, s3)
              t.toString() == s1 + s3 + s2
            }
    property("delete at start") =
        forAll { (s: String) =>
            val t = new Text(s); t.deleteRange(0, s.length)
            t.toString() == ""
          }
          property("delete at end") =
            forAll { (s1: String, s2: String) =>
              val t = new Text(s1 + s2); t.deleteRange(s1.length, t.length)
              t.toString() == s1
            }
    property("delete in the middle") =
            forAll { (s1: String, s2: String, s3: String) =>
              val t = new Text(s1 + s3 + s2); t.deleteRange(s1.length, s2.length)
              t.toString() == s1 + s2

        }
}
