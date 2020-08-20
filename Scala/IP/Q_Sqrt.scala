import org.scalacheck._
import org.scalacheck.Prop._

object Q_Sqrt extends org.scalacheck.Properties("Sqrt") {
  property("is a root") = forAll { (n: Int) =>
    (n >= 0) ==> {
    val t = n/100000
    scala.math.sqrt(t*t) == t
  }
  }
}
