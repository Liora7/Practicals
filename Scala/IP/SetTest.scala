/**  With Scala 2.12:
fsc -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar TestTest.scala
scala -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar org.scalatest.run TestTest
*/
import org.scalatest.FunSuite

class SetTest extends FunSuite{

  val set = new scala.collection.mutable.HashSet[String]

  test("add element"){
    assert(set.add("first")===true)
  }
  test("remove element in the set"){
    assert(set.remove("first")===true)
  }
  test("remove element which is not in the set"){
    assert(set.remove("second")===false)
  }
  test("add element which is already in the set"){
    set.add("first")
    assert(set.add("first")===false)
  }
  test("test for empty set"){
    assert(set.isEmpty===false)
  }
  test("size of set"){
    set += ("one", "two")
    assert(set.size===3)
  }
  test("try to group set into groups of size -2"){
    intercept[IllegalArgumentException]{ set.grouped(-2) }
  }
}
