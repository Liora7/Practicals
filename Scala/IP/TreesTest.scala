/*
fsc -cp .:./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar BinaryTreeBag.scala BinaryTreeTest.scala
scala -cp .:./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar org.scalatest.run BinaryTreeTest
*/
import org.scalatest.FunSuite

class TreesTest extends FunSuite{
  val bag = new Trees

  test("one"){
    var tr = Tree("three", Tree("four", Tree("five",null,null), Tree("six",
    Tree("seven", Tree("one",null,null), null), null)), Tree("two",null,null))
    println(tr)
  }
}
