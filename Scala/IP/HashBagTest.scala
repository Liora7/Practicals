import org.scalatest.FunSuite
/*
fsc HashBag.scala
fsc -cp .:./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar HashBagTest0.scala
scala -cp .:./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar org.scalatest.run HashBagTest
*/
class HashBagTest extends FunSuite{
  val bag  = new HashBag
  test("add"){
    bag.add("a"); bag.add("a")
    assert(bag.count("a")==2 && bag.count("b")==0)
    assert(bag.size == 1)
    for (i <- 0 to 10) bag.add("elem"+i)
    assert(bag.size == 12)
  }

  test("delete"){
    bag.delete("a")
    assert(bag.size == 12)
    assert(bag.count("a")==1)
    bag.delete("elem1")
    assert(bag.size == 11)
    assert(bag.count("elem1")==0)
  }
}
