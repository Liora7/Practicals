/*
fsc -cp .:./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar BinaryTreeBag.scala BinaryTreeTest.scala
scala -cp .:./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar org.scalatest.run BinaryTreeTest
*/
import org.scalatest.FunSuite

class BinaryTreeTest extends FunSuite{
  val bag = new BinaryTreeBag

  test("one"){
    assert(bag.depths===(0, 0))
    assert(bag.tdepths===(0, 0))
    assert(bag.getT===0)
    assert(bag.stackT===0)
    for(w <- List("c", "a", "f", "k", "d", "e", "1", "f", "g",
                  "l", "z", "y", "s", "r", "q"))
      bag.add(w)
    bag.printBag
    assert(bag.count("c")===1); assert(bag.count("a")===1)
    assert(bag.count("f")===2); assert(bag.count("p")===0)
    assert(bag.getT===15)
    assert(bag.stackT===15)
    assert(bag.depths===(2, 9))
    assert(bag.tdepths===(2, 9))
  }
  test("delete repeated entry"){
    bag.delete("f"); assert(bag.count("f")===1);
    assert(bag.getT===14)
    assert(bag.stackT===14)
    assert(bag.depths===(2, 9))
    assert(bag.tdepths===(2, 9))
  }
  test("delete1"){
    bag.delete("c"); assert(bag.count("c")===0);
  }
  test("delete -- not present"){
    bag.delete("c"); assert(bag.count("c")===0);
  }
  test("delete2"){
    bag.delete("a"); assert(bag.count("a")===0);
    assert(bag.count("h")===0);
  } // ("k", "d", "e", "1", "f", "g", "l", "z", "y",  "s", "r", "q")
  test("re-add"){
    bag.add("a"); assert(bag.count("a")===1);
  }
  test("delete3"){
    bag.delete("g"); bag.delete("y");
    assert(bag.count("g")===0)
    bag.printBag
  }// ("a", "k", "d", "e", "1", "f",  "l", "z",  "s", "r", "q")
  test("delete4"){
    bag.delete("g"); bag.delete("y"); bag.delete("a"); bag.delete("l");
    assert(bag.count("g")===0)
    bag.printBag
  }// ("k", "d", "e", "1", "f",  "z",  "s", "r", "q")
}
