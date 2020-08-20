/**  With Scala 2.12:
fsc -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar TestTest.scala
scala -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar org.scalatest.run TestTest
*/
import org.scalatest.FunSuite


class StringOrder extends FunSuite{
  var array = Array("abc", "total", "haskell", "fun", "any", "tea")
  test("default string order"){
    assert(array.sorted===Array("abc", "any", "fun", "haskell", "tea", "total"))
  }
  test("map multiply"){
    assert(array.map(_*2)===Array("abcabc", "totaltotal", "haskellhaskell", "funfun", "anyany", "teatea"))
  }
  test("reverse"){
    assert(array.reverse===Array("tea", "any", "fun", "haskell", "total", "abc"))
  }
  test("map reverse"){
    assert(array.map(_.reverse)===Array("cba", "latot", "lleksah", "nuf", "yna", "aet"))
  }
  test("max"){
    assert(array.max==="total")
  }
  test("sort by length"){
    assert(array.sortBy(_.length)===Array("abc", "fun", "any", "tea", "total", "haskell")) 
  }
}


/*
//  Corrected:
class TestTest extends FunSuite{
  var x = 0
  test("x=0"){
    x=0
    assert(x===0)
  }
  test("x=1"){
    x=1
    assert(x===1)
  }
}
*/
