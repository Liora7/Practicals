/**  With Scala 2.12:
fsc -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar TestTest.scala
scala -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar org.scalatest.run TestTest
*/
import org.scalatest.FunSuite

class IntSetTest extends FunSuite{

  val set = new IntSet()

  test("add element"){
    set.add(1)
    assert(set.contains(1)===true)
    assert(set.contains(0)===false)
  }
  test("size of set"){
    assert(set.size===1)
  }
  test("remove element which is in the set"){
    assert(set.remove(1)===true)
    assert(set.contains(1)===false)
  }
  test("remove element which is not in the set"){
    assert(set.remove(0)===false)
    assert(set.contains(0)===false)
  }
  test("add element which is already in the set"){
    set.add(1)
    set.add(1)
    assert(set.size===1)
    assert(set.contains(1)===true)
  }
  test("test any"){
    assert(set.any===1)
    set.add(0)
    assert(set.any===0)
    set.add(2)
    assert(set.any===0)
    val emp = IntSet()
    intercept[AssertionError]{emp.any}
  }
  test("equals set"){
    val set1 =  IntSet(0, 1, 2)
    assert(set.equals(set1)===true)
    val set2 =  IntSet(0, 1, 3)
    assert(set.equals(set2)===false)
    val set3 =  IntSet(0, 1, 2, 3)
    assert(set.equals(set3)===false)
  }
  test("subset"){
    val set1 =  IntSet(0, 1, 2, 4)
    assert(set.subsetOf(set1)===true)
    val set2 =  IntSet(0, 1, 3, 4)
    assert(set.subsetOf(set2)===false)
    val set3 =  IntSet(0, 1, 2)
    assert(set.subsetOf(set3)===true)
    val set4 =  IntSet(0, 1, 3)
    assert(set.subsetOf(set4)===false)
    val set5 =  IntSet(0, 1)
    assert(set.subsetOf(set5)===false)
  }
  test("union"){
    val res1 = set.union(IntSet(0, 1, 2, 4))
    assert(res1.equals(res1))
    val res2 = set.union(IntSet(5, 6, 7))
    val set1 = IntSet(0, 1, 2, 5, 6, 7)
    assert(res2.equals(set1))
    assert(set.union(IntSet()).equals(set))
  }
  test("intersect"){
    val res1 = set.intersect(IntSet(0, 1, 2, 4))
    assert(res1.equals(IntSet(0, 1, 2)))
    val res2 = set.intersect(IntSet(5, 6, 7))
    assert(res2.equals(IntSet()))
    assert(set.intersect(IntSet()).equals(IntSet()))
  }
  test("map"){
    val f = (x: Int) => 2*x
    assert(set.map(f)===IntSet(0, 2, 4))
    val g = (x: Int) => x
    assert(set.map(g)===set)
    assert(IntSet().map(f)===IntSet())
  }
  test("filter"){
    val p = (x: Int) => x==0
    assert(set.filter(p)===IntSet(0))
    val q = (x: Int) => x<100
    assert(set.filter(q)===set)
    val r = (x: Int) => x>100
    assert(set.filter(r)===IntSet())
    assert(IntSet().filter(p)===IntSet())
  }
}
