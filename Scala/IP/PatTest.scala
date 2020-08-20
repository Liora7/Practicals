/**  With Scala 2.12:
fsc -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar TestTest.scala
scala -cp ./scalatest_2.12-3.0.5.jar:./scalactic_2.12-3.0.5.jar org.scalatest.run TestTest
*/
import org.scalatest.FunSuite
import Div.search


class PatTest extends FunSuite{
  test("mistake a: replace initialization found=false by found=true"){
    assert(search("hem".toArray, "he".toArray)===false)
    // as the loop tests for j<=N-K &&!found, if found is initially true the loop is never entered so the program will always return true.
  }
  test("mistake b: replace <= by < in while loop condition"){
    assert(search("almost".toArray, "almost".toArray)===true)
    // since j<N-K, if N=K, and j=0, the condition is false, the loop is never entered, and found's initial value of false is output even if the two input strings were identical.
  }
  test("mistake c: replace N-K by N-K+1"){
    assert(search("hah".toArray, "ha".toArray)===false)
    // this will give an out of bounds exception when line is indexed at position j+k when k=K-1 as then the index will be N-K-1+1+K = N even though the maximum index in line is N-1.
  }
  test("mistake d: replace k=0 by k=1"){
    assert(search("a".toArray, "o".toArray)===false)
    // this will return true if k is initialized to 1, as for K=1, k<K will then test false, so the program skips to found = (k==K) which is true, and hence sets found to true even though the strings are different.
  }
  test("mistake e: replace < by <= in while loop condition"){
    assert(search("hon".toArray, "honey".toArray)===true)
    // this will give an out of bounds exception because if the last character of pat matches up with line, then the condition k<=K returns true once more when k=K, leading to the program testing the next condition where it compares pat(K), even though if pat.size=K, the maximum index is K-1.
  }
  // mistake f will not lead to an error as the condition in the while loop requires that k<K strictly, so afetr k=k+1, k's maximum value will be K. Therefore changing the test k==K to k>=K makes no difference as k can never be greater than K.
}
