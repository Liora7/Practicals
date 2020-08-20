/** state: S : P Int , Int ⊆ [0..N)
* init: S = {} */

trait IntSet{
/** Add elem to the set.
* pre: elem∈[0..N)
* post: S = S0 ∪ {elem} */
def add(elem: Int)

/** Does the set contain elem?
* post: S = S0 ∧ returns elem ∈ S */
def isIn(elem: Int): Boolean

/** Remove elem from the set.
* post: S = S0 − {elem} */
def remove(elem: Int)

/** The size of the set.
* post: S = S0 ∧ returns #S */
def size : Int
}
