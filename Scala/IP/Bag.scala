// state: bag : Int → Int
// init: bag = {} */

trait Bag{
/** Add the maplet Int -> Count to the mapping.
* post: bag = bag0 ∪ {number → count+1} */
def add(number: Int)

/** Return the count of a number.
* pre: number ∈ dom bag
* post: bag = bag0 ∧ returns bag(number) */
def find(number: Int) : Int

}
