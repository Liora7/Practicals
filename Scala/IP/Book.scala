/** The state of a phone book, mapping names (Strings) to numbers (also Strings).
* state: book : String → String
* init: book = {} */

trait Book{
/** Add the maplet name -> number to the mapping.
* post: book = book0 ⊕ {name → number} */
def store(name: String, number: String)

/** Return the number stored against name.
* pre: name ∈ dom book
* post: book = book0 ∧ returns book(name) */
def recall(name: String) : String

/** Is name in the book?
* post: book = book0 ∧ returns name ∈ dom book */
def isInBook(name: String) : Boolean

/** Delete the number stored against name (if it exists)
	* pre: book = name -> number ∪ book0 || book0
	* post: book0 ∧ returns name ∈ dom book */
def delete(name: String) : Boolean

}
