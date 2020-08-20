/** Import is for readLine so that we can write input directly to the program */
import scala.io.StdIn

object Brack{
	//Maximum length of word so we can define our arrays in dynamic programming
	val MAXWORD = 30

	//Operation to take 'A', 'B' and 'C' to corresponding Ints
  def LetterToInt(a: Char) : Int = {
		if(a == 'A' || a == 'B' || a == 'C'){
			return (a.toInt - 'A'.toInt);
		} else{
			println("Please only Letters from A,B,C.")
			sys.exit
		}
	}

  //Defining the op array for everything to use
  val op = Array.ofDim[Int](3,3)
  op(0)(0) = 1; op(0)(1) = 1; op(0)(2) = 0
	op(1)(0) = 2; op(1)(1) = 1; op(1)(2) = 0
	op(2)(0) = 0; op(2)(1) = 2; op(2)(2) = 2

  /** Read file into array (discarding the EOF character) */
  def readFile(fname: String) : Array[Char] =
    scala.io.Source.fromFile(fname).toArray.init


  /* Functions below here need to be implemented */


	//TASK 1
	//PossibleRec checks whether bracketing to something is possible recursively
	//Checks whether w(i,j) can be bracketed to z

	def PossibleRec(w: Array[Int], i: Int, j: Int, z: Int): Boolean = {
		var split = i + 1
		// i<split<j
		while (split < j){
			// go through all places where w can be split and recursively partition the two halves
			for (a <- 0 to 2) {
				for (b <- 0 to 2) {
					val x = PossibleRec(w, i, split, a)
					val y = PossibleRec(w, split, j, b)
					// go through all possible values that can be made from each half and check if they can be used to make z
					if (x && y && op(a)(b)==z) return true
				}
			}
		split += 1
		// check next split
		}
		if (j-i==1 && w(i)==z) true else false
		// check if single letter is z
	}


	//TASK 2
	//NumberRec which checks the ways you get a result recursively
	//Computes number of ways w(i,j) can be bracketed to get z

	def NumberRec(w: Array[Int], i: Int, j: Int, z:Int): Int = {
		var split = i + 1
		// i<split<j
		var count = 0
		// 0<=count && count:=# of ways to bracket and get z
		while (split < j){
			// go through all places where w can be split and recursively partition the two halves
			for (a <- 0 to 2) {
				for (b <- 0 to 2) {
					val x = NumberRec(w, i, split, a)
					val y = NumberRec(w, split, j, b)
					// go through all possible values that can be made from each half, count the ways in which they can be made and get number of ways in which they can be combined to form z
					if (op(a)(b)==z) count += (x*y)
					// x*y is 0 if one of the two is 0 i.e. if it is impossible to get either a or b from the split
				}
			}
		split += 1
		// check next split
		}
		if (j-i==1 && w(i)==z) count += 1
		// check if single letter is z and if so, increment count
		count
	}


	//TASK 3
	/*
		Both procedures check every possible grouping of 2 possibly themselves composite letters. So at the first step, we have nC2, then we have (n-1)C2, and so on, until we reach 2C2. In total then we have:
		nC2 * (n-1)C2 * (n-2)C2 * ... * 3C2 * 2C2.
		The factorials cancel to give (n! * (n-1)!)/(2^(n-2)) which is O(n^2n).

		Tests:
		1 letter: 0m0.672s
		2 letters: 0m0.688s
		3 letters: 0m0.682s
		4 letters: 0m0.693s
		5 letters: 0m0.837s
		6 letters: 0m1.617s
		7 letters: 0m4.152s
		8 letters: 0m26.407s
		9 letters: 7m21.497s
	*/

	//You may find the following class useful for Task 7
	// Binary tree class
	abstract class BinaryTree
	case class Node (left : BinaryTree, right : BinaryTree) extends BinaryTree
	case class Leaf (value : Char) extends BinaryTree

	//Printing for a binary tree
	def print_tree(t : BinaryTree) = {
		println(treetoStr(t))
		// print tree string
	}

	def treetoStr(t : BinaryTree): String = {
		// recursively convert tree into bracketed string, grouping pairs of 2 together
		var st = ""
		t match {
			case Leaf(x: Char) => st = x.toString
			case Node(l, r) => st = ("(" + treetoStr(l) + treetoStr(r) + ")")
		}
		st
	}

	def letter(z: Int): Char = {
		z match {
			case 0 => 'A'
			case 1 => 'B'
			case 2 => 'C'
		}
		// convert back into chars based on def
	}

	//These arrays should hold the relevant data for dynamic programming
	var poss = Array.ofDim[Boolean](MAXWORD, MAXWORD, 3)
	var ways = Array.ofDim[Int](MAXWORD, MAXWORD, 3)
	var exp = Array.ofDim[BinaryTree](MAXWORD, MAXWORD, 3)


	//Task 4, 5, and 7(optional)


	def Tabulate(w: Array[Int], n: Int): Unit = {
		for (length <- 1 to n){
			// start with length=1 and fill in table, in order to be able to break problems into shorter words (already filled in) later on
			for (i <- 0 to n-length){
				// move i from 0 to n-length to get different sub-words
				val j = i + length
				// set j so that j-i=length
				for (z <- 0 to 2){
					// fill in poss & ways for each z
						var split = i + 1
						// i<split<j
						while (split < j){
							// go through all places where w can be split and look up the two halves in the matrices
							for (a <- 0 to 2){
								for (b <- 0 to 2){
									// go through all possible values that can be made from each half, count the ways in which they can be made and get number of ways in which they can be combined to form z, as well as fetch example bracketing of each substring and combine into bigger binary tree example
										val x = poss(i)(split)(a)
										val xcount = ways(i)(split)(a)
										val xexp = exp(i)(split)(a)
										val y = poss(split)(j)(b)
										val ycount = ways(split)(j)(b)
										val yexp = exp(split)(j)(b)
										if (x && y && op(a)(b)==z) {
											poss(i)(j)(z) = true
											ways(i)(j)(z) += (xcount * ycount)
											exp(i)(j)(z) = Node(xexp, yexp)
											// same principles as PossibleRec and Number-Rec
								}
							}
						}
						split += 1
						// check next split
					}
					if (length == 1 && w(i)==z) {
						poss(i)(j)(z) = true
						ways(i)(j)(z) += 1
						exp(i)(j)(z) = Leaf(letter(z))
						// check if single letter is z and if so, increment count and set poss to true, create leaf of single letter (call letter(z) to convert number to char)
				}
			}
		}
	}
}

	//Task 6
	/* Tabulate can correctly determine the number of bracketings for words up to 20 characters, as the total numbers of bracketings for words of each length form the sequence of Catalan numbers; supposing there is a word at each length for which all bracketings result in the same letter, we need the counter of that letter to be able to hold a Catalan number. The biggest value an Int can hold is 2147483647, and the largest Catalan number less than or equal to that is the 20th. Hence in the worst case, we can only store the number of possible bracketings for words with up to 20 characters.
	Tabulate should take O(n^2), as the main work is done in the two for loops iterating through values of length and i, which is O(n^2), and the other loops add a constant of 8 to the leading term of n^2, which is irrelevant to the complexity.

	Tests:
	1 letter: 0m0.700s
	2 letters: 0m0.694s
	3 letters: 0m0.693s
	4 letters: 0m0.702s
	5 letters: 0m0.695s
	6 letters: 0m0.718s
	7 letters: 0m0.701s
	8 letters: 0m0.701s
	9 letters: 0m0.701s
	...
	20 letters: 0m0.725s
	...
	30 letters: 0m0.762s

	Tabulate is too fast to see a difference between these different word lengths of up to MAXWORD, which is much better than the recursive version which could not be used for words longer than 9 letters (it already took over 7 minutes for 9 letters). The order of magnitude of the two functions is also worlds apart: O(n^2) is much more efficient than O(n^2n), which is infeasible. However, Tabulate requires 6n^2 of space for its two matrices, while the recursive version only needs constant extra space.
	*/

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {

    // string to print if error occurs
    val errString =
      "Usage: scala Brack -PossibleRec [file]\n"+
      "     | scala Brack -NumberRec [file]\n"+
      "     | scala Brack -Tabulate [file]\n"

		if (args.length > 2){
			println(errString)
			sys.exit
		}

    //Get the plaintext, either from the file whose name appears in position
    //pos, or from standard input
    def getPlain(pos: Int) =
      if(args.length==pos+1) readFile(args(pos)) else StdIn.readLine.toArray

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
		val plain = getPlain(1)
    val command = args(0)

		//Making sure the letters are of the right type
		val len = plain.length
		var plainInt = new Array[Int](len)
		if (len > MAXWORD){
			println("Word Too Long! Change MAXWORD")
			sys.exit;
		} else {
    	for (i <- 0 until len){
				plainInt(i) = LetterToInt(plain(i))
			}
		}

		//Executing appropriate command
    if(command=="-PossibleRec"){
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			if(PossibleRec(plainInt, 0, len, i)){
				println(('A'.toInt + i).toChar + " is Possible");
			}
			else{
				println(('A'.toInt + i).toChar + " is not Possible");
			}
		}
    }
    else if(command=="-NumberRec"){
		var z: Int = 0
		println("Bracketing values for "+ plain.mkString(""))
		for(i<-0 to 2){
			z = NumberRec(plainInt, 0, len, i)
			if(z == 1){
				printf(('A'.toInt + i).toChar+ " can be achieved in %d way\n", z)
			}
			else{
				printf(('A'.toInt + i).toChar+ " can be achieved in %d ways\n", z)
			}
		}
    }

    else if(command=="-Tabulate"){
		Tabulate(plainInt,len)
		println("Bracketing values for "+ plain.mkString(""))
		for(v<-0 to 2){
		var z: Int = ways(0)(len)(v)
			if(z==0){
			println(('A'.toInt + v).toChar+ " cannot be achieved")
			}
			else if(z==1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d way\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
			else if (z > 1){
				printf(('A'.toInt + v).toChar+ " can be achieved %d ways\n", z)
				printf("For example:")
				print_tree(exp(0)(len)(v))
				printf("\n")
			}
		}
	}
    else println(errString)
  }
}
