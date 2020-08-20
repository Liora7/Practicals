// Using a binary tree to represent a bag of words

// Companion object defines the type of trees
object BinaryTreeBag{
  private class Tree(var word: String, var count: Int,
                     var left: Tree, var right: Tree)
}

class BinaryTreeBag{
  // Define shorthands, so we can write "Tree" rather than
  // "BinaryTreeBag.Tree"
  private type Tree = BinaryTreeBag.Tree
  private def Tree(word: String, count: Int, left: Tree, right: Tree) =
    new BinaryTreeBag.Tree(word, count, left, right)

  private var root : Tree = null
  // This represents the bag containing n.count copies of n.word, for each
  // node n reachable from root by following left and right references.
  // Datatype invariant: the tree is ordered by word fields.

  // Add an occurrence of word to the tree.  This version uses the recursive
  // function below.
  // def add(word: String) = root = addToTree(word, root)

  /** Find the count stored for a particular word */
  // def count(word: String) : Int = countInTree(word, root)

  /** Find the count stored for a particular word, within t */
  private def countInTree(word: String, t: Tree) : Int =
    if(t == null) 0
    else if(word == t.word) t.count
    else if(word < t.word) countInTree(word, t.left)
    else countInTree(word, t.right)

  /** Find count stored for particular word, iterative version */
  def count(word: String) : Int = {
    var t = root
    // Invariant: if word is within the main tree, then it is
    // within the tree rooted at t:
    // for all t1: if t1 in T(root) and t1.word = word,
    // then t1 in T(t)
    while(t != null && t.word != word)
      if(word < t.word) t = t.left else t = t.right
    if(t == null) 0 else t.count
  }

  /** Add an occurrence of word to the given tree, returning the
    * new tree. */
  // This version updates existing nodes.
  private def addToTree(word: String, t: Tree) : Tree =
    if(t == null) Tree(word, 1, null, null)
    else if(word == t.word){ t.count += 1; t }
    else if(word < t.word){ t.left = addToTree(word, t.left); t }
    else{ t.right = addToTree(word, t.right); t }

  /** Add an occurrence of word to the tree */
  // iterative version
  def add(word: String) =
    if(root==null) root = Tree(word, 1, null, null)
    else{
      var t = root
      // Invariant: word needs to be inserted within the tree
      // rooted at t; t != null.
      while(word < t.word && t.left != null ||
            word > t.word && t.right != null)
      {
        if(word < t.word) t = t.left else t = t.right
      }
      if(word == t.word) t.count += 1
      // In the cases below, t.left, resp. t.right, is null
      else if(word < t.word) t.left = Tree(word, 1, null, null)
      else t.right = Tree(word, 1, null, null)
    }


  /** Print the contents of the entire bag tree */
  def printBag = printTree(root)

  /** Print the contents of tree t */
  private def printTree(t: Tree) : Unit =
    if(t!=null){
      printTree(t.left)
      println(t.word+" -> "+t.count)
      printTree(t.right)
    }


  /** Print the contents of the tree */
  def printIterative = {
    var t = root
    val stack = new scala.collection.mutable.Stack[Tree]

    // Invariant: We still need to print t; and for each tree t1
    // in the stack, we still need to print the data in the top
    // node, and the data in the nodes of the right subtree
    // (in the order of the stack).
    while(t != null || !stack.isEmpty){
      if(t != null){ stack.push(t); t = t.left }
      else{
        val t1 = stack.pop
        println(t1.word+" -> "+t1.count)
        t = t1.right
      }
    }
  }

  /** Delete one occurrence of word from the tree. */
  def delete(word: String) : Unit = root = deleteFromTree(word, root)

  /** Delete one occurrence of word from t, returning the
    * resulting tree. */
  private def deleteFromTree(word: String, t: Tree) : Tree =
    if(t == null) null
    else if(word < t.word){ t.left = deleteFromTree(word, t.left); t }
    else if(word > t.word){ t.right = deleteFromTree(word, t.right); t }
    else if(t.count > 1){ t.count -= 1; t }
    // delete the contents of this node
    else if(t.left == null) t.right
    else if(t.right == null) t.left
    else{
      val (w, c, newR) = delMin(t.right)
      t.word = w; t.count = c; t.right = newR
      t
    }

  /** Delete the minimum node of t, returning the word and count
    * from that node, and the resulting tree.
    * Precondition: t!=null */
  private def delMin(t: Tree) : (String, Int, Tree) ={
    if(t.left==null) (t.word, t.count, t.right)
    else{
      val (w, c, newL) = delMin(t.left)
      t.left = newL; (w, c, t)
    }
  }

  private def total(t: Tree): Int = {
    var s = 0
    if (t!=null){
      s += t.count
      s += total(t.left)
      s += total(t.right)
    }
    s
  }

  def getT: Int = total(root)
  def stackT: Int = stackTotal(root)

  private def stackTotal(t: Tree): Int = {
    var stack = scala.collection.mutable.Stack[Tree]()
    var current = t
    var done = false
    var total = 0
    while(!done){
      while(current != null){
        total += current.count
        stack.push(current)
        current = current.left
      }
      if(!stack.isEmpty){
        current = stack.pop
        current = current.right
      }
      else done = true
  }
  total
}

private def depth(t: Tree): (Int, Int)={
  var min = 0; var max = 0
  if(t != null){
    val (min1, max1) = depth(t.left)
    val (min2, max2) = depth(t.right)
    if (min1<min2) min = min1 + 1 else min = min2 + 1
    if (max1>max2) max = max1 + 1 else max = max2 + 1
  }
  (min, max)
}

def depths: (Int, Int) = depth(root)

def tdepth(t: Tree): (Int, Int) = {
  var stack = scala.collection.mutable.Stack[(Tree, Int)]()
  var current = t
  var done = false
  var d = 0; var min = getT; var max = 0
  while(!done){
    while(current != null){
      stack.push((current, d))
      current = current.left
      d += 1
    }
    if(!stack.isEmpty){
      if (d < min) min = d
      if (d > max) max = d
      val (cs, ds) = stack.pop
      current = cs; d = ds + 1
      current = current.right
    }
    else {
      done = true
  }
}
(min, max)
}

def tdepths: (Int, Int) = tdepth(root)



}
