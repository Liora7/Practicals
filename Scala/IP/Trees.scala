// Using a binary tree to represent a bag of words

// Companion object defines the type of trees
object Trees{
   class Tree(var word: String,
                     var left: Tree, var right: Tree)
}

class Trees{
   type Tree = Trees.Tree
   def Tree(word: String, left: Tree, right: Tree) =
    new Trees.Tree(word, left, right)

    def printT(t: Tree, depth: Int): Unit = {
      if(t!=null){
        println(" . " * depth + t.word)
        printT(t.left, depth + 1)
        printT(t.right, depth + 1)
      }
      else println(" . " * depth + "null")
    }


    def stackPrint(t: Tree): Unit = {
      var stack = scala.collection.mutable.Stack[(Tree, Int)]()
      var current = t
      var done = false
      var d = 0; var dcurr = 0;
      while(!done){
        d = dcurr
        while(current != null){
          stack.push((current, d))
          println(" . " * d + current.word)
          current = current.left
          d += 1
        }
        if(!stack.isEmpty){
          val (cs, ds) = stack.pop
          current = cs; dcurr = ds + 1
          println(" . " * d + "null")
          current = current.right
        }
        else {
          println(" . " * d + "null")
          done = true
      }
    }
  }

  def flip(t: Tree) : Unit = {
    if(t!=null){
      val temp = t.left
      t.left = t.right
      t.right = temp
      flip(t.left)
      flip(t.right)
    }
  }


    var tr = Tree("three", Tree("four", Tree("five",null,null), Tree("six",
    Tree("seven", Tree("one",null,null), null), null)), Tree("two",null,null))
}
