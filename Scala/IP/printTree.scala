object printTree{
  private class Tree(var word: String, var left: Tree, var right: Tree)
}

class printTree{
   type Tree = printTree.Tree
   def Tree(word: String, left: Tree, right: Tree) =
    new printTree.Tree(word, left, right)

    var depth = 0
    def printT(t: Tree): Unit = {
      if(t!=null){
        println(" . " * depth + t.word)
        print(" . " * (depth + 1)); printT(t.left)
        print(" . " * (depth + 1)); printT(t.right)
      }
    }

  var tr = Tree("three", Tree("four", Tree("five",null,null), Tree("six",
  Tree("seven", Tree("one",null,null), null), null)), Tree("two",null,null))
  println(tr)
}
