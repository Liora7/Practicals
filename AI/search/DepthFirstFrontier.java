package search;

import java.util.Stack;

public class DepthFirstFrontier implements Frontier {

    private Stack<Node> stack = new Stack();
    private int maxNs = 0;
    private int numNs = 0;


    public void add(Node node){
        stack.push(node);
        numNs += 1;
        if (numNs > maxNs) maxNs = numNs;
    }

    public void clear(){
        stack.clear();
        numNs = 0;
        maxNs = 0;
    }

    public boolean isEmpty(){
        return stack.isEmpty();
    }

    public Node getNode(){
        numNs -= 1;
        return stack.pop();
    }

    public int maxNodes(){
        return this.maxNs;
    }
}
