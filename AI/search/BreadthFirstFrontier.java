package search;

import java.util.ArrayDeque;
import java.util.Queue;

public class BreadthFirstFrontier implements Frontier {
    private Queue<Node> queue = new ArrayDeque<>();
    private int maxNs = 0;
    private int numNs = 0;


    public void add(Node node){
        queue.add(node);
        numNs += 1;
        if (numNs > maxNs) maxNs = numNs;
    }

    public void clear(){
        queue.clear();
        numNs = 0;
        maxNs = 0;
    }

    public boolean isEmpty(){
        return queue.isEmpty();
    }

    public Node getNode(){
        numNs -= 1;
        return queue.remove();
    }

    public int maxNodes(){
        return this.maxNs;
    }
}
