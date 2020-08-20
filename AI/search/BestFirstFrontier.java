package search;

import java.util.PriorityQueue;
import java.util.Queue;

public class BestFirstFrontier implements Frontier {
    private Queue<Node> queue = new PriorityQueue<>(); // priority queue, sorted by comparator written for the nodes
    private int maxNs = 0;
    private int numNs = 0;
    private NodeFunction f;

    public BestFirstFrontier(NodeFunction f){
        this.f = f;
    } // set function in constructor


    public void add(Node node){
        int nValue = f.eval(node); // when adding a node to the frontier, calculate and set its value with f
        node.setValue(nValue);
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
        return queue.poll(); // get min valued node from queue
    }

    public int maxNodes(){
        return this.maxNs;
    }
}
