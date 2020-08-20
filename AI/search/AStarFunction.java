package search;

public class AStarFunction implements NodeFunction {
    private NodeFunction h;

    public AStarFunction(NodeFunction heuristicFunction){
        this.h = heuristicFunction;
    }  // constructor to pass in heuristic

    public int eval(Node node){
        return node.g + h.eval(node);
    } // return cost of getting to node plus heuristic evaluated at the node
}
