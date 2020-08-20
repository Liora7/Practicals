package search;

import java.util.HashSet;
import java.util.Set;

public class AStarGraphSearch {
    private Frontier frontier;
    private int nodesGen;

    public AStarGraphSearch(AStarFunction f){
        this.frontier = new BestFirstFrontier(f);
        this.nodesGen = 0;
    } // pass in f in constructor


    public Node findSolution(State initialConfiguration, GoalTest goalTest) {
        Set<State> explored = new HashSet();
        frontier.add(new Node(0, null, null, initialConfiguration)); nodesGen += 1;
        while (!frontier.isEmpty()) {
            Node node = frontier.getNode();
            explored.add(node.state);
            int pCost = node.g; // cost of getting to parent node
            if (goalTest.isGoal(node.state)) {
                System.out.println("Max nodes stored in frontier: " + frontier.maxNodes());
                return node;
            }
            else {
                for (Action action : node.state.getApplicableActions()) {
                    State newState = node.state.getActionResult(action);
                    Node newNode = new Node(pCost + action.cost(), node, action, newState);
                    // pass in cost of getting to this new node (cost of getting to parent + cost of action)
                    if (!explored.contains(newNode.state)) {
                        frontier.add(newNode);
                        nodesGen += 1;
                    }
                }
            }
        }
        System.out.println("Max nodes stored in frontier: " + frontier.maxNodes());
        return null;
    }

    public int nodesGenerated(){
        return nodesGen;
    }
}