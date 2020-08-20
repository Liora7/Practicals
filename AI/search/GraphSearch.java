package search;

import java.util.HashSet;
import java.util.Set;

public class GraphSearch {
    private Frontier frontier;
    private int nodesGen;

    public GraphSearch(Frontier frontier){
        this.frontier = frontier;
        this.nodesGen = 0;
    }

    public Node findSolution(State initialConfiguration, GoalTest goalTest) {
        Set<State> explored = new HashSet();
        frontier.add(new Node(null, null, initialConfiguration)); nodesGen += 1;
        while (!frontier.isEmpty()) {
            Node node = frontier.getNode();
            explored.add(node.state);
            if (goalTest.isGoal(node.state))
                return node;
            else {
                for (Action action : node.state.getApplicableActions()) {
                    State newState = node.state.getActionResult(action);
                    Node newNode = new Node(node, action, newState);
                    if (!explored.contains(newNode.state)) {
                        frontier.add(newNode);
                        nodesGen += 1;
                    }
                }
            }
        }
        return null;
    }

    public int nodesGenerated(){
        return nodesGen;
    }
}
