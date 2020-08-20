package search;

public class AStarTreeSearch implements Search {
    private Frontier frontier;
    private int nodesGen;

    public AStarTreeSearch(AStarFunction f){
        this.frontier = new BestFirstFrontier(f);
        this.nodesGen = 0;
    } // pass in heuristic in constructor

    public Node findSolution(State initialConfiguration, GoalTest goalTest) {
        frontier.add(new Node(0,null, null, initialConfiguration));
        while (!frontier.isEmpty()) {
            Node node = frontier.getNode();
            int pCost = node.g; // cost of getting to parent node
            if (goalTest.isGoal(node.state)) {
                System.out.println("Max nodes stored in frontier: " + frontier.maxNodes());
                return node;
            }
            else {
                for (Action action : node.state.getApplicableActions()) {
                    State newState = node.state.getActionResult(action);
                    frontier.add(new Node(pCost + action.cost(), node, action, newState));
                    // pass in cost of getting to this new node (cost of getting to parent + cost of action)
                    nodesGen += 1;
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
