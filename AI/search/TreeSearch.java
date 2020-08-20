package search;

public class TreeSearch implements Search {

    private Frontier frontier;
    private int nodesGen;

    public TreeSearch(Frontier frontier){
        this.frontier = frontier;
        this.nodesGen = 0;
    }

    public Node findSolution(State initialConfiguration, GoalTest goalTest) {
        frontier.add(new Node(null, null, initialConfiguration));
        while (!frontier.isEmpty()) {
            Node node = frontier.getNode();
            if (goalTest.isGoal(node.state))
                return node;
            else {
                for (Action action : node.state.getApplicableActions()) {
                    State newState = node.state.getActionResult(action);
                    frontier.add(new Node(node, action, newState));
                    nodesGen += 1;
                }
            }
        }
        return null;
    }

    public int nodesGenerated(){
        return nodesGen;
    }
}
