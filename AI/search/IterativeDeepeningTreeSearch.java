package search;

public class IterativeDeepeningTreeSearch implements Search{

    private int nodesGen = 0;

    public Node findSolution(State initialConfiguration, GoalTest goalTest) {
        int maxNodes = 0;
        for (int depth=0; depth < Integer.MAX_VALUE; depth++){
            int d = 0;
            Frontier frontier = new DepthFirstFrontier();
            frontier.add(new Node(null, null, initialConfiguration)); nodesGen += 1;
            while (!frontier.isEmpty()) {
                Node node = frontier.getNode();
                if (goalTest.isGoal(node.state)){
                    if (frontier.maxNodes() > maxNodes) maxNodes = frontier.maxNodes();
                    System.out.println("Max nodes stored in frontier: " + maxNodes);
                    return node;
                }
                else {
                    d = node.depth;
                    if (d + 1 <= depth) {
                        for (Action action : node.state.getApplicableActions()) {
                            State newState = node.state.getActionResult(action);
                            nodesGen += 1;
                            frontier.add(new Node(node, action, newState, d + 1));
                        }
                    }
                }
            }
            if (frontier.maxNodes() > maxNodes) maxNodes = frontier.maxNodes();
        }
        System.out.println("Max nodes stored in frontier: " + maxNodes);
        return null;
    }

    public int nodesGenerated() {
        return this.nodesGen;
    }
}
