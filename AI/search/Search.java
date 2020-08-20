package search;

public interface Search {
    Node findSolution(State initialConfiguration, GoalTest goalTest);
    int nodesGenerated();
}
