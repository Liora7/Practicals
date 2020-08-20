package npuzzle;

import search.*;

public class BFTS {
    public static void main(String[] args) {
        System.out.println("This is breadth-first tree search on 8-puzzle");
        System.out.println();

        Tiles initialConfiguration = new Tiles(new int[][] {
                { 7, 4, 2 },
                { 8, 1, 3 },
                { 5, 0, 6 }
        });

        GoalTest goalTest = new TilesGoalTest();
        Frontier frontier = new BreadthFirstFrontier();
        TreeSearch tree = new TreeSearch(frontier);
        Node solution = tree.findSolution(initialConfiguration, goalTest);
        System.out.println("Total nodes generated: " + tree.nodesGenerated());
        System.out.println("Max nodes stored in frontier: " + frontier.maxNodes());
        new NPuzzlePrinting().printSolution(solution);
    }
}
