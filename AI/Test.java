import npuzzle.MisplacedTilesHeuristicFunction;
import npuzzle.NPuzzlePrinting;
import npuzzle.Tiles;
import npuzzle.TilesGoalTest;
import search.*;
import tour.*;

public class Test {

    public static void main(String[] args) {
        Frontier[] frontiers = {new BreadthFirstFrontier(), new DepthFirstFrontier()};

        TourIDS(); NPuzzleIDS();

        // IDS on the tour puzzle generates 41636 nodes with max 22 at a time
        // IDS on the N puzzle generates 1211854 nodes with max 25 at a time

        for ( Frontier f : frontiers ) {
            TourGraphSearch(f);
            TourTreeSearch(f);
            NPuzzleGraphSearch(f);
            NPuzzleTreeSearch(f);

            // breadth-first searches;
            // The tour puzzle generates 2083 nodes in total, with max 451 at a time in graph search
            //                       and  75972 nodes in total, with max 44217 at a time in tree search
            // the N puzzle generates 5019 nodes in total, with max 1926 at a time in graph search
            //                    and 2298273 nodes in total, with max 1473871 at a time in tree search
            // depth-first searches;
            // the tour puzzle generates 69 nodes total, with max 26 in the frontier at a time with graph search
            // depth-first tree search isn't complete and so runs out of memory
        }
    }

    private static void NPuzzleTreeSearch(Frontier f){
        Class fClass = f.getClass();
        String type = (fClass.isInstance(new BreadthFirstFrontier()))? "breadth-first" : "depth-first";
        System.out.println("This is " + type + " tree search on 8-puzzle");
        System.out.println();

        Tiles initialConfiguration = new Tiles(new int[][] {
                { 7, 4, 2 },
                { 8, 1, 3 },
                { 5, 0, 6 }
        });

        GoalTest goalTest = new TilesGoalTest();
        TreeSearch tree = new TreeSearch(f);
        Node solution = tree.findSolution(initialConfiguration, goalTest);
        System.out.println("Total nodes generated: " + tree.nodesGenerated());
        System.out.println("Max nodes stored in frontier: " + f.maxNodes());
        f.clear();
        new NPuzzlePrinting().printSolution(solution);
    }

    private static void NPuzzleGraphSearch(Frontier f){
        Class fClass = f.getClass();
        String type = (fClass.isInstance(new BreadthFirstFrontier()))? "breadth-first" : "depth-first";
        System.out.println("This is " + type + " graph search on 8-puzzle");
        System.out.println();

        Tiles initialConfiguration = new Tiles(new int[][] {
                { 7, 4, 2 },
                { 8, 1, 3 },
                { 5, 0, 6 }
        });

        GoalTest goalTest = new TilesGoalTest();
        GraphSearch graph = new GraphSearch(f);
        Node solution = graph.findSolution(initialConfiguration, goalTest);
        System.out.println("Total nodes generated: " + graph.nodesGenerated());
        System.out.println("Max nodes stored in frontier: " + f.maxNodes());
        f.clear();
        new NPuzzlePrinting().printSolution(solution);
    }

    private static void TourTreeSearch(Frontier f){
        Class fClass = f.getClass();
        String type = (fClass.isInstance(new BreadthFirstFrontier()))? "breadth-first" : "depth-first";
        System.out.println("This is " + type + " tree search on Romania tour");
        System.out.println();

        Cities romania = SetUpRomania.getRomaniaMapSmall();
        City startCity = romania.getState("Bucharest");

        GoalTest goalTest = new TourGoalTest(romania.getAllCities(), startCity);
        TreeSearch tree = new TreeSearch(f);
        Node solution = tree.findSolution(new TourState(startCity), goalTest);
        System.out.println("Total nodes generated: " + tree.nodesGenerated());
        System.out.println("Max nodes stored in frontier: " + f.maxNodes());
        f.clear();
        new TourPrinting().printSolution(solution);
    }

    private static void TourGraphSearch(Frontier f){
        Class fClass = f.getClass();
        String type = (fClass.isInstance(new BreadthFirstFrontier()))? "breadth-first" : "depth-first";
        System.out.println("This is " + type + " graph search on Romania tour");
        System.out.println();

        Cities romania = SetUpRomania.getRomaniaMapSmall();
        City startCity = romania.getState("Bucharest");

        GoalTest goalTest = new TourGoalTest(romania.getAllCities(), startCity);
        GraphSearch graph = new GraphSearch(f);
        Node solution = graph.findSolution(new TourState(startCity), goalTest);
        System.out.println("Total nodes generated: " + graph.nodesGenerated());
        System.out.println("Max nodes stored in frontier: " + f.maxNodes());
        f.clear();
        new TourPrinting().printSolution(solution);
    }

    private static void NPuzzleIDS(){
        System.out.println("This is iterative deepening search on 8-puzzle");
        System.out.println();

        Tiles initialConfiguration = new Tiles(new int[][] {
                { 7, 4, 2 },
                { 8, 1, 3 },
                { 5, 0, 6 }
        });

        GoalTest goalTest = new TilesGoalTest();
        IterativeDeepeningTreeSearch tree = new IterativeDeepeningTreeSearch();
        Node solution = tree.findSolution(initialConfiguration, goalTest);
        System.out.println("Total nodes generated: " + tree.nodesGenerated());
        new NPuzzlePrinting().printSolution(solution);
    }

    private static void TourIDS(){
        System.out.println("This is iterative deepening search on Romania tour");
        System.out.println();

        Cities romania = SetUpRomania.getRomaniaMapSmall();
        City startCity = romania.getState("Bucharest");

        GoalTest goalTest = new TourGoalTest(romania.getAllCities(), startCity);
        IterativeDeepeningTreeSearch tree = new IterativeDeepeningTreeSearch();
        Node solution = tree.findSolution(new TourState(startCity), goalTest);
        System.out.println("Total nodes generated: " + tree.nodesGenerated());
        new TourPrinting().printSolution(solution);
    }


}
