import npuzzle.MisplacedTilesHeuristicFunction;
import npuzzle.NPuzzlePrinting;
import npuzzle.Tiles;
import npuzzle.TilesGoalTest;
import search.*;
import tour.*;

public class Test2 {

    public static void main(String[] args) {
        NPuzzleAStarGraphSearch(); NPuzzleAStarTreeSearch();
        // A* graph search on N puzzle generates 229 nodes with max 94 in the frontier at a time
        // A* tree search on N puzzle generates 963 nodes with max 613 in the frontier at a time
        // tree search obviously visits some nodes several times and also stores more nodes in the frontier at a given point
        // however both are much more efficient than the uninformed versions

        TourAStarGraphSearch(); TourAStarTreeSearch();
        // A* graph search on the Tour puzzle generates 224 nodes with max 116 in the frontier at a time
        // A* tree search on the Tour puzzle generates 452 nodes with max 291 in the frontier at a time
        // again, both A* searches are a lot more efficient than the uninformed versions
    }


    private static void NPuzzleAStarTreeSearch(){
        System.out.println("This is A* tree search on 8-puzzle");
        System.out.println();

        Tiles initialConfiguration = new Tiles(new int[][] {
                { 7, 4, 2 },
                { 8, 1, 3 },
                { 5, 0, 6 }
        });

        GoalTest goalTest = new TilesGoalTest();
        MisplacedTilesHeuristicFunction h = new MisplacedTilesHeuristicFunction();
        AStarFunction f = new AStarFunction(h);
        AStarTreeSearch tree = new AStarTreeSearch(f);
        Node solution = tree.findSolution(initialConfiguration, goalTest);
        System.out.println("Total nodes generated: " + tree.nodesGenerated());
        new NPuzzlePrinting().printSolution(solution);
    }

    private static void NPuzzleAStarGraphSearch(){
        System.out.println("This is A* graph search on 8-puzzle");
        System.out.println();

        Tiles initialConfiguration = new Tiles(new int[][] {
                { 7, 4, 2 },
                { 8, 1, 3 },
                { 5, 0, 6 }
        });

        GoalTest goalTest = new TilesGoalTest();
        MisplacedTilesHeuristicFunction h = new MisplacedTilesHeuristicFunction();
        AStarFunction f = new AStarFunction(h);
        AStarGraphSearch graph = new AStarGraphSearch(f);
        Node solution = graph.findSolution(initialConfiguration, goalTest);
        System.out.println("Total nodes generated: " + graph.nodesGenerated());
        new NPuzzlePrinting().printSolution(solution);
    }


    private static void TourAStarTreeSearch(){
        System.out.println("This is A* tree search on Romania tour");
        System.out.println();

        Cities romania = SetUpRomania.getRomaniaMapSmall();
        City startCity = romania.getState("Bucharest");

        GoalTest goalTest = new TourGoalTest(romania.getAllCities(), startCity);
        TourHeuristic h = new TourHeuristic(romania, startCity);
        AStarFunction f = new AStarFunction(h);
        AStarTreeSearch tree = new AStarTreeSearch(f);
        Node solution = tree.findSolution(new TourState(startCity), goalTest);
        System.out.println("Total nodes generated: " + tree.nodesGenerated());
        new TourPrinting().printSolution(solution);
    }

    private static void TourAStarGraphSearch(){
        System.out.println("This is A* graph search on Romania tour");
        System.out.println();

        Cities romania = SetUpRomania.getRomaniaMapSmall();
        City startCity = romania.getState("Bucharest");

        GoalTest goalTest = new TourGoalTest(romania.getAllCities(), startCity);
        TourHeuristic h = new TourHeuristic(romania, startCity);
        AStarFunction f = new AStarFunction(h);
        AStarGraphSearch graph = new AStarGraphSearch(f);
        Node solution = graph.findSolution(new TourState(startCity), goalTest);
        System.out.println("Total nodes generated: " + graph.nodesGenerated());
        new TourPrinting().printSolution(solution);
    }

}
