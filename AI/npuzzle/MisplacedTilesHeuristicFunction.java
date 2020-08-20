package npuzzle;

import search.Node;
import search.NodeFunction;

public class MisplacedTilesHeuristicFunction implements NodeFunction {

    public int eval(Node node){
        int misplaced = 0; // count number of misplaced tiles
        Tiles tiles = (Tiles) node.state;
        int w = tiles.getWidth();
        int emptyRow = w - 1;
        int emptyCol = w - 1;
        for (int row = 0; row < w; row++) {
            for (int column = 0; column < w; column++) { // for each tile on the puzzle, if it isn't in the right spot, inc counter
                int tile = tiles.getTile(row, column);
                if (tile != (row * w + column + 1) && tile != 0) misplaced += 1; // for all tiles but the empty tile
                else if (tile == 0 && row != emptyRow && column != emptyCol) misplaced += 1; // check empty tile is in the right spot
            }
        }
        return misplaced;
    }

}
