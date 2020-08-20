package tour;

import search.Node;
import search.NodeFunction;


public class TourHeuristic implements NodeFunction {

    private Cities cities; // set of cities in the map
    private City goal; // goal city

    public int eval(Node n) {
        TourState tour = (TourState) n.state;
        City furthestC = tour.currentCity; // city we're currently at
        int furthestDist = 0; // shortest distance to furthest unvisited city
        cities.computeShortestDistances(); // compute shortest distances for all cities
        for (City c : cities.getAllCities()){ //iterate over each city
            if (!tour.visitedCities.contains(c) && c.getShortestDistanceTo(tour.currentCity) > furthestDist){
                furthestDist = c.getShortestDistanceTo(tour.currentCity);
                furthestC = c;
                // if we haven't visited the city yet and it is further than the furthest city we've seen so far, update variables
            }
        }
        return furthestDist + furthestC.getShortestDistanceTo(goal); // return distance to furthest city + distance from there to goal
    }

    public TourHeuristic(Cities cities, City goalCity){
        this.cities = cities;
        this.goal = goalCity;
    } // pass in cities and goal in constructor
}
