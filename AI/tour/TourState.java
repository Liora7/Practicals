package tour;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

import search.Action;
import search.State;

public class TourState implements State {
	protected final Set<City> visitedCities;
	protected final City currentCity;
	
	public TourState(City startCity) {
		this.visitedCities = Collections.emptySet();
		this.currentCity = startCity;
	}
	public TourState(Set<City> visitedCities, City currentCity) {
		this.visitedCities = visitedCities;
		this.currentCity = currentCity;
	}
	public Set<Road> getApplicableActions() {
		return currentCity.outgoingRoads;
	}
	public State getActionResult(Action action) {
		Road road = (Road)action;
		Set<City> newVisitedCities = new LinkedHashSet<City>(visitedCities);
		newVisitedCities.add(road.targetCity);
		return new TourState(newVisitedCities, road.targetCity);
	}
	public boolean equals(Object that){
		if (this == that) return true;
		if (that == null) return false;
		if (this.getClass() != that.getClass()) return false;
		TourState tour = (TourState) that;
		if (this.visitedCities.size() != tour.visitedCities.size()) return false;
		if (this.visitedCities.containsAll(tour.visitedCities) && this.currentCity == tour.currentCity) return true;
		return false;
	}
	public int hashCode(){
		final int prime = 31;
		int result = 1;
		result = this.currentCity.getName().hashCode();
		for (Road r : this.getApplicableActions()){
			result = result * prime + r.cost();
		}
		return result;
	}
}
