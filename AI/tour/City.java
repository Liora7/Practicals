package tour;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class City {
	protected final String name;
	protected final Set<Road> outgoingRoads;
	protected final Map<City, Integer> shortestDistanceByCity;

	public City(String name) {
		this.name = name;
		this.outgoingRoads = new LinkedHashSet<>();
		this.shortestDistanceByCity = new LinkedHashMap<>();
	}

	public String getName() {
		return name;
	}

	public Set<Road> getOutgoingRoads() {
		return outgoingRoads;
	}

	public int getShortestDistanceTo(City city) {
		Integer distance = shortestDistanceByCity.get(city);
		if (distance == null)
			return Integer.MAX_VALUE;
		else
			return distance.intValue();
	}

	public boolean equals(Object that) {
		if (this == that) return true;
		if (that == null) return false;
		if (this.getClass() != that.getClass()) return false;
		City city = (City) that;
		if (city.getName() == this.getName()) return true;
		return false;
	}

}