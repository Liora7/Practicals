package search;

public class Node implements Comparable<Node> {
	public final Node parent;
	public final Action action;
	public final State state;
	public int depth;
	public int value;
	public int g;

	public Node(Node parent, Action action, State state) {
		this.parent = parent;
		this.action = action;
		this.state = state;
	}
	
	public Node(Node parent, Action action, State state, int depth) { // method overloading to use depth in IDS only
		this.parent = parent;
		this.action = action;
		this.state = state;
		this.depth = depth;
	}

	public Node(int g, Node parent, Action action, State state) { // method overloading to use g(n)
		this.parent = parent;
		this.action = action;
		this.state = state;
		this.g = g;
	}

	public void setValue(int value){
		this.value = value;
	} // set value of node (used by frontier)

	public int compareTo(Node other) {
		return this.value - other.value;
	} // comparator for nodes - compare nodes by their values to sort them in the priority queue according to these

}
