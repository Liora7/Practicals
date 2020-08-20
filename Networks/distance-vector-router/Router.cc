#include <stdio.h>
#include <omnetpp.h>

#include "Router.h"

Define_Module(Router);

using namespace std;


// functions from the cModule interface

void Router::initialize() {
  if (static_cast<double>(par("smallInfinity")) > 0.0){ // if small infinity param set, set INFINITE_ROUTE_COST to the param via the private var infinity
    infinity = par("smallInfinity");
  }
  // initialise a zero-route for this router to itself
  Route selfRoute;
  selfRoute.dest = getName();
  selfRoute.nextHop = getName();
  selfRoute.cost = 0.0;
  routes[getName()] = selfRoute;

  // setup the internal activity messages
  periodic_update_activity =
    new cMessage("Periodic update to neighbours.");
  scheduleAt(simTime() + par("periodicUpdateInterval"), periodic_update_activity);

  message_sending_activity =
    new cMessage("Periodic sending of message to other router.");
  string messageTo = par("messageTo");
  if (messageTo != "") {
    scheduleAt(simTime() + par("messageSendingInterval"), message_sending_activity);
  }

  check_alive_activity =
    new cMessage("Periodic checking of neighbours being alive.");
  scheduleAt(simTime() + par("checkAliveInterval"), check_alive_activity);

}

void Router::finish() {
  printRoutingTable();

  // delete internal activity messages
  cancelEvent(periodic_update_activity);
  delete periodic_update_activity;
  cancelEvent(message_sending_activity);
  delete(message_sending_activity);
  cancelEvent(check_alive_activity);
  delete(check_alive_activity);
}

void Router::handleMessage(cMessage *msg) {
  if (simTime() > par("diesAt")) {
    // if router has died, do nothing.
    if(msg != periodic_update_activity &&
       msg != message_sending_activity &&
       msg != check_alive_activity) {
      // don't delete the above messages because we reuse them.
      // do delete messages coming from other routers.
      delete msg;
    }
    return;
  } else if (msg == check_alive_activity) {
    checkAliveActivity();
    scheduleAt(simTime() + par("checkAliveInterval"),
               check_alive_activity);
  } else if (msg == periodic_update_activity) {
    sendRoutingUpdatesActivity();
    scheduleAt(simTime()+par("periodicUpdateInterval"), periodic_update_activity);

  } else if (msg == message_sending_activity) {
    sendMessageToOtherRouterActivity(par("messageTo"));
    scheduleAt(simTime() + par("messageSendingInterval"), message_sending_activity);

  } else {
    // non-internal messages
    if (msg->getKind() == UPDATE_PACKET) {
      updateRoutesActivity((UpdatePacket*) msg);
      lastSeen[msg->getSenderModule()->getName()] = msg->getSendingTime();
    } else if (msg->getKind() == MESSAGE_PACKET) {
      MessagePacket* p = (MessagePacket*) msg;
      EV << '@' << getName()
         << ": Message " << p->getId()
         << " arrived from " << p->getSource()
         << ": [" << p->getData() << ']' << endl;
    }
    delete msg;
  }
}


// utility functions

void Router::printRoutingTable() {
  EV << '@' << getName() << ": Routing table:" << endl;
  for (map<string, Route>::iterator r = routes.begin(); r != routes.end(); ++r) {
    EV << "--" << r->second.nextHop << "--> " << r->second.dest
       << ": " <<  r->second.cost << endl;
  }
  EV << endl;
}

void Router::sendCost(string dest) {
  // TODO: Addapt this function for "useSplitHorizon" and "usePoisonReverse"

  if (static_cast<bool>(par("useSplitHorizon"))){ // split horizon
    if (routes.find(dest) != routes.end()) {
      Route r = routes.find(dest)->second;
      // iterate over all output gates of this router
      for (int i=0; i < gateSize("out"); i++) {
        cGate *g = gate("out", i);
        if (r.nextHop != g->getNextGate()->getOwnerModule()->getName()){ // if the gate doesn't lead to the next hop, send the route

          UpdatePacket* p = new UpdatePacket("", UPDATE_PACKET);
          p->setSource(getName());
          p->setDestination(dest.c_str());
          p->setCost(r.cost);
          send(p, g);
        }
      }
    } else {
      EV << "Error: Request to send cost to unknown router." << endl;
    }
    // split horizon works, eliminatin count to infinity and doing 517 events
  }
  else if (static_cast<bool>(par("usePoisonReverse"))){
    if (routes.find(dest) != routes.end()) {
      Route r = routes.find(dest)->second;
      cGate *nextHopGate = findGateForNeighbour(r.nextHop);
      // iterate over all output gates of this router
      for (int i=0; i < gateSize("out"); i++) {
        cGate *g = gate("out", i);
        if (r.nextHop == g->getNextGate()->getOwnerModule()->getName()){ // if the gate leads to the next hop, send infinite cost route

          UpdatePacket* p = new UpdatePacket("", UPDATE_PACKET);
          p->setSource(getName());
          p->setDestination(dest.c_str());
          p->setCost(INFINITE_ROUTE_COST);
          send(p, g);
        }
        else{ // for all other routers, send correct cost
          UpdatePacket* p = new UpdatePacket("", UPDATE_PACKET);
          p->setSource(getName());
          p->setDestination(dest.c_str());
          p->setCost(r.cost);
          send(p, g);
        }
      }
    } else {
      EV << "Error: Request to send cost to unknown router." << endl;
    }
    // poison reverse works, eliminating count-to-infinity and doing 741 events.
  }
  else{ // no strategy used, always send normal cost route
    if (routes.find(dest) != routes.end()) {
      Route r = routes.find(dest)->second;

      // iterate over all output gates of this router
      for (int i=0; i < gateSize("out"); i++) {
        cGate *g = gate("out", i);

        UpdatePacket* p = new UpdatePacket("", UPDATE_PACKET);
        p->setSource(getName());
        p->setDestination(dest.c_str());
        p->setCost(r.cost);
        send(p, g);
      }
    } else {
      EV << "Error: Request to send cost to unknown router." << endl;
    }
  }
  // with no strategy and regardless of whether we use small infinity or not, 15939 events are communicated and we have count-to-infinity.

}

cGate* Router::findGateForNeighbour(string n) {
  for (int i=0; i < gateSize("out"); i++) {
    cGate *g = gate("out", i);
    if (g->getNextGate()->getOwnerModule()->getName() == n) {
      return g;
    }
  }

  EV << "@" << getName() << ": NO_GATE. Cannot find out gate for router "
     << n << endl;
  return NULL;
}


// activity functions

void Router::sendRoutingUpdatesActivity() {
  // advertise myself to all neighbours
  for (map<string, Route>::iterator r = routes.begin(); r != routes.end(); ++r){
    sendCost(r->second.dest);
  }
  // TODO: send the whole routing table instead
  // Iterate through list of routes and send each one
}

void Router::checkAliveActivity() {
  for (map<string, simtime_t>::iterator ls = lastSeen.begin();
       ls != lastSeen.end(); ) {
    if(simTime() - ls->second > par("peerFailedTimeOut")) {
      EV << "@" << getName() << ": neighbour " << ls->first << " failed" << endl;

      for (map<string, Route>::iterator r = routes.begin(); r != routes.end(); ++r){  // for each route, if the next hop is the failed router, set the route cost to infinite and update neighbours with the new cost
        if (r->second.nextHop == ls->first){
          r->second.cost = INFINITE_ROUTE_COST;
          routes[r->second.dest] = r->second; //update route in routing table
          sendCost(r->second.dest);
        }
      }
      // Neighbour has not been seen recently and is considered failed
      // TODO: Update the routing table accordingly and inform the neighbours

      // remove the entry in the mapping, so the failure is not reported again
      lastSeen.erase(ls++);
    } else {
      ls++;
    }
  }
}
// with the initial max INFINITE_ROUTE_COST this results in count-to-infinity in the RouterAFailsAt10s; all routers other than A have costs larger than 200 for routes to A, and do not use A as a next hop in any route. They also keep advertising routes to A with higher and higher costs.

void Router::updateRoutesActivity(UpdatePacket* p) {
  EV << "@" << getName() <<": Received update from " << p->getSource()
     << ". route " << p->getSource() << " --> " << p->getDestination()
     << ", cost " << p->getCost() << endl;

  if (routes.find(p->getDestination()) == routes.end()) { // new destination (not in routes map)
    Route newRoute;                       // create new route and set fields
    newRoute.dest = p->getDestination();
    newRoute.nextHop = p->getSource();
    newRoute.cost = p->getCost() + 1;
    routes[p->getDestination()] = newRoute;
    sendCost(p->getDestination());      //send new route to neighbours
  }
  else{   // existing route
    Route r = routes.find(p->getDestination())->second;   // find version in routing table
    if (r.cost > p->getCost() + 1){  // known route improved; update route
      r.nextHop = p->getSource();
      r.cost = p->getCost() + 1;
      routes[p->getDestination()] = r;  // replace suboptimal route in table
      sendCost(p->getDestination());  // send updated route to neighbours
    }
    else if (r.cost < p->getCost() + 1 && r.nextHop == p->getSource()){  // cost of route increased; update route
      if (p->getCost() == INFINITE_ROUTE_COST){ // route became infinite cost; update
        r.cost = INFINITE_ROUTE_COST;
      }
      else{
        r.cost = p->getCost() + 1;
      }
      routes[p->getDestination()] = r;  // replace route in table
      sendCost(p->getDestination());  // send updated route to neighbors
    }

  }
  /* Routing Tables for AllRoutersAlive: (optimal - similarly for Ring5 and Ring5_4)
INFO (Router)A_Ring4.A:@A: Routing table:
INFO (Router)A_Ring4.A:--A--> A: 0
INFO (Router)A_Ring4.A:--B--> B: 1
INFO (Router)A_Ring4.A:--B--> C: 2
INFO (Router)A_Ring4.A:--B--> D: 2
INFO (Router)A_Ring4.A:--B--> E: 3
INFO (Router)A_Ring4.A:
INFO (Router)A_Ring4.B:@B: Routing table:
INFO (Router)A_Ring4.B:--A--> A: 1
INFO (Router)A_Ring4.B:--B--> B: 0
INFO (Router)A_Ring4.B:--C--> C: 1
INFO (Router)A_Ring4.B:--D--> D: 1
INFO (Router)A_Ring4.B:--C--> E: 2
INFO (Router)A_Ring4.B:
INFO (Router)A_Ring4.C:@C: Routing table:
INFO (Router)A_Ring4.C:--B--> A: 2
INFO (Router)A_Ring4.C:--B--> B: 1
INFO (Router)A_Ring4.C:--C--> C: 0
INFO (Router)A_Ring4.C:--E--> D: 2
INFO (Router)A_Ring4.C:--E--> E: 1
INFO (Router)A_Ring4.C:
INFO (Router)A_Ring4.D:@D: Routing table:
INFO (Router)A_Ring4.D:--B--> A: 2
INFO (Router)A_Ring4.D:--B--> B: 1
INFO (Router)A_Ring4.D:--E--> C: 2
INFO (Router)A_Ring4.D:--D--> D: 0
INFO (Router)A_Ring4.D:--E--> E: 1
INFO (Router)A_Ring4.D:
INFO (Router)A_Ring4.E:@E: Routing table:
INFO (Router)A_Ring4.E:--C--> A: 3
INFO (Router)A_Ring4.E:--C--> B: 2
INFO (Router)A_Ring4.E:--C--> C: 1
INFO (Router)A_Ring4.E:--D--> D: 1
INFO (Router)A_Ring4.E:--E--> E: 0
INFO (Router)A_Ring4.E:
  */
  // TODO: update routing table and advertise changes to neighbours
  //       if received update packet makes this neccessary

}

void Router::sendMessageToOtherRouterActivity(string dest) {
  MessagePacket* p = new MessagePacket("", MESSAGE_PACKET);
  p->setSource(getName());
  p->setDestination(dest.c_str());
  char data[100];
  snprintf(data, 100, "This is a message from %s to %s.", getName(), dest.c_str());
  p->setData(data);

  // send off the message
  if (routes.find(dest) != routes.end()) {
    send(p, findGateForNeighbour(routes.find(dest)->second.nextHop));
  }
}


/*
In this practical, I got all the compulsory parts working. I had some issues with C++ along the way, since I'd never used it, and hence had some errors with e.g. assuming structs were mutable, when really, C++ creates a copy. Such bugs were annoying but quickly taken care of.
To test my implementation, I ran the simulations in the GUI and checked both emssages sent as well as the final routing table, making sure (by hand) that the routes were correct.
The biggest difficulty was the C++ syntax; especially setting the value of the INFINITE_ROUTE_COST parameter via a private variable was slightly tricky, and I lost a lot of time due to not casting the usePoisonReverse and useSplitHorizon parameters to bools first.
I found that small infinity does not help with count-to-infinity, whereas both split horizon and poison reverse do.
I made sure to test with several networks, checking the messages for convergence and the final routing tables.

*/
