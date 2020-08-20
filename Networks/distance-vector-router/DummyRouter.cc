#include <stdio.h>
#include <omnetpp.h>

#include "DummyRouter.h"

Define_Module(DummyRouter);

using namespace std;


// functions from the cModule interface

void DummyRouter::initialize() {
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

void DummyRouter::finish() {
  printRoutingTable();

  // delete internal activity messages
  cancelEvent(periodic_update_activity);
  delete periodic_update_activity;
  cancelEvent(message_sending_activity);
  delete(message_sending_activity);
  cancelEvent(check_alive_activity);
  delete(check_alive_activity);
}

void DummyRouter::handleMessage(cMessage *msg) {
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

void DummyRouter::printRoutingTable() {
  EV << '@' << getName() << ": Routing table:" << endl;
  for (map<string, Route>::iterator r = routes.begin(); r != routes.end(); ++r) {
    EV << "--" << r->second.nextHop << "--> " << r->second.dest
       << ": " <<  r->second.cost << endl;
  }
  EV << endl;
}

void DummyRouter::sendCost(string dest) {
  // iterate over all output gates of this router
  for (int i=0; i < gateSize("out"); i++) {
    cGate *g = gate("out", i);

    if (routes.find(dest) != routes.end()) {
      Route r = routes.find(dest)->second;

      UpdatePacket* p = new UpdatePacket("", UPDATE_PACKET);
      p->setSource(getName());
      p->setDestination(dest.c_str());
      p->setCost(r.cost);
      send(p, g);

    } else {
      EV << "Error: Request to send cost to unknown router." << endl;
    }
  }
}

cGate* DummyRouter::findGateForNeighbour(string n) {
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

void DummyRouter::sendRoutingUpdatesActivity() {
  // advertise myself to all neighbours
  sendCost(getName());
  // TODO: send the whole routing table instead

}

void DummyRouter::checkAliveActivity() {
  for (map<string, simtime_t>::iterator ls = lastSeen.begin(); 
       ls != lastSeen.end(); ) {
    if(simTime() - ls->second > par("peerFailedTimeOut")) {
      EV << "@" << getName() << ": neighbour " << ls->first << " failed" << endl;
      // Neighbour has not been seen recently and is considered failed
      // TODO: Update the routing table accordingly and inform the neighbours



      // remove the entry in the mapping, so the failure is not reported again
      lastSeen.erase(ls++);
    } else {
      ls++;
    }
  }
}

void DummyRouter::updateRoutesActivity(UpdatePacket* p) {
  EV << "@" << getName() <<": Received update from " << p->getSource()
     << ". route " << p->getSource() << " --> " << p->getDestination() 
     << ", cost " << p->getCost() << endl;
  // TODO: update routing table and advertise changes to neighbours 
  //       if received update packet makes this neccessary
  
}

void DummyRouter::sendMessageToOtherRouterActivity(string dest) {
  MessagePacket* p = new MessagePacket("", MESSAGE_PACKET);
  p->setSource(getName());
  p->setDestination(dest.c_str());
  char data[100];
  snprintf(data, 100, "This is a message from %s to %s.", getName(), dest.c_str());
  p->setData(data);

  // send off the message
  send(p, findGateForNeighbour(dest));  // this is different in Router.cc
}
