simple DummyRouter {
  parameters:
    double periodicUpdateInterval @unit(s) = default(5s);
    double messageSendingInterval @unit(s) = default(0s);
    double diesAt @unit(s) = default(1000s);
    double peerFailedTimeOut @unit(s) = default(10s);
    double checkAliveInterval @unit(s) = default(10s);
    string messageTo = default("");

  gates:
    input in[];
    output out[];
}

network DemoNetwork {
  submodules:
    A: DummyRouter;
    B: DummyRouter;
    C: DummyRouter;

  connections:
    A.out++ --> { delay = 10ms; } --> B.in++;
    B.out++ --> { delay = 10ms; } --> A.in++;

    A.out++ --> { delay = 10ms; } --> C.in++;
    C.out++ --> { delay = 10ms; } --> A.in++;
}


simple Router {
  parameters:
    double periodicUpdateInterval @unit(s) = default(5s);
    double messageSendingInterval @unit(s) = default(0s);
    double diesAt @unit(s) = default(1000s);
    double peerFailedTimeOut @unit(s) = default(10s);
    double checkAliveInterval @unit(s) = default(10s);
    string messageTo = default("");

    double
    smallInfinity = default(0.0);

    bool useSplitHorizon = default(false);
    bool usePoisonReverse = default(false);

  gates:
    input in[];
    output out[];
}


network A_Ring4 {
  submodules:
    A: Router;
    B: Router;
    C: Router;
    D: Router;
    E: Router;

  connections:
    A.out++ --> { delay = 10ms; } --> B.in++;
    B.out++ --> { delay = 10ms; } --> A.in++;

    B.out++ --> { delay = 20ms; } --> C.in++;
    C.out++ --> { delay = 20ms; } --> B.in++;

    C.out++ --> { delay = 30ms; } --> E.in++;
    E.out++ --> { delay = 30ms; } --> C.in++;

    E.out++ --> { delay = 40ms; } --> D.in++;
    D.out++ --> { delay = 40ms; } --> E.in++;

    D.out++ --> { delay = 200ms; } --> B.in++;
    B.out++ --> { delay = 200ms; } --> D.in++;

}

network Ring5 {
  submodules:
    A: Router;
    B: Router;
    C: Router;
    F: Router;
    G: Router;

  connections:
    A.out++ --> { delay = 10ms; } --> B.in++;
    B.out++ --> { delay = 10ms; } --> A.in++;

    B.out++ --> { delay = 20ms; } --> C.in++;
    C.out++ --> { delay = 20ms; } --> B.in++;

    C.out++ --> { delay = 35ms; } --> G.in++;
    G.out++ --> { delay = 35ms; } --> C.in++;

    G.out++ --> { delay = 25ms; } --> F.in++;
    F.out++ --> { delay = 25ms; } --> G.in++;

    F.out++ --> { delay = 15ms; } --> A.in++;
    A.out++ --> { delay = 15ms; } --> F.in++;

}

network Ring5_4 {
  submodules:
    A: Router;
    B: Router;
    C: Router;
    D: Router;
    E: Router;
    F: Router;
    G: Router;

  connections:
    A.out++ --> { delay = 10ms; } --> B.in++;
    B.out++ --> { delay = 10ms; } --> A.in++;

    B.out++ --> { delay = 20ms; } --> C.in++;
    C.out++ --> { delay = 20ms; } --> B.in++;

    C.out++ --> { delay = 30ms; } --> E.in++;
    E.out++ --> { delay = 30ms; } --> C.in++;

    E.out++ --> { delay = 40ms; } --> D.in++;
    D.out++ --> { delay = 40ms; } --> E.in++;

    D.out++ --> { delay = 200ms; } --> B.in++;
    B.out++ --> { delay = 200ms; } --> D.in++;

    C.out++ --> { delay = 35ms; } --> G.in++;
    G.out++ --> { delay = 35ms; } --> C.in++;

    G.out++ --> { delay = 25ms; } --> F.in++;
    F.out++ --> { delay = 25ms; } --> G.in++;

    F.out++ --> { delay = 15ms; } --> A.in++;
    A.out++ --> { delay = 15ms; } --> F.in++;
}
