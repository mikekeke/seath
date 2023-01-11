# Off-chain interaction prototype

Prototype of the off-chain client-server communication.

In this prototype several "nodes" can communicate through IPv4 to establish a p2p connection and exchange messages. The connection graph can be cyclic or acyclic. Through message exchange nodes can form a spanning tree connection by selecting "leader" (root node) and choosing "parent" among their own peers. The algorithm is based on Pinecones' [parent selection](https://matrix-org.github.io/pinecone/tree/parent_selection) and [root election](https://matrix-org.github.io/pinecone/tree/root_election), although simplified a lot.

Each `n` seconds nodes run a hash function to get a new hash which is then used to determine the leader - each node's hash is propagated through the network and the node with the smallest hash is selected as the leader. After each leader selection node also chooses new parent and overall connection overlay is rebuilt anew.

With minimal additions, nodes can get the ability to send payload directly to the leader. With some sort of protocol it should be possible to add the ability to propagate payload to the leader through the chain of ancestors starting from the direct parent.

At the moment setup is made to work on a single host machine and the node only knows the ports of peers, parent, and root.

## Possible TODOs

- don't accept any messages but connection requests from clients who are not peers
- do not react to root announcement immediately by switching root and parent, coz we actually don't need to know parent and root until we wish to propagate transactions to the leader
  - collect root announcements like Pinecone routers do to some storage
  - at the time when payload need to be propagated to the leader, traverse announcements storage to identify parent and leader
  - how safe is it tho?
- add ability to work over different hosts
