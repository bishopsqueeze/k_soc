import networkx as nx
from collections import defaultdict
import sys

def read_nodeadjlist(filename):
  G = nx.Graph()
  for line in open(filename):
    e1, es = line.split(':')
    es = es.split()
    for e in es:
      if e == e1: continue
      G.add_edge(int(e1),int(e))
  return G

def findCommunities(filename):
  G = read_nodeadjlist(filename)    ##
  c = nx.connected_components(G)    ## this is the main component
  return c

if __name__ == '__main__':
    
  if len(sys.argv) < 2:
    print "Expected list of ego networks, e.g. 'python link_clustering.py *.egonet'"
    sys.exit(0)
  
  print "UserId,Predicted"

  ## for each filename
  for arg in sys.argv[1:]:
    
    ## set the egoUser to a dummy value
    egoUser = -1
    
    ## try to extract the egoUser from the filename
    try:
      egoUser = int(arg.split('/')[-1].split('.egonet')[0])
    ## if it doesn't work, then error out
    except Exception as e:
      print "Expected files to be names 'X.egonet' where X is a user ID"
      sys.exit(0)

    ## pass the filename to the function findCommunities
    cs = list(findCommunities(arg))

    ## if the output of the function is 0, then
    if len(cs) == 0:
      cs = [set(adj.keys())]

    ## otherwise join all the communities to a string
    cs = [' '.join([str(y) for y in x]) for x in cs]
    print str(egoUser) + ',' + ';'.join(cs)
