#import networkx as nx
#G=nx.Graph()
#G.add_node("spam")
#G.add_edge(1,2)
#print(G.nodes())


import networkx as nx
import matplotlib.pyplot as plt

G=nx.random_geometric_graph(200,0.125)
# position is stored as node attribute data for random_geometric_graph
pos=nx.get_node_attributes(G,'pos')

# find node near center (0.5,0.5)
dmin=1
ncenter=0
for n in pos:
    x,y=pos[n]
    d=(x-0.5)**2+(y-0.5)**2
    if d<dmin:
        ncenter=n
        dmin=d

# color by path length from node near center
p=nx.single_source_shortest_path_length(G,ncenter)

plt.figure(figsize=(8,8))
nx.draw_networkx_edges(G,pos,nodelist=[ncenter],alpha=0.4)
nx.draw_networkx_nodes(G,pos,nodelist=p.keys(),
                       node_size=80,
                       node_color=p.values(),
                       cmap=plt.cm.Reds_r)

plt.xlim(-0.05,1.05)
plt.ylim(-0.05,1.05)
plt.axis('off')
#plt.savefig('random_geometric_graph.png')
plt.show()
G_old=G.copy()

# threshold function
# theta(v(i))
theta={}
for n in G.nodes():
    theta[n]=0.5
    
k={}
for n in G.nodes():
    k[n]=theta[n]*G.degree(n)

dist={}
for n in range(G.number_of_nodes()):
    dist[n]=G.degree(n)-k[n]
oo=float('infinity')

    
flag=True
while flag:
    (m,i) = min((v,i) for i,v in enumerate(dist.values()))
    if dist[i]==oo:
        flag=False
    else:
        for v in G.neighbors(i):
#            print 'v = ',v, 'dist = ', dist[v]
            if dist[v]>0:
                #print 'v = ',v, 'dist = ', dist[v] -1
                dist[v]=dist[v]-1
#                print 'v = ',v, 'new dist = ', dist[v]

            else:
                dist[v]=oo
        G.remove_node(i)
        dist[i]=oo
        
for i in G.nodes():
    print 'remaining nodes', i
    p[i]=0
# find node near center (0.5,0.5)
plt.figure(figsize=(8,8))
nx.draw_networkx_edges(G_old,pos,nodelist=[ncenter],alpha=0.4)
nx.draw_networkx_nodes(G_old,pos,nodelist=p.keys(),
                       node_size=80,
                       node_color=p.values(),
                       cmap=plt.get_cmap('jet'))

plt.xlim(-0.05,1.05)
plt.ylim(-0.05,1.05)
plt.axis('off')
#plt.savefig('random_geometric_graph.png')
plt.show()
    
        
