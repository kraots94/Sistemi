import random
from random import randint
from graphviz import Digraph
from graphviz.backend import version
from graphviz.dot import Graph
import sys

PATH_SEPARATOR = "\\"
WEIGHT_MIN = 5
WEIGHT_MAX = 20
TOTAL_NODES = 10
TOTAL_EDGES = 20

def generateRandomConnectedGraph(V):
    #PRE: V for the number of vertices
    #POST: creates a random connected graph with a V-1 edges

    initialSet = set()

    visitedSet = set()

    vertices = set()

    edges = set()

    #generate the set of names for the vertices

    for i in range(1, V+1):

        initialSet.add(str(i))

        vertices.add(str(i))

    #set the intial vertex to be connected

    curVertex = random.sample(initialSet, 1).pop()

    initialSet.remove(curVertex)

    visitedSet.add(curVertex)

    #loop through all the vertices, connecting them randomly

    while initialSet:

        adjVertex = random.sample(initialSet, 1).pop()

        edge = (random.randint(WEIGHT_MIN,WEIGHT_MAX), curVertex, adjVertex)

        edges.add(edge)

        initialSet.remove(adjVertex)

        visitedSet.add(adjVertex)

        curVertex = adjVertex

    return vertices, edges

def generateCombination(n):

    com = []

    for i in range(1, n+1):

        for j in range(i+1, n+1):

            com.append([i,j])

    return com

def add_edges(graph_edges, total_nodes, total_to_add):
    combinations = generateCombination(total_nodes)
    visited_combo = []

    for edge in graph_edges:
        for index in range(len(combinations)):
            if edge[1] == combinations[index][0] and edge[2] == combinations[index][1]:
                visited_combo.append(index)
                break

    for _ in range(total_to_add):
        index = randint(0,len(combinations))
        while index in visited_combo:
            index = randint(0,len(combinations)-1)

        visited_combo.append(index)

        graph_edges.add((random.randint(WEIGHT_MIN,WEIGHT_MAX), combinations[index][0], combinations[index][1]))

def create_file(V, E):
    dot = Graph(name="City_Map", filename="map.gv")
    
    for vertex in V:
        dot.node(str(vertex))

    for edge in E:
        dot.edge(str(edge[1]), str(edge[2]), label=str(edge[0]))

    dot.render(sys.path[0]+PATH_SEPARATOR+"map.gv")

V, E = generateRandomConnectedGraph(TOTAL_NODES)
edges_to_add = TOTAL_EDGES - TOTAL_NODES + 1
add_edges(E,TOTAL_NODES, edges_to_add)

print(V)
print(E)

create_file(V, E)