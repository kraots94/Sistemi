import random;
WEIGHT_MIN = 10
WEIGHT_MAX = 100

#PRE: V for the number of vertices

#POST: creates a random connected graph with a V-1 edges

def generateRandomConnectedGraph(V):

    initialSet = set()

    visitedSet = set()

    vertices = set()

    edges = set()

    #generate the set of names for the vertices

    for i in range(V):

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

    for i in range(n):

        for j in range(i+1, n):

            com.append([i,j])

    return com

V, E = generateRandomConnectedGraph(10)

print(V)
print(E)

combi = generateCombination(10)
print(combi)
