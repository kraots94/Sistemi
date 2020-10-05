import random
from random import randint
from graphviz.dot import Graph
from PIL import Image, ImageDraw, ImageFont
import sys
import math

PATH_SEPARATOR = "\\"
PATH_MAP = sys.path[0]+PATH_SEPARATOR+".."+ PATH_SEPARATOR +"map" + PATH_SEPARATOR
PATH_GRAPH = sys.path[0]+PATH_SEPARATOR

WEIGHT_MIN = 5
WEIGHT_MAX = 20
TOTAL_NODES = 20
TOTAL_EDGES = 30
CARTESIAN_SIDE = 100

"""convert positive decimal integer n to equivalent in another base (2-26)"""
def baseconvert(n, base):

    digits = "abcdefghijklmnopqrstuvwxyz"

    try:
        n = int(n)
        base = int(base)
    except:
        return ""

    if n < 0 or base < 2 or base > 26:
        return ""

    s = ""
    while 1:
        r = math.floor(n % base)
        s = digits[r] + s
        n = math.floor(n / base)
        if n == 0:
            break

    return s

def reverse_string(str):  
    str1 = ""   # Declaring empty string to store the reversed string  
    for i in str:  
        str1 = i + str1  
    return str1    # It will return the reverse string to the caller function  

''' Only works if symbols are: "abcdefghijklmnopqrstuvwxyz"'''
def base26to10(number26):
    reversed = reverse_string(number26)
    out = 0
    count = 0
    for c in reversed:
        n = math.pow(26,count) * (ord(c) - ord('a'))
        out = out + n
        count = count + 1

    return math.floor(out)

# Function to do insertion sort 
def insertionSort(arr, begin, end, k): 

    # Traverse through 1 to len(arr) 
    for i in range(begin+1, end+1): 
  
        key = arr[i]
        # Move elements of arr[0..i-1], that are 
        # greater than key, to one position ahead 
        # of their current position 
        j = i-1
        while j >=begin and key[k] < arr[j][k] : 
                arr[j+1] = arr[j]
                j -= 1
        arr[j+1] = key 

def createNodes(N):
    vertices = []
    for _ in range(0, N):
        x = y = -1
        can_not_exit = True
        while can_not_exit:
            x = randint(1, CARTESIAN_SIDE)
            y = randint(1, CARTESIAN_SIDE)
            can_not_exit = False
            for v in vertices:
                if x == v[1] and y == v[2]:
                    can_not_exit = True
                    break
            
        new_vertex = ["", x, y]
        vertices.append(new_vertex)

    #sorting
    insertionSort(vertices, 0, N-1, 1)

    begin = 0
    while 1:
        if begin >= N:
            break

        currentValue = vertices[begin][1]
        end = begin + 1
        while end < N and vertices[end][1] == currentValue:
            end = end + 1

        insertionSort(vertices, begin, end-1, 2)
        begin = end

    last_node_name = baseconvert(N, 26)
    name_length = len(last_node_name)

    for i in range(0, N):
        name = baseconvert(i, 26)
        while len(name) < name_length:
            name = "a"+name
        vertices[i][0] = name

    return vertices

def createPath(V, E):
    visitedList = []
    initialList = []
    for vertex in V:
        initialList.append(vertex[0])


    index = randint(0, len(initialList)-1)
    currentVertex = initialList[index]
    initialList.remove(currentVertex)
    
    visitedList.append(currentVertex)

    while len(initialList) > 0:
        index = randint(0, len(initialList)-1)
        adjVertex = initialList[index]
        edge = [random.randint(WEIGHT_MIN,WEIGHT_MAX), currentVertex, adjVertex]
        E.append(edge)
        initialList.remove(adjVertex)
        visitedList.append(adjVertex)
        currentVertex = adjVertex

def generateCombination(n):
    com = []
    for i in range(0, n):
        for j in range(i+1, n):
            com.append([i,j])
    return com

def add_edges(graph_edges, graph_vertices, total_nodes, total_to_add):
    combinations = generateCombination(total_nodes)
    visited_combo = []

    for edge in graph_edges:
        from_edge = edge[1]
        to_edge = edge[2]

        for index in range(len(combinations)):
            current_combination = combinations[index]

            graph_vertex_from = graph_vertices[current_combination[0]][0]
            graph_vertex_to = graph_vertices[current_combination[1]][0]
            if from_edge == graph_vertex_from and to_edge == graph_vertex_to:
                visited_combo.append(index)
                break

            graph_vertex_from = graph_vertices[current_combination[1]][0]
            graph_vertex_to = graph_vertices[current_combination[0]][0]
            if from_edge == graph_vertex_from and to_edge == graph_vertex_to:
                visited_combo.append(index)
                break

    for _ in range(total_to_add):
        index = randint(0,len(combinations) -1)
        while index in visited_combo:
            index = randint(0,len(combinations) -1)
        current_combination = combinations[index]
        graph_edges.append([random.randint(WEIGHT_MIN,WEIGHT_MAX), graph_vertices[current_combination[0]][0], graph_vertices[current_combination[1]][0]])
        visited_combo.append(index)

def create_file_graph(V, E):
    fo = open(PATH_MAP+"city_map_graph.dat", "w")
    total_nodes = len(V)
    total_edges = len(E)
    graph_type = "undirected"
    weights_type = "d"
    fo.write("{} {} {} {}\n".format(total_nodes, total_edges, graph_type, weights_type))
    for edge in E:
        fo.write(str(base26to10(edge[1])) + " " + str(base26to10(edge[2]))  + " " + str(edge[0]) + "\n")
    fo.close()

def create_file_nodes_positions(V):
    fo = open(PATH_MAP+"city_map_nodes.dat", "w")
    fo.write(str(len(V)) + "\n")
    for vertex in V:
        fo.write(vertex[0] + " "+ str(base26to10(vertex[0])) +" "+str(vertex[1]) + " " +str(vertex[2]) + "\n")
    fo.close()

def create_file_dot(V, E):
    dot = Graph(name="City_Map", filename="map")
    
    for vertex in V:
        dot.node(str(vertex[0]), label="{} {} ({},{})".format(str(base26to10(vertex[0])), vertex[0], vertex[1], vertex[2]))

    for edge in E:
        dot.edge(str(edge[1]), str(edge[2]), label=str(edge[0]))

    dot.render(filename="map", directory=PATH_GRAPH, cleanup=True)

def create_image(V):    
    tot_letters_name = len(V[0][0])
    letter_width = 5
    letter_height = 8
    img_width = CARTESIAN_SIDE*letter_width*tot_letters_name
    img_height = CARTESIAN_SIDE*letter_height
    img = Image.new('RGB', (img_width, img_height), color = 'white')
    fnt = ImageFont.truetype(PATH_GRAPH+"digital_7_mono.ttf", 10)
    d = ImageDraw.Draw(img)
    for vertex in V:
        name = vertex[0]
        x = vertex[1] * letter_width * tot_letters_name
        y = img_height - vertex[2] * letter_height
        d.text((x,y), name, font=fnt, fill=(0,0,0))

    img.save(PATH_GRAPH+'map.png')

edges_to_add = TOTAL_EDGES - TOTAL_NODES + 1

V = createNodes(TOTAL_NODES)
E = []
createPath(V, E)
add_edges(E, V, TOTAL_NODES, edges_to_add)

create_file_graph(V, E)
create_file_nodes_positions(V)
create_file_dot(V, E)
create_image(V)