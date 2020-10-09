import random
from random import randint
from graphviz.dot import Graph
from PIL import Image, ImageDraw, ImageFont
import sys
import math

PATH_SEPARATOR = "\\"
PATH_MAP = sys.path[0]+PATH_SEPARATOR+".." + \
    PATH_SEPARATOR + "map" + PATH_SEPARATOR
PATH_GRAPH = sys.path[0]+PATH_SEPARATOR

WEIGHT_MIN = 3
WEIGHT_MAX = 10
TOTAL_NODES = 20
TOTAL_EDGES = 40
TOTAL_CHARGING_COLS = 4
CARTESIAN_SIDE = 20
MAX_DISTANCE_SQUARED = CARTESIAN_SIDE*CARTESIAN_SIDE*2

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
        n = math.pow(26, count) * (ord(c) - ord('a'))
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
        while j >= begin and key[k] < arr[j][k]:
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


def calculateSquaredDistance(P, Q):
    diff1 = P["x"] - Q["x"]
    diff2 = P["y"] - Q["y"]
    return diff1*diff1 + diff2*diff2


def mapValues(value, istart, istop, ostart, ostop):
    return ostart + (ostop - ostart) * ((value - istart) / (istop - istart))


def getNodeFromID(node_id, Nodes):
    outNode = None
    for node in Nodes:
        if node_id == base26to10(node[0]):
            outNode = node
            break
    return outNode

def getNodeFromName(node_name, Nodes):
    outNode = None
    for node in Nodes:
        if node_name == node[0]:
            outNode = node
            break
    return outNode

def calculateWeight(U, V):
    P = {"x": U[1], "y": U[2]}
    Q = {"x": V[1], "y": V[2]}
    squaredDistance = calculateSquaredDistance(P, Q)
    return math.floor(mapValues(squaredDistance, 0, MAX_DISTANCE_SQUARED, WEIGHT_MIN, WEIGHT_MAX))

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
        node_u = getNodeFromName(currentVertex, V)
        node_v = getNodeFromName(adjVertex, V)
        weight = calculateWeight(node_u, node_v)
        edge = [weight, currentVertex, adjVertex]
        E.append(edge)
        initialList.remove(adjVertex)
        visitedList.append(adjVertex)
        currentVertex = adjVertex


def generateCombination(n):
    com = []
    for i in range(0, n):
        for j in range(i+1, n):
            com.append([i, j])
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
        index = randint(0, len(combinations) - 1)
        while index in visited_combo:
            index = randint(0, len(combinations) - 1)
        current_combination = combinations[index]
        node_u = graph_vertices[current_combination[0]]
        node_v = graph_vertices[current_combination[1]]
        weight = calculateWeight(node_u, node_v)
        graph_edges.append([weight, node_u[0], node_v[0]])
        visited_combo.append(index)


def createColumns(Nodes, N):
    tmp = 1
    while True:
        if tmp * tmp >= N:
            break
        tmp = tmp+1
    square_side = math.floor(CARTESIAN_SIDE / tmp)

    matrix = []
    for i in range(tmp):
        matrix.append([])
        for _ in range(tmp):
            matrix[i].append({"points": [],
                              "col_inside": False})

    def mapFunc1(vertex):
        new_x = math.floor((vertex[1]-1)/square_side)
        new_y = math.floor((vertex[2]-1)/square_side)
        new_x = tmp-1 if new_x >= tmp else new_x
        new_y = tmp-1 if new_y >= tmp else new_y
        return [new_x, new_y, vertex]

    points_mapped = list(map(mapFunc1, Nodes))

    for point in points_mapped:
        new_x = point[0]
        new_y = point[1]
        matrix[new_y][new_x]["points"].append(point[2])

    avaiableChunks = []
    for i in range(tmp):
        for j in range(tmp):
            if len(matrix[i][j]["points"]) > 0:
                avaiableChunks.append([i, j])

    Chargin_Cols_Positions = []
    total_avaiable_chunks = len(avaiableChunks)
    total_cols_to_insert = min(total_avaiable_chunks, TOTAL_CHARGING_COLS)
    for _ in range(total_cols_to_insert):

        while True:
            randomPos = random.randint(0, total_avaiable_chunks - 1)
            random_i = avaiableChunks[randomPos][0]
            random_j = avaiableChunks[randomPos][1]
            if matrix[random_i][random_j]["col_inside"] == False:
                break

        matrix[random_i][random_j]["col_inside"] = True
        points_in_pos = len(matrix[random_i][random_j]["points"])
        randomPos = random.randint(0, points_in_pos - 1)
        Point = matrix[random_i][random_j]["points"][randomPos]

        Chargin_Cols_Positions.append(Point)

    return Chargin_Cols_Positions


def create_file_graph(V, E):
    fo = open(PATH_MAP+"city_map_graph.dat", "w")
    total_nodes = len(V)
    total_edges = len(E)
    graph_type = "undirected"
    weights_type = "d"
    fo.write("{} {} {} {}\n".format(
        total_nodes, total_edges, graph_type, weights_type))
    for edge in E:
        fo.write(str(base26to10(edge[1])) + " " +
                 str(base26to10(edge[2])) + " " + str(edge[0]) + "\n")
    fo.close()


def create_file_nodes_positions(V):
    fo = open(PATH_MAP+"city_map_nodes.dat", "w")
    fo.write(str(len(V)) + "\n")
    for vertex in V:
        fo.write(vertex[0] + " " + str(base26to10(vertex[0])) +
                 " "+str(vertex[1]) + " " + str(vertex[2]) + "\n")
    fo.close()


def create_file_dot(V, E):
    dot = Graph(name="City_Map", filename="map")

    for vertex in V:
        dot.node(str(vertex[0]), label="{} {} ({},{})".format(
            str(base26to10(vertex[0])), vertex[0], vertex[1], vertex[2]))

    for edge in E:
        dot.edge(str(edge[1]), str(edge[2]), label=str(edge[0]))

    dot.render(filename="map", directory=PATH_GRAPH, cleanup=True)


def create_file_columns(Cols):
    fo = open(PATH_MAP+"city_map_charging_cols.dat", "w")
    fo.write(str(len(cols)) + "\n")
    for col in Cols:
        fo.write(col[0] + " " + str(base26to10(col[0])) +
                 " "+str(col[1]) + " " + str(col[2]) + "\n")
    fo.close()


def create_image(V, Cols):
    tot_letters_name = len(V[0][0])
    letter_width = 14
    letter_height = 14
    img_width = CARTESIAN_SIDE*letter_width*tot_letters_name
    img_height = CARTESIAN_SIDE*letter_height*tot_letters_name
    img = Image.new('RGB', (img_width, img_height), color='white')
    fnt = ImageFont.truetype(PATH_GRAPH+"FiraMono-Medium.ttf", 20)
    d = ImageDraw.Draw(img)
    for vertex in V:
        name = vertex[0].upper()
        x = (vertex[1]-1) * letter_width * tot_letters_name + 1
        increment = (letter_height*tot_letters_name - letter_height) / 2
        y = img_height - (vertex[2]) * letter_height * \
            tot_letters_name - increment
        isChargingCol = False
        for col in Cols:
            if name == col[0].upper():
                isChargingCol = True
                break

        if isChargingCol:
            d.text((x, y), name, font=fnt, fill=(255, 0, 0))
        else:
            d.text((x, y), name, font=fnt, fill=(0, 0, 0))

    img.save(PATH_GRAPH+'map.png')


edges_to_add = TOTAL_EDGES - TOTAL_NODES + 1

V = createNodes(TOTAL_NODES)
E = []
createPath(V, E)
add_edges(E, V, TOTAL_NODES, edges_to_add)
cols = createColumns(V, TOTAL_CHARGING_COLS)

create_file_graph(V, E)
create_file_nodes_positions(V)
create_file_columns(cols)
create_file_dot(V, E)
create_image(V, cols)
