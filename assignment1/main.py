import csv

def read_csv_data(fileName="100_nodes.csv"):
    cvsFile = open(fileName, "r")
    inputfile = csv.reader(cvsFile)
    node_keys = []
    nodes = {}
    i = 0
    for row in inputfile:
        linked_nodes = [(node_nr,eval(dist)) for (node_nr, dist) in enumerate(row[3:]) if eval(dist) != 0]
        myDict ={}
        for val in linked_nodes:
            myDict[val[0]] = val[1]
        nodes[eval(row[0])] = {"x":eval(row[1]),"y":eval(row[2]),"paths":myDict}        

    cvsFile.close()
    return nodes

def calc_heuristic(node,goal_node):
    x_goal = goal_node["x"]
    y_goal = goal_node["y"]
    x_node = node["x"]
    y_node = node["y"]
    xdist = abs(x_goal-x_node)
    ydist = abs(y_goal-y_node)
    return xdist+ydist

def has_visited(key, visited):
    if key in visited:
        return True
    else:
        return False

def is_new_route_more_expensive(key,travel_cost,visited):
    if key in visited:
        if visited[key]["cost"]<travel_cost:
            return True
    else:
        return False
    
def a_star_search(goal_node_nr, weight, nodes, frontier, visited, nr_generated_nodes):
    current_node = frontier.pop(0)
    current_node_nr = current_node["nr"]
    current_travel_cost = current_node["cost"]
    current_node_neighbors = nodes[current_node_nr]["paths"]
    del current_node["nr"]
    visited[current_node_nr] = current_node 
  
    if goal_node_nr == current_node_nr:
        return {}, visited, nr_generated_nodes
        
    for key in current_node_neighbors:
        nr_generated_nodes +=1
        heuristic = calc_heuristic(nodes[key],nodes[goal_node_nr])
        travel_cost = current_node_neighbors[key]+current_travel_cost
        est_total_cost = weight*travel_cost+(1-weight)*heuristic
        
        if has_visited(key,visited) and is_new_route_more_expensive(key,travel_cost,visited):
            continue
        else:
            frontier.append({"nr":key, "cost":travel_cost,"estimated_total_cost":est_total_cost,"parent":current_node_nr})

    frontier = sorted(frontier, key=lambda d: d["estimated_total_cost"])

    return frontier, visited, nr_generated_nodes       

def main():
    nodes = read_csv_data()
    visited = {}
    frontier = []
    nr_generated_nodes = 0

    start_node_nr = int(input('Start node number: '))
    goal_node_nr = int(input('Goal node number: '))
    weight = float(input('Weight (between 0 and 1): '))

    heuristic = calc_heuristic(nodes[start_node_nr],nodes[goal_node_nr])
    heuristic =(1-weight)*heuristic
    frontier.append({"nr":start_node_nr, "cost":0, "estimated_total_cost":heuristic, "parent":-1})
    nr_generated_nodes +=1

    while len(frontier) > 0:
       frontier, visited, nr_generated_nodes =  a_star_search(goal_node_nr, weight, nodes, frontier, visited, nr_generated_nodes)
           
    if goal_node_nr in visited:
        i = goal_node_nr
        while i != -1:
            print("Node nr: {} {}".format(i,visited[i]))
            i = visited[i]["parent"]
        print("The total path cost is {}.".format(visited[goal_node_nr]['cost']))
    else:
        print("Could not find route")

    print("The total number of generated nodes are: {}".format(nr_generated_nodes))
main()
