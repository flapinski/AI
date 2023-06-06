import statistics
import numpy as np
import random as rand
import time
from math import floor
import matplotlib.pyplot as plt
from matplotlib.path import Path

def Nodes():
    data = [0, 0, 36, 64, 1, 3, 94, 47, 2, 12, 10, 23, 3, 25, 16, 46, 4, 4, 25, 79, 5, 11, 41, 30, 6, 20, 81, 45, 7, 21, 14, 79, 8, 10, 42, 56, 9, 20, 90, 17, 10, 13, 41, 39, 11, 14, 21, 14, 12, 16, 41, 46, 13, 17, 65, 96, 14, 11, 13, 49, 15, 36, 21, 14, 16, 6, 57, 2, 17, 7, 14, 42, 18, 21, 66, 62, 19, 11, 58, 96, 20, 17, 5, 51, 21, 22, 41, 50, 22, 10, 50, 99, 23, 19, 84, 85, 24, 21, 97, 90, 25, 23, 47, 76, 26, 19, 11, 54, 27, 15, 60, 97, 28, 22, 60, 89, 29, 7, 58, 68, 30, 11, 30, 93, 31, 15, 9, 60, 32, 22, 47, 44, 33, 12, 19, 40, 34, 24, 15, 40, 35, 25, 88, 21, 36, 2, 33, 58, 37, 15, 21, 51, 38, 18, 57, 7, 39, 13, 81, 6, 40, 3, 49, 6, 41, 20, 51, 78, 42, 14, 9, 62, 43, 10, 84, 36, 44, 10, 95, 76, 45, 66, 89, 44, 46, 10, 10, 49, 47, 7, 69, 16, 48, 12, 75, 66, 49, 24, 97, 11, 50, 5, 74, 69, 51, 18, 1, 14, 52, 7, 96, 91, 53, 11, 46, 22, 54, 12, 74, 92]
    nodes = []
    for i in range(len(data)):
        if i % 4 == 0:
            dicty = {'dem': int(data[i+1]),
                     'x': int(data[i+2]),
                     'y': int(data[i+3])}
            nodes.append(dicty)
    return nodes


def Dist(i, j, nodes):
    xi = nodes[i]['x']
    yi = nodes[i]['y']
    xj = nodes[j]['x']
    yj = nodes[j]['y']
    return np.sqrt((xi-xj)**2+(yi-yj)**2)


def D_Array(nodes):
    n = len(nodes)
    D = np.empty((n, n))
    for i in range(n):
        for j in range(n):
            d = Dist(i, j, nodes)
            D[i, j] = d
    return D


def Initialize_Population(N, D, nodes, w):
    ind = [i for i in range(len(nodes))]
    ind = ind + [0 for i in range(7)]
    Population = []
    for i in range(N):
        x = rand.sample(ind, len(ind))
        f, d, feas = Fitness(x, D, nodes, w)
        dicty = {'route': x, 'fitness': f, 'distance': d, 'feasible': feas}
        Population.append(dicty)
    return Population


def Fitness(x, D, nodes, w):
    d = 0
    maxload = 100
    dmin = 1073
    x = [0] + x + [0]           # start and end at depot
    for i in range(len(x)-1):
        d += D[x[i], x[i+1]]
    indices = [i for i, val in enumerate(x) if val == 0]
    overload = 0
    feasible = True
    for i in range(len(indices)-1):     # for all of the 9 routes
        demand = 0
        start = indices[i]
        end = indices[i+1]
        route = x[start+1:end]
        for nr in route:               # for every node on the route
            demand += nodes[nr]['dem']
        if demand > maxload:
            overload += demand - maxload
            feasible = False    # feasible = False if any of the cars take more than 100 units
    if feasible == False:
        overload += 10
    f = dmin/d - w*overload     # dmin/d = 0.3 to 1, overload = 0-400
    return f, d, feasible


def Select_Parent(Population, n, tprop):
    tournament_size = floor(tprop*n)
    participants = rand.choices(Population, k=tournament_size)
    winner = Fittest(participants)
    return winner.get('route')


def Replace_Zeros(chromosome):
    replaced = [gene for gene in chromosome]
    M = max(chromosome)+1
    for j in range(len(chromosome)):
        if replaced[j] == 0:
            replaced[j] = M
            M += 1
    return replaced


def Crossover(parent1, parent2, N):
    mom = Replace_Zeros(parent1)
    dad = Replace_Zeros(parent1)
    start, end = sorted(rand.choices(range(N+1), k=2))
    daughter = [None]*N
    son = [None]*N
    daughter[start:end] = mom[start:end]
    son[start:end] = dad[start:end]
    i, j = 0, 0
    while i < N and j < N:
        if dad[j] in daughter[start:end]:
            j += 1
        else:
            if i == start:
                i = end
            daughter[i] = dad[j]
            i += 1
            j += 1

    i, j = 0, 0
    while i < N and j < N:
        if mom[j] in son[start:end]:
            j += 1
        else:
            if i == start:
                i = end
            son[i] = mom[j]
            i += 1
            j += 1
    for i in range(N):  # remove the numbers >54 and replace with zeros again
        if daughter[i] > 54:
            daughter[i] = 0
        if son[i] > 54:
            son[i] = 0
    return daughter,son
    
    

def Mutate(x, mutation_rate, D):
    if rand.uniform(0, 1) <= mutation_rate:
        mutation_nr = rand.randrange(1,4)   # the nr of mutations to do: 1, 2, or 3
        for i in range(mutation_nr):
            mutation_id = rand.randrange(4) # which mutation to choose
            if mutation_id == 0:  # swap
                j = rand.randrange(len(x))
                k = rand.randrange(len(x))
                temp = x[j]
                x[j] = x[k]
                x[k] = temp
            if mutation_id == 1:  # insert
                j = rand.randrange(len(x))
                gene = x.pop(j)
                k = rand.randrange(len(x))
                x.insert(k, gene)
            if mutation_id == 3:  # inversion
                j = rand.randrange(len(x))
                b = rand.randrange(len(x)-j)
                k = j + b
                x[j:k] = x[j:k][::-1]
            if mutation_id == 2:  # inversion for one route
                x = [0] + x + [0]
                zeroind = [i for i, val in enumerate(x) if val == 0]
                start = rand.choice(zeroind[:len(zeroind)-1])
                ind = zeroind.index(start)
                end = zeroind[ind+1]
                route = x[start+1:end]
                if len(route)>2:
                    i = rand.randrange(len(route))
                    b = rand.randrange(len(route)-i)
                    j = i + b
                    route[i:j] = route[i:j][::-1]
                    x[start+1:end] = route
                x.pop(0)
                x.pop()
    return x


def divide_routes(candidate_nodes):
    candidate_nodes = [0] + candidate_nodes + [0]
    zero_indices = [
        index for (index, val) in enumerate(candidate_nodes)
        if val == 0
    ]
    routes = [
        candidate_nodes[zero_indices[i]:(zero_indices[i+1] + 1)]
        for i in range(len(zero_indices)-1)
    ]
    return routes


def plot(candidate_nodes, all_nodes, fitness_values):
    routes = divide_routes(candidate_nodes)
    myxs = [[all_nodes[i]["x"] for i in route] for route in routes]
    myys = [[all_nodes[i]["y"] for i in route] for route in routes]

    plt.figure(1)
    plt.title("Routes ")
    for i in range(len(routes)):
        plt.plot(myxs[i], myys[i], marker="*")

    plt.show()
    #plt.savefig("routes.png")  # Uncomment to save figure

    plt.figure(2)
    plt.title("Fitness value across generations")
    plt.plot(fitness_values)
    plt.show()
    #plt.savefig("fitness.png")  # Uncomment to save figure

def Fittest(Popu):
    bf = -1000  # initialize as a large, negative number
    ind = 0
    for i in range(len(Popu)):
        f = Popu[i].get('fitness')
        if f > bf:
            bf = f
            ind = i
    return Popu[ind]

def Shortest(Popu):
    bd = 10000
    ind = 0
    for i in range(len(Popu)):
        d = Popu[i].get('distance')
        if d < bd:
            bd = d
            ind = i
    return Popu[ind]


def Main():
    start_time = time.time()

    runs = 20
    nodes = Nodes()
    D = D_Array(nodes)

    N = 250         # population size
    I = 350         # no. of generations
    alpha = 0       # crossover rate
    beta = 0.6      # mutation rate
    w = 0.003      # penalty weight
    tprop = 0.15    # proportion that participates in tournament selection
    Best_of_all_runs = []   # contains the best result from every run
    execution_times = []
    
    for r in range(runs):
        small_time = time.time()
        Popu = Initialize_Population(N, D, nodes, w)
        n = len(Popu[0].get('route'))   # route length
        if r == 0:
            fitness_values = []
        for _ in range(I):
            NewPopu = []
            for j in range(int(N/2)): # N/2 since we produce two children in each iteration
                parent1 = Select_Parent(Popu, n, tprop)
                parent2 = Select_Parent(Popu, n, tprop)
                if rand.uniform(0,1) < alpha:
                    child1, child2 = Crossover(parent1, parent2, n)
                else:
                    child1 = [x for x in parent1]
                    child2 = [x for x in parent2]
                
                child1 = Mutate(child1, beta, D)
                child2 = Mutate(child2, beta, D)
                    
                f1, d1, feas1 = Fitness(child1, D, nodes, w)
                dict1 = {'route': child1, 'fitness': f1, 'distance': d1, 'feasible': feas1}
                NewPopu.append(dict1)

                f2, d2, feas2 = Fitness(child2, D, nodes, w)
                dict2 = {'route': child2, 'fitness': f2, 'distance': d2, 'feasible': feas2}
                NewPopu.append(dict2)

            bfitted = Fittest(Popu)
            if r == 0:
                bfitness = bfitted.get('fitness')
                fitness_values.append(bfitness)
            NewPopu[0] = bfitted    # keep the best chromosome from the last generation
            Popu = NewPopu
            
        ## Find shortest solution that is feasible:
        Solutions = [x for x in Popu if x.get('feasible')==True]
        if len(Solutions) > 0:
            Best = Shortest(Solutions)
            Best_of_all_runs.append(Best)
            print('Total distance: %.2f' % Best.get('distance'))
        else:
            print('The run did not result in any feasible solutions!')
        ex_time  = time.time() - small_time
        print("Execution time for this run: %.2f seconds \n" % (ex_time))
        execution_times.append(ex_time)

    if len(Best_of_all_runs) > 0:   # If at least one run resulted in a feasible solution
        Best_ever_dict = Shortest(Best_of_all_runs)
        Best_ever = [0]+Best_ever_dict.get('route')+[0] # This is so we get the correct initial and end '0'

        ## Overview of the results of all runs:
        print('Number of feasible solutions: %s' % len(Best_of_all_runs))
        short_enough = [x for x in Best_of_all_runs if x.get('distance')<=1073*1.1]    # best solution should be no worse than 1.1 times the optimal solution
        print('Solutions shorter than 1073*1.1: %s \n' % len(short_enough))
        print("Maximum execution time: %.2f" % max(execution_times))
        print("Minimum execution time: %.2f" % min(execution_times))
        print("Mean execution time: %.2f" % statistics.mean(execution_times))
        print("Median execution time: %.2f \n" % statistics.median(execution_times))

        ## Info on the best solution out of all runs:
        print('Best route of all runs: %s' % Best_ever)
        print('Total distance: %.2f' % Best_ever_dict.get('distance'))
        print("Execution time: %.2f seconds \n" % (time.time() - start_time))
        plot(Best_ever, nodes, fitness_values)
    else:
        print('None of the runs resulted in feasible solutions!')
        print("Execution time: %.2f seconds \n" % (time.time() - start_time))

Main()
