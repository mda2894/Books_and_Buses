import elkai
import numpy


cities = elkai.Coordinates2D({
    'city1': (0, 0),
    'city2': (0, 4),
    'city3': (5, 0)
})

print(cities.solve_tsp())

cities = elkai.DistanceMatrix([
    [0, 4, 0],
    [0, 0, 5],
    [0, 0, 0]
])

print(cities.solve_tsp())

dist_matrix = numpy.loadtxt("data/NB_dist_matrix.csv", delimiter = ",", dtype = int)
dist_matrix_list = dist_matrix.tolist()
elkai_dist_matrix = elkai.DistanceMatrix(dist_matrix_list)

solution = elkai_dist_matrix.solve_tsp()
