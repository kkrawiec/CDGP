




def loadBenchmark(path):
    f = open(path)
    data = []
    for i, row in enumerate(f):
        entries = [float(v) for v in row.split()]
        data.append((entries[:-1], entries[-1]))
    f.close()
    return data



def constraintRange(data):
    # We can never really infer that constraint with 100% accuracy. While it is true for every type of inferred constraint, this one is potentially very harmful due to its restricting effect and dependence on local values.
    min_value = data[0][1]  # output of the first example
    max_value = data[0][1]
    for (input, output) in data:
        if output < min_value:
            min_value = output
        if output > max_value:
            max_value = output
    return (min_value, max_value)


def constraintPositionSymmetry_pair():
    return None

def constraintPositionSymmetry():
    """Tests for the argument position symmetry by checking all pairs of variables."""
    return None

def constraintMonotonicity():
    return None



data = loadBenchmark("Feynman_with_units/I.6.2")
print("Range constraint:")
print(constraintRange(data))