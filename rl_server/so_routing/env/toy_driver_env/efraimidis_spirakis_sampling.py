def reservoir_sampling(population, weights, k, rng):
    """
    taken from 
    https://maxhalford.github.io/blog/weighted-sampling-without-replacement/
    samples k items from a population where each member of the population
    has a weight value
    """
    # guard against divide by zero
    almost_zero = 0.00000001
    w_clean = list(weights)
    for idx, w in enumerate(w_clean):
        if w == 0:
            w_clean[idx] = almost_zero

    # reservoir sampling
    v = [rng.random() ** (1 / w) for w in w_clean]
    order = sorted(range(len(population)), key=lambda i: v[i])
    return [population[i] for i in order[-k:]]
