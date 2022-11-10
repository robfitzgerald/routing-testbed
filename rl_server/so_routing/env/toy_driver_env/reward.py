from typing import List


def jain_index(values) -> float:
    var_coef = values.std() / values.mean()
    return 1 / (1 + (var_coef ** 2))


def fair_allocation_mark(values) -> float:
    b1 = sum(values)
    b2 = sum([v * v for v in values])
    fam = None if b2 == 0 else b2 / b1
    return fam


def jain_user_fairness(values) -> List[float]:
    fam = fair_allocation_mark(values)
    if fam is None:
        # special case where the variance is 0 (100% fair)
        return [1.0 for _ in values]
    else:
        return [v / fam for v in values]
