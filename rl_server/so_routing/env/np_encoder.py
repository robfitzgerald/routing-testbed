import json
import numpy as np


class NpEncoder(json.JSONEncoder):
    """
    unwraps numpy for serialization, as proposed here:
    https://stackoverflow.com/a/57915246/4803266
    """
    def default(self, obj):
        if isinstance(obj, np.integer):
            return int(obj)
        if isinstance(obj, np.floating):
            return float(obj)
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        return super(NpEncoder, self).default(obj)