import timeit


def _benchmark(dev):
    code = f'''
import torch
device = torch.device('{dev}')
x = torch.rand((10000, 10000), dtype=torch.float32)
y = torch.rand((10000, 10000), dtype=torch.float32)
x.to(device)
y.to(device)   
x * y
'''

    print(f'running {dev} benchmark')
    t = timeit.timeit(code, number=10)
    print(f'ran test in {t:.4f} seconds')


def run_torch_benchmark():
    print('running benchmarks')
    for dev in ['cpu', 'mps']:
        _benchmark(dev)
    print('done running benchmarks')
