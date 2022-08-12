import json
from pathlib import Path
from pkg_resources import resource_filename

CONF_FILE = resource_filename("rl_server.rllib_server", "conf.json")


def server_conf(server):
    with Path(CONF_FILE).open('r') as f:
        conf = json.loads(f.read())
    s_conf = conf.get(server)
    if s_conf is None:
        raise KeyError(f'unknown server configuration {server}')
    else:
        return s_conf
