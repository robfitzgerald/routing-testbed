import rl_server.rllib_server.server_driver_policy as d_server
import rl_server.rllib_server.server_network_policy_v2 as n_server
import os
import sys
import logging

log = logging.getLogger("rl_server")


def run_main():
    services = ['driver', 'network']
    service = os.environ.get("SERVICE")

    if service == "driver":
        return d_server.run()
    elif service == "network":
        return n_server.run()
    else:
        log.error(f'os.environ["SERVICE"] = {service}')
        log.error(os.environ)
        log.error(
            f"please set the environment variable SERVICE to one of: {str(services)}")
        return 1


if __name__ == "__main__":
    sys.exit(run_main())
