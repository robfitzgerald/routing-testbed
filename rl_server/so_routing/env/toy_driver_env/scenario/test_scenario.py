from rl_server.so_routing.env.toy_driver_env.scenario.scenario import Scenario

TestScenario = Scenario(
    name="Test World",
    adoption_rate=0.20,
    min_start_time=0,
    max_start_time=50,
    max_replannings=20,
    max_trip_increase_pct=2.0,
    free_flow_drivers_threshold=10,
    half_speed_drivers_threshold=30,
    mean_step_distance=2,
    mean_congestion_delay=1,
    mean_replanning_delay=5,
    mean_trip_distance=30,
    stdev_trip_distance=10,
    min_trip_distance=10,
    max_trip_distance=50,
    zones_of_control=16,
    zones_coverage_percent=1.00
)
