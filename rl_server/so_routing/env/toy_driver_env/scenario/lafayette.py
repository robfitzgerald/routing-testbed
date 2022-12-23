from rl_server.so_routing.env.toy_driver_env.scenario.scenario import Scenario

# based _roughly_ on current MATSim scenario, distance units are in meters
# zone of control data based on recent runs using an arterial intersection
# heuristic for filtering zones, which found around 40 down from 400.
STEP_SIZE_SECONDS = 30
AVG_SPEED_MPH = 25
MAX_REPLANNINGS = 20

AVG_SPEED_MPS = AVG_SPEED_MPH * 1609
STEPS_PER_HOUR = 3600 / STEP_SIZE_SECONDS
MEAN_STEP_DISTANCE = AVG_SPEED_MPS / STEPS_PER_HOUR

# values based on an agentExperience.csv output from MATSim Lafayette runs
MEAN_TRIP_DISTANCE = 5219
STDEV_TRIP_DISTANCE = 2601
MAX_TRIP_DISTANCE = 16520
MEAN_DELAY_DISTANCE = (MAX_TRIP_DISTANCE -
                       MEAN_TRIP_DISTANCE) / MAX_REPLANNINGS

Lafayette = Scenario(
    name="Lafayette, CO",
    adoption_rate=0.20,
    min_start_time=0,
    max_start_time=60,
    max_replannings=20,
    max_trip_increase_pct=3.0,
    free_flow_drivers_threshold=500,
    half_speed_drivers_threshold=1500,
    mean_step_distance=100,
    mean_congestion_delay=10,  # meters per timestep
    mean_replanning_delay=MEAN_DELAY_DISTANCE,
    mean_trip_distance=MEAN_TRIP_DISTANCE,
    stdev_trip_distance=STDEV_TRIP_DISTANCE,
    min_trip_distance=STDEV_TRIP_DISTANCE,
    max_trip_distance=MAX_TRIP_DISTANCE,
    zones_of_control=100,
    zones_coverage_percent=0.8
)
