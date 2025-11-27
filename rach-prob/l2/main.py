import numpy as np
import matplotlib.pyplot as plt
import typing
import numpy.typing as npt
from numpy.random import Generator, MT19937

# m - # of balls
# n - # of bins

# Bn : First collision
# Un : # empty bins after n balls
# Cn : min # balls, so that there are no empty bins
# Dn : min # balls, so that there are at least 2 balls in each bin
# Dn - Cn : # balls to from 1 to 2 in each bin

# Simulate with n [1000, 2000, ..., 100 000] and with
# k = 50 repetitions

IntArray = np.typing.NDArray[np.int_]
ContextFunc = typing.Callable[[np.int64, np.int64, IntArray], None]
StopFunc = typing.Callable[[], bool]

GENERATOR = Generator(MT19937())

FIRST_COLLISION_NAME = 'Bn'
NUM_EMPTY_BINS_NAME = 'Un'
MIN_NO_EMPTY_BINS_NAME = 'Cn'
MIN_TWO_IN_EACH_BIN_NAME = 'Dn'
MIN_TWO_MINUS_MIN_NO_EMPTY_NAME = 'Dn_minus_Cn'

STATS = {
    FIRST_COLLISION_NAME: 0,
    NUM_EMPTY_BINS_NAME: [],
    MIN_NO_EMPTY_BINS_NAME: 0,
    MIN_TWO_IN_EACH_BIN_NAME: 0,
    MIN_TWO_MINUS_MIN_NO_EMPTY_NAME: 0,
}


# FIXME: This hurts my soul, use numba for speedup
def sim_balls_in_bins(num_bins: int, stop_fn: StopFunc, *funcs: ContextFunc):
    """Simulates placing randomly `num_balls` balls into `num_bins` bins,
    returns the count of balls in each bin. Calls each `context` function after
    placing each ball with (ball #, chosen bin index, current bins array).

    Args:
        num_bins (int): number of bins
        num_balls (int): number of balls
        *funcs: context functions to call after placing each ball
    """
    bins = np.zeros(num_bins, dtype=int)
    ball_no = np.int_(0)
    while not stop_fn():
        chosen_bin = GENERATOR.integers(0, num_bins)
        bins[chosen_bin] += 1
        for f in funcs:
            f(ball_no, chosen_bin, bins)
        ball_no += 1

# Context functions, called during simulation

IS_FIRST_COLLISION = False
def first_collision_context(ball_no: np.int64, chosen_bin: np.int64, bins: IntArray):
    global IS_FIRST_COLLISION
    if not IS_FIRST_COLLISION:
        if bins[chosen_bin] > 1:
            IS_FIRST_COLLISION = True
            STATS[FIRST_COLLISION_NAME] = ball_no


MIN_NO_EMPTY_BINS_SET = set()
IS_MIN_NO_EMPTY_BINS = False
def min_no_empty_bins_context(ball_no: np.int64, chosen_bin: np.int64, bins: IntArray):
    global IS_MIN_NO_EMPTY_BINS, MIN_NO_EMPTY_BINS_SET
    if not IS_MIN_NO_EMPTY_BINS:
        if len(MIN_NO_EMPTY_BINS_SET) < len(bins):
            MIN_NO_EMPTY_BINS_SET.add(chosen_bin)
        else:
            IS_MIN_NO_EMPTY_BINS = True
            STATS[MIN_NO_EMPTY_BINS_NAME] = ball_no


def count_empty_bins_context(ball_no: np.int64, chosen_bin: np.int64, bins: IntArray):
    global IS_MIN_NO_EMPTY_BINS, MIN_NO_EMPTY_BINS_SET
    if not IS_MIN_NO_EMPTY_BINS:
        STATS[NUM_EMPTY_BINS_NAME].append(len(bins) - len(MIN_NO_EMPTY_BINS_SET))

IS_MIN_TWO_IN_EACH_BIN = False
def min_two_in_each_bin_context(ball_no: np.int64, chosen_bin: np.int64, bins: IntArray):
    global IS_MIN_TWO_IN_EACH_BIN
    if not IS_MIN_TWO_IN_EACH_BIN:
        if np.all(bins >= 2):
            IS_MIN_TWO_IN_EACH_BIN = True
            STATS[MIN_TWO_IN_EACH_BIN_NAME] = ball_no
            STATS[MIN_TWO_MINUS_MIN_NO_EMPTY_NAME] = ball_no - STATS[MIN_NO_EMPTY_BINS_NAME]



def reset(stats: bool = True):
    global IS_FIRST_COLLISION, IS_MIN_NO_EMPTY_BINS, MIN_NO_EMPTY_BINS_SET, IS_MIN_TWO_IN_EACH_BIN
    IS_FIRST_COLLISION = False
    IS_MIN_NO_EMPTY_BINS = False
    MIN_NO_EMPTY_BINS_SET = set()
    IS_MIN_TWO_IN_EACH_BIN = False

    # if stats:
    #     for key in STATS.keys():
    #         STATS[key] = []

def run_experiment(n_bins: int, k: int = 50):
    reset(False)

    for _ in range(k):
        sim_balls_in_bins(
            n_bins, lambda: IS_MIN_TWO_IN_EACH_BIN,
            first_collision_context,
            min_no_empty_bins_context,
            count_empty_bins_context,
            min_two_in_each_bin_context,
        )




def main():
    run_experiment(1000)

if __name__ == '__main__':
    main()

