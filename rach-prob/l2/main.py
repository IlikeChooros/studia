import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os


# Configuration
OUTPUT_DIR = "outputs"
SIM_REPETITIONS = 50

def load_simulation_data(sim_index: int) -> pd.DataFrame:
    file_path = os.path.join(OUTPUT_DIR, f"outputs{sim_index}.csv")
    return pd.read_csv(file_path, sep=';', dtype={'n_bins': int, 'first_collision': int, 'n_empty_bins_after_n': int,
                                                  'min_balls_no_empty_bin': int, 'min_balls_each_2_in_bin': int,
                                                  'n_balls_from_1_to_2': int})

# Function to plot a single simulation's results
def plot_simulation_data(sim_data: pd.DataFrame, sim_index: int):
    plt.figure(figsize=(10, 6))
    ns = sim_data['n_bins']
    plt.plot(ns, sim_data['first_collision'], label='First Collision', marker='o')
    plt.plot(ns, sim_data['n_empty_bins_after_n'], label='Empty Bins After N', marker='o')
    plt.plot(ns, sim_data['min_balls_no_empty_bin'], label='Min Balls No Empty Bin', marker='o')
    plt.plot(ns, sim_data['min_balls_each_2_in_bin'], label='Min Balls Each 2 In Bin', marker='o')
    plt.plot(ns, sim_data['n_balls_from_1_to_2'], label='Balls From 1 To 2', marker='o')

    # plt.xscale('linear')
    # plt.yscale('linear')
    plt.xlabel('Number of Bins (linear scale)')
    plt.ylabel('Number of Balls (linear scale)')
    plt.title(f'Simulation {sim_index} Results')
    plt.legend()
    plt.grid(True)
    plt.show()

def plot_empty_bins(aggregated_data: pd.DataFrame):
    plt.figure(figsize=(10, 6))
    ns = aggregated_data['n_bins']
    mean_empty_bins = aggregated_data.groupby('n_bins')['n_empty_bins_after_n'].mean()
    plt.plot(ns.unique(), mean_empty_bins, label='Mean Empty Bins After N', marker='o')

    plt.xscale('linear')
    plt.yscale('linear')
    plt.xlabel('Number of Bins (linear scale)')
    plt.ylabel('Mean Number of Empty Bins (linear scale)')
    plt.title('Mean Empty Bins After N Across Simulations')
    plt.legend()
    plt.grid(True)
    plt.show()

def plot_min_balls_no_empty_bin(aggregated_data: pd.DataFrame):
    plt.figure(figsize=(10, 6))
    ns = aggregated_data['n_bins']
    mean_min_balls_no_empty = aggregated_data.groupby('n_bins')['min_balls_no_empty_bin'].mean()
    plt.plot(ns.unique(), mean_min_balls_no_empty, label='Mean Min Balls No Empty Bin', marker='o')

    plt.xscale('linear')
    plt.yscale('linear')
    plt.xlabel('Number of Bins (linear scale)')
    plt.ylabel('Mean Min Balls No Empty Bin (linear scale)')
    plt.title('Mean Min Balls No Empty Bin Across Simulations')
    plt.legend()
    plt.grid(True)
    plt.show()

def plot_min_balls_each_2_in_bin(aggregated_data: pd.DataFrame):
    plt.figure(figsize=(10, 6))
    ns = aggregated_data['n_bins']
    mean_min_balls_each_2 = aggregated_data.groupby('n_bins')['min_balls_each_2_in_bin'].mean()
    plt.plot(ns.unique(), mean_min_balls_each_2, label='Mean Min Balls Each 2 In Bin', marker='o')

    plt.xscale('linear')
    plt.yscale('linear')
    plt.xlabel('Number of Bins (linear scale)')
    plt.ylabel('Mean Min Balls Each 2 In Bin (linear scale)')
    plt.title('Mean Min Balls Each 2 In Bin Across Simulations')
    plt.legend()
    plt.grid(True)
    plt.show()

def plot_n_balls_from_1_to_2(aggregated_data: pd.DataFrame):
    plt.figure(figsize=(10, 6))
    ns = aggregated_data['n_bins']
    mean_n_balls_from_1_to_2 = aggregated_data.groupby('n_bins')['n_balls_from_1_to_2'].mean()
    plt.plot(ns.unique(), mean_n_balls_from_1_to_2, label='Mean Balls From 1 To 2', marker='o')

    plt.xscale('linear')
    plt.yscale('linear')
    plt.xlabel('Number of Bins (linear scale)')
    plt.ylabel('Mean Balls From 1 To 2 (linear scale)')
    plt.title('Mean Balls From 1 To 2 Across Simulations')
    plt.legend()
    plt.grid(True)
    plt.show()

def plot_first_collision(aggregated_data: pd.DataFrame):
    plt.figure(figsize=(10, 6))
    ns = aggregated_data['n_bins']
    mean_first_collision = aggregated_data.groupby('n_bins')['first_collision'].mean()
    plt.plot(ns.unique(), mean_first_collision, label='Mean First Collision', marker='o')

    plt.xscale('linear')
    plt.yscale('linear')
    plt.xlabel('Number of Bins (linear scale)')
    plt.ylabel('Mean Number of Balls for First Collision (linear scale)')
    plt.title('Mean First Collision Across Simulations')
    plt.legend()
    plt.grid(True)
    plt.show()

def aggregate_simulation_data() -> pd.DataFrame:
    all_data = []
    for k in range(1, SIM_REPETITIONS + 1):
        sim_data = load_simulation_data(k)
        all_data.append(sim_data)
    return pd.concat(all_data, ignore_index=True)


def main():
    if not os.path.exists(OUTPUT_DIR):
        print(f"Output directory '{OUTPUT_DIR}' does not exist. Please run the C++ simulations first.")
        return
    
    # For now, just plot the first simulation
    # sim_data = load_simulation_data(1)
    # plot_simulation_data(sim_data, 1)

    aggregated_data = aggregate_simulation_data()
    plot_first_collision(aggregated_data)
    # plot_empty_bins(aggregated_data)
    # plot_min_balls_no_empty_bin(aggregated_data)
    # plot_min_balls_each_2_in_bin(aggregated_data)
    # plot_n_balls_from_1_to_2(aggregated_data)


if __name__ == "__main__":
    main()