import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os


# Configuration
OUTPUT_DIR = "outputs"
SIM_REPETITIONS = 50

def load_simulation_data(sim_index: int) -> pd.DataFrame:
    file_path = os.path.join(OUTPUT_DIR, f"outputs{sim_index}.csv")
    return pd.read_csv(file_path)

# Function to plot a single simulation's results
def plot_simulation_data(sim_data: pd.DataFrame, sim_index: int):
    plt.figure(figsize=(10, 6))
    plt.plot(sim_data['n_bins'], sim_data['max_balls_in_bin'], label='Max Balls in Bin')
    plt.plot(sim_data['n_bins'], sim_data['avg_balls_in_bin'], label='Avg Balls in Bin')
    plt.plot(sim_data['n_bins'], sim_data['stddev_balls_in_bin'], label='Std Dev of Balls in Bin')
    plt.xscale('linear')
    plt.yscale('linear')
    plt.xlabel('Number of Bins (linear scale)')
    plt.ylabel('Number of Balls (linear scale)')
    plt.title(f'Simulation {sim_index} Results')
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
    sim_data = load_simulation_data(1)
    print(sim_data.head())
    print(sim_data.describe())
    print(f"Total rows in simulation 1 data: {len(sim_data)}")
    print(sim_data['n_bins'])

    plot_simulation_data(sim_data, 1)

if __name__ == "__main__":
    main()