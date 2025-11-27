#include <iostream>
#include <random>
#include <vector>
#include <algorithm>
#include <functional>
#include <format>
#include <string>
#include <fstream>
#include <string_view>
#include <filesystem>
#include <thread>

/*
m - # of balls
n - # of bins

Bn : First collision
Un : # empty bins after n balls
Cn : min # balls, so that there are no empty bins
Dn : min # balls, so that there are at least 2 balls in each bin
Dn - Cn : # balls to from 1 to 2 in each bin

Simulate with n [1000, 2000, ..., 100 000]
k = 50 repetitions

*/
constexpr const char* OUTPUT_DIR = "./outputs";
constexpr const char* FILE_NAME_FMT = "./outputs/outputs{}.csv";
constexpr auto SIM_REPETITIONS = 50;

typedef int number_t;
typedef std::vector<number_t> vec_t;
typedef struct stats_t {
    number_t first_collision{0};
    number_t n_empty_bins_after_n{0};
    number_t min_balls_no_empty_bin{0};
    number_t min_balls_each_2_in_bin{0};
    number_t n_balls_from_1_to_2{0};

    static constexpr std::string_view csv_header() {
        return "first_collision;n_empty_bins_after_n;min_balls_no_empty_bin;min_balls_each_2_in_bin;n_balls_from_1_to_2";
    }

    inline std::string to_csv() const {
        return std::format("{};{};{};{};{}",
            first_collision,
            n_empty_bins_after_n,
            min_balls_no_empty_bin,
            min_balls_each_2_in_bin,
            n_balls_from_1_to_2);
    }
} stats_t;

std::ostream& operator<<(std::ostream& out, const stats_t& stats) {
    return out << "First collision: " << stats.first_collision << ", "
            << "Empty bins after n balls: " << stats.n_empty_bins_after_n << ", "
            << "Min balls no empty bin: " << stats.min_balls_no_empty_bin << ", "
            << "Min balls each 2 in bin: " << stats.min_balls_each_2_in_bin << ", "
            << "Balls from 1 to 2 in each bin: " << stats.n_balls_from_1_to_2;
}

stats_t sim_balls_and_bins(number_t n_bins, std::mt19937& local_gen) {
    auto bins = vec_t(n_bins, 0);
    auto stats = stats_t{};
    std::uniform_int_distribution<number_t> dist(0, n_bins);

    // Optimized empty bin count
    auto empty_bins = n_bins;
    auto bins_with_more_than_1 = 0;

    for (number_t ball_no = 1; stats.min_balls_each_2_in_bin == 0; ball_no++) {
        number_t chosen_bin = dist(local_gen);
        bins[chosen_bin]++;
        auto chosen = bins[chosen_bin];

        // This bin was empty before
        if (chosen == 1) {
            empty_bins--;
        }
        // This bin has now more than 1 ball
        if (chosen == 2) {
            bins_with_more_than_1++;
        }

        // U(n) Count non-empty (can be optimized probably)
        if (ball_no == n_bins) {
            stats.n_empty_bins_after_n = empty_bins;
        }

        // B(n) First collision
        if (chosen > 1 && stats.first_collision == 0) {
            stats.first_collision = ball_no;
        }
        // C(n) The moment, when there's no empty balls
        if (empty_bins == 0 && stats.min_balls_no_empty_bin == 0) {
            stats.min_balls_no_empty_bin = ball_no;
        }
        // D(n) In each bin, there are 2 balls, the loop will break after this 
        // condition is met
        if (bins_with_more_than_1 == n_bins) {
            stats.min_balls_each_2_in_bin = ball_no;
            stats.n_balls_from_1_to_2 = ball_no - stats.min_balls_no_empty_bin;
        }
    }

    return stats;
}

void run_experiment(int k, size_t seed) {
    std::fstream file;
    file.open(std::format(FILE_NAME_FMT, k), std::ios::out);

    if (!file.is_open()) {
        std::cerr << "Failed to open file: " << std::format(FILE_NAME_FMT, k) << std::endl;
        return;
    }

    std::mt19937 local_gen(seed);
    file << "n_bins;" << stats_t::csv_header() << "\n";
    for (number_t n_bins = 1000; n_bins <= 100000; n_bins += 1000) {
        // std::cout << "Simulating for n_bins = " << n_bins << "...\n";
        auto stats = sim_balls_and_bins(n_bins, local_gen);
        file << n_bins << ";" << stats.to_csv() << "\n";
    }

    file.close();
}

int main(int argc, char** argv) {
    std::cout << "Creating output directory...\n";
    std::filesystem::create_directory(OUTPUT_DIR);

    std::cout << "Starting simulations...\n";
    std::random_device rd{};
    std::vector<std::thread> threads;
    threads.reserve(SIM_REPETITIONS);
    
    for (int k = 1; k <= SIM_REPETITIONS; k++) {
        auto seed = rd();
        std::cout << "Launching simulation " << k << " (seed=" << seed << ")...\n";
        threads.emplace_back([k, seed]() {
            run_experiment(k, seed);
        });
    }

    size_t progress = 0;
    for (auto& thread : threads) {
        thread.join();
        progress++;
        std::cout << "Progress: " << progress << "/" << threads.size() << " completed.\n";
    }
    std::cout << "Simulations completed. See ./outputs folder\n";
    return 0;
}
