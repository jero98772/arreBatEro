#include "pagerank.h"

PageRank::PageRank(const unordered_map<int, vector<int>>& graph) : graph(graph) {
    numPages = graph.size();
    ranks.resize(numPages, 1.0 / numPages);
}

void PageRank::calculatePageRank() {
    for (int i = 0; i < NUM_ITERATIONS; ++i) {
        vector<double> newRanks(numPages, (1.0 - DAMPING_FACTOR) / numPages);

        for (const auto& node : graph) {
            int page = node.first;
            const vector<int>& links = node.second;
            double rankShare = ranks[page] / links.size();
            for (int linkedPage : links) {
                newRanks[linkedPage] += DAMPING_FACTOR * rankShare;
            }
        }

        if (hasConverged(ranks, newRanks)) {
            break;
        }

        ranks = newRanks;
    }
}

void PageRank::printRanks() const {
    for (size_t i = 0; i < ranks.size(); ++i) {
        cout << "Page " << i << ": " << ranks[i] << endl;
    }
}

vector<double> PageRank::getRanks() const {
    return ranks;
}
vector<int> PageRank::searchPagesByRank(double minRankThreshold) const {
    vector<int> result;
    for (size_t i = 0; i < ranks.size(); ++i) {
        if (ranks[i] >= minRankThreshold) {
            result.push_back(i);
        }
    }
    return result;
}

bool PageRank::hasConverged(const vector<double>& oldRanks, const vector<double>& newRanks) const {
    for (size_t i = 0; i < oldRanks.size(); ++i) {
        if (abs(oldRanks[i] - newRanks[i]) > EPSILON) {
            return false;
        }
    }
    return true;
}
