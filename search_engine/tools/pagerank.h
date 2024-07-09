#ifndef PAGERANK_H
#define PAGERANK_H

#include <unordered_map>
#include <vector>
#include <iostream>
#include <cmath>

using namespace std;

const int NUM_ITERATIONS = 100;
const double DAMPING_FACTOR = 0.85;
const double EPSILON = 1e-6;

class PageRank {
public:
    PageRank(const unordered_map<int, vector<int>>& graph);

    void calculatePageRank();
    void printRanks() const;
    vector<double> getRanks() const;  // Modified function declaration
    vector<int> searchPagesByRank(double minRankThreshold) const;

private:
    unordered_map<int, vector<int>> graph;
    vector<double> ranks;
    int numPages;

    bool hasConverged(const vector<double>& oldRanks, const vector<double>& newRanks) const;
};

#endif // PAGERANK_H
