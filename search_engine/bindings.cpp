#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include "pagerank.h"

namespace py = pybind11;

PYBIND11_MODULE(pagerank, m) {
    py::class_<PageRank>(m, "PageRank")
        .def(py::init<const std::unordered_map<int, std::vector<int>>&>())
        .def("calculatePageRank", &PageRank::calculatePageRank)
        .def("printRanks", &PageRank::printRanks)
        .def("getRanks", &PageRank::getRanks)
        .def("searchPagesByRank", &PageRank::searchPagesByRank);  

}
