#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <string>
#include <vector>
#include <memory>

namespace py = pybind11;

// Define the Node structure
struct Node {
    std::string name;
    std::vector<std::shared_ptr<Node>> children;

    Node(const std::string& name) : name(name) {}

    // Method to add a child
    void addChild(std::shared_ptr<Node> child) {
        children.push_back(child);
    }

    // Method to delete a child
    void deleteChild(const std::string& childName) {
        children.erase(
            std::remove_if(children.begin(), children.end(),
                [&](const std::shared_ptr<Node>& node) {
                    return node->name == childName;
                }),
            children.end());
    }

    // Method to check if a node is an ancestor
    bool isAncestor(const std::string& ancestorName) const {
        for (const auto& child : children) {
            if (child->name == ancestorName || child->isAncestor(ancestorName)) {
                return true;
            }
        }
        return false;
    }

    // Method to check if a node is a direct child
    bool isSon(const std::string& sonName) const {
        for (const auto& child : children) {
            if (child->name == sonName) {
                return true;
            }
        }
        return false;
    }
};

// Define the n-tree class
class NTree {
private:
    std::shared_ptr<Node> root;

public:
    NTree() : root(nullptr) {}

    // Method to add a node to the tree
    void addNode(const std::string& parentName, const std::string& nodeName) {
        auto newNode = std::make_shared<Node>(nodeName);
        if (!root) {
            root = newNode;
        } else {
            addChildToNode(root, parentName, newNode);
        }
    }

    // Recursive function to add a child to a specific node
    void addChildToNode(std::shared_ptr<Node>& currentNode, const std::string& parentName, std::shared_ptr<Node>& newNode) {
        if (currentNode->name == parentName) {
            currentNode->addChild(newNode);
        } else {
            for (auto& child : currentNode->children) {
                addChildToNode(child, parentName, newNode);
            }
        }
    }

    // Method to delete a node from the tree
    void deleteNode(const std::string& nodeName) {
        if (root && root->name == nodeName) {
            root = nullptr; // delete root if it matches
        } else {
            deleteChildFromNode(root, nodeName);
        }
    }

    // Recursive function to delete a child from a specific node
    void deleteChildFromNode(std::shared_ptr<Node>& currentNode, const std::string& nodeName) {
        currentNode->deleteChild(nodeName);
        for (auto& child : currentNode->children) {
            deleteChildFromNode(child, nodeName);
        }
    }

    // Method to check if a node is an ancestor in the tree
    bool isAncestor(const std::string& ancestorName) const {
        return root->isAncestor(ancestorName);
    }

    // Method to check if a node is a direct child in the tree
    bool isSon(const std::string& sonName) const {
        return root->isSon(sonName);
    }
};

// Binding code using pybind11
PYBIND11_MODULE(ntree, m) {
    py::class_<NTree>(m, "NTree")
        .def(py::init<>())
        .def("add_node", &NTree::addNode, "Add a node to the n-tree")
        .def("delete_node", &NTree::deleteNode, "Delete a node from the n-tree")
        .def("is_ancestor", &NTree::isAncestor, "Check if a node is an ancestor in the n-tree")
        .def("is_son", &NTree::isSon, "Check if a node is a son in the n-tree");
}
