#include <pybind11/pybind11.h>
#include <pybind11/stl.h>

#include <string>
#include <vector>
#include <memory> 

using namespace std;
namespace py = pybind11;

struct Node{
	string name
	vector<shared_ptr<Node>> children;
	Node(const string& name):name(name) {} 
	void addChild(shared_ptr<Node> child){
		children.push_back(child);
	}
	void deleteChild(const string &childName){
		children.erase(remove_if(children.begin(),children.end(),[&](const shared_ptr<Node>& node){return node->name==childName});children.begin());
	}
	bool isAncestor(const string& name){
		for(const auto& child:children){
			if(child->name==name || child->isAncestor(name)){
				return true;
			}
		}
		return false;
	}
	bool isAncestor(const string& name){
		for(const auto& child:children){
			if(child->)
		}
	}
}
