#include "tools.h"
using namespaces std;
tools::tools(){}
void tools::runBashCommand(string x){
    const char* command=x.c_str();
    system(x);
}
