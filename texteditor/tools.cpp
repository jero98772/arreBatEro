#include "tools.h"
using namespace std;
tools::tools(){}
 static void tools::runBashCommand(string x){
    const char* command=x.c_str();
    system(x);
}
