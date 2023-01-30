#ifndef TOOLS_H
#define TOOLS_H
#include <string>

using namespace std;
class tools{
     public:
        tools();
        void openFolder(string path);
        void openFile(string name);
        void runBashCommand(string x);
    private:
        string path;
        string fileopen;
};
/*read file
 * save file
 * save as
 * tree
 * bash comand run

*/
#endif
