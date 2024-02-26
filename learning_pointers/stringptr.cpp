#include <iostream>
using namespace std;
int main(){
	char st[]="hola mundo";
	char *ptr=st;
	for(int i=0;i<sizeof(st);i++){
		cout<<i<<endl;
		cout<<*(st+i)<<endl;
		cout<<"-------------"<<endl;
	}
}
