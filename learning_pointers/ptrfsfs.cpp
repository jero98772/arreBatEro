#include <iostream>
using namespace std;
int add2(int x){
	return x+2;
}

void process(int (*func)(int),int num){
	cout<<add2(num);
}
int main(){
	process(add2,6);
}


