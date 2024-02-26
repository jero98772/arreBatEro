#include <iostream>
using namespace std;
int main(){
	int** ptrx2;
	int* ptr = new int;
	//int * ptr=(int*)malloc(sizeof(int));
	ptrx2=&ptr;
	**ptrx2=42;
	cout<<**ptrx2;
	return 0;
}
