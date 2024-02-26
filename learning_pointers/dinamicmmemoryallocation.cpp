#include <iostream>
using namespace std;
int main(){
	int *ptr = (int* )malloc(sizeof(int));//maybe error by *) 
	if (ptr!=NULL){
		*ptr=100;
		cout<<*ptr;
		free(ptr);
	}else{
		cout<<"fail memory allocation";
	}
}
