#include <iostream>
using namespace std;
void incre(int *ptr){
	(*ptr)++;
}
int main(){
  int num =10;
  incre(&num);
  cout<<num<<endl;
}
