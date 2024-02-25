#include <iostream>
using namespace std;
int main(){
	int arr[]={1,2,3,4,5,6,7,8};
	int* ptr=arr;
	for(int i=0;i<8;i++){
		cout<<i<<endl;
		cout<<*ptr+i<<endl;
		cout<<(ptr+i)<<endl;
		cout<<*(ptr+i)<<endl;
		cout<<"-------------"<<endl;
	}
}
