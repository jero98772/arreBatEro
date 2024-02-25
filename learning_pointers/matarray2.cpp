#include <iostream>
using namespace std;
int main(){
	int arr[3][3]= {{1,4,7},{2,5,8},{3,6,9}};//int* ptr=arr;
	for(int i=0;i<3;i++){
		for(int j=0;j<3;j++){
		cout<<i<<endl;
		int* ptr= &arr[0][0]+i*3+j;
	        cout<<*ptr<<endl;
			cout<<""<<endl;
		}
	}
}
