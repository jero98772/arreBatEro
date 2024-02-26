#include <iostream>
using namespace std;
void printarr(int *ptr,int size){
	for(int i=0;i<size;i++){
		cout<<*(ptr+i)<<endl;
	}

}

int main(){
	int arr[]={1,3,5,7,9};
	int size=sizeof(arr)/sizeof(arr[0]);
	cout<<"size: "<<size<<endl;
	printarr(arr,size);
	return 0;
}
