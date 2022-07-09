#include <bits/stdc++.h>
using namespace std;
int main(){
  int rows;
  cin>>rows;
	string bus[5][rows];
  for(int i=0;i<=rows;i++){
  	for(int j=0;j<5;j++){
  		cin>>bus[j][i];
  	}
  }
  if (true){
  	for(int i=0;i<=rows;i++){
  		for(int j=0;j<5;j++){
  			cout<<bus[j][i]<<endl;
  		}
  	}
  }
	return 0;
}