#include <bits/stdc++.h>
using namespace std;
int main(){
	int it,x;
	x=0;
	string instruccion;
	cin>>it;
	for(int i=0;i<it;i++){
		cin>>instruccion;
		if(instruccion=="X++" || instruccion=="x++" || instruccion=="++X" || instruccion=="++x"){
			x++;//x=x+1;
		}
		if(instruccion=="X--" || instruccion=="x--" || instruccion=="--X" || instruccion=="--x"){
			x--;//x=x-1;
		}	
	}	
	cout<<x<<endl;
}