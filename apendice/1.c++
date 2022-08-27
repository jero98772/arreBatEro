#include <bits/stdc++.h>
using namespace std;
int main(){
	bool al=true;
	int s;cin>>s;
	char mat[s][s];
	for(int i=0;i<s;i++){
		for(int ii=0;ii<s;ii++){
			cin>>mat[i][ii];
		}
	}
	for(int i=0;i<s;i++){
		for(int ii=0;ii<s;ii++){
			if(0<i-1 && 0<ii-1 && s>i+1 && s>ii+1){
				if(mat[i-1][ii-1]=="0" && mat[i+1][ii+1]=="0" && mat[i][ii]=="0"){
					cout<<"YES"<<endl;
					al=false;
					break;
				}
				else if(mat[i-1][ii+1]=="0" && mat[i+1][ii-1]=="0" && mat[i][ii]=="O"){
					cout<<"YES"<<endl;
					al=false;					
					break;
				}
		
			}
		}
	if(al){
				cout<<"NO"<<endl;
	}
}