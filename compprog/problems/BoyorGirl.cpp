#include <bits/stdc++.h>
using namespace std;

int main(){
	string txt="";
	string t;cin>>t;
	int count=0;
	for(int i=0;i<t.size();i++){
		if(txt.find(t[i])==string::npos){
			txt=txt+t[i];
			count++;
		}
	}
	//cout<<count<<endl;
	if(count%2==0){
		cout<<"CHAT WITH HER!"<<endl;
	}
	else{
		cout<<"IGNORE HIM!"<<endl;	
	}		
}