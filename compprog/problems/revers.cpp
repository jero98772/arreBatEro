//https://codeforces.com/problemset/problem/999/B
#include <bits/stdc++.h>
using namespace std;
int main(){
	int size;
	string word; 
	cin>>size;
	cin>>word;
	string tmp;
	tmp="?";
	for(int i=size;size>0;i++){
		//cout<<word[i]<<endl;
		string tmp=tmp+word[i];
	}
	cout<<tmp<<endl;
}