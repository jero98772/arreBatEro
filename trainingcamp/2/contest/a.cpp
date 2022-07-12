#include <bits/stdc++.h>
#include <cctype>

using namespace std;
int main(){
   int s;
   int counter=0;
   string word;
   string alph="abcdefghijklmnopqrstuvwxyz";
   cin>>s;
   cin>>word;
   //word=
   transform(word.begin(),word.end(),word.end(), tolower());
   for(int i=0;i<s;i++){
   		alph.erase(alph.begin()+alph.find(word[i]));
   }
   if(alph.size()==counter){
   		cout<<"YES"<<endl;
   }
	 else{
	 		cout<<"NO"<<endl;
	 }
}