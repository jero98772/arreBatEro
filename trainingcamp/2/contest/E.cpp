#include <bits/stdc++.h>
using namespace std;
int main(){
		int s;
		string word,pok;
		cin>>s;
		cin>>word;
		map<string,int> pokesd;
		pokesd["vaporeon"]=0;
		pokesd["jolteon"]=0;
		pokesd["flareon"]=0;
		pokesd["espeon"]=0;
		pokesd["umbreon"]=0;
		pokesd["leafeon"]=0;
		pokesd["glaceon"]=0;
		pokesd["sylveon"]=0;
		
    vector<string> words ={"vaporeon", "jolteon", "flareon", "espeon", "umbreon", "leafeon", "glaceon", "sylveon"};
    for(int i=0;i<words.size();i++){
    	if(s==words[i].length()){
    		pok=words[i];
    		for(int ii=0;ii<words[i].length();i++){
    				if(word[ii]==words[i][ii]){
    					pokesd[words[i]]=pokesd[words[i]]+1;
    				}
    				if(ii>=2){
    					pok="";
    				}
    			}
    		}
    	}
    
    //map <string,float> :: iterator iter;

}
