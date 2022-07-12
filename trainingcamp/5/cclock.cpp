#include <bits/stdc++.h>
using namespace std;

void applayformat(string& time,int h,int m){
	string tmpm,tmph;
	if(m<10){
		tmpm="0"+to_string(m);
	}else{
		tmpm=to_string(m);
	}
	if(h<10){
		tmph="0"+to_string(h);
	}else{
		tmph=to_string(h);
	}
	time=tmph+":"+tmpm;
}

bool ispalindrome(string time){
	if(time[0]==time[4] && time[1]==time[3]){
		return true;
	}
	else{
		return false;
	}
}

void timeupdate(string& txt,int minutes){
	  int h=stoi(txt.substr(0,2));
  	int m=stoi(txt.substr(3));
  	h=(h+(m+minutes)/60)%24;
  	m=(m+minutes)%60;
  	applayformat(txt,h,m);
}

int main(){
  int testcases,minutes,m,h;
  int pals;
  string time,timef;
  cin>>testcases;
  for(int i=0;i<testcases;i++){
  	pals=0;
  	cin>>time>>minutes;
  	
  	if(ispalindrome(timef))pals++;
  	timeupdate(timef,minutes);  
  		
	  while(time!=timef){
	  	if(ispalindrome(time)){
  			pals+=1;
  		}
  		timeupdate(timef,minutes);
  	}
  	cout<<pals<<endl;
  }
	return 0;
}