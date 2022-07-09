#include <bits/stdc++.h>
using namespace std; 
#define p(x) cout<<x<<endl;
int main(){
  int t,contest;
  string name,tmpname;
  int time,tmptime,p,tmpp;
  cin>>t;
  for(int i=0;i<t;i++){
    cin>>contest;
    for(int ii=0;ii<contest;ii++){
      cin>>name>>p>>time;
      
      if(ii==0){
        tmpname=name;
        tmpp=p;
        tmptime=time;
      }
      if(p==tmpp){
        if(tmptime>time){
	        tmpname=name;
	        tmpp=p;
	        tmptime=time;
        }
      }
      if(p>tmpp){
      	  tmpname=name;
	        tmpp=p;
	        tmptime=time;
      }
    }
    p(tmpname)
  }

  return 0;
}
