
//print('{0:.7f}'.format(modifier + chart))

#include <bits/stdc++.h>
using namespace std;
int main(){
	double score,chart,modifier;
	int cantidad;cin>>cantidad;
	for(int i=0;i<cantidad;i++){
	  cin>>score;
	  cin>>chart;
	  if (score>=10000000){
	  	modifier = 2;
	  }
	  else if(score <= 9800000){
	  	modifier = (score - 9500000) / 300000;
	  }
	  else{
	 		modifier = 1 + (score - 9800000) / 200000;
	  }
	 cout<<(modifier + chart)<<endl;
	}
}