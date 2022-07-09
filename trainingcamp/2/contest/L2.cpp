#include <bits/stdc++.h>
using namespace std;
bool istprime(int n){
  //float root=sqrt(n);
  int cast=(int)(root);
  cout<<cast<<endl;
  cout<<root<<endl;
    if(n%cast==0){
       return true;
    }else{
     return false;
   }
}
int main(){
  int times,num;
  cin>>times;
  int nums[times];
  for(int i=0;i<times;i++){
   cin>>num;
   if(istprime(num)){
      cout<<"YES"<<endl;
    }
    else{
      cout<<"NO"<<endl;
    }
  }
  return 0;
}
