#include <bits/stdc++.h>
using namespace std;
bool istprime(int n){
  int divs=0;
  for(int i=1;i<=n;i++){
    if(n%i==0){
      divs+=1;
     }
   }
   if(divs==3){
       return true;
   }else if(divs>3){
     return false;
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
