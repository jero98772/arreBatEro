#include <bits/stdc++.h>
using namespace std;
bool checkpal(int n){
  string num=to_string(n);
  /*int pointer1=0;
  int pointer2=num.length();
  bool alert=false;
   for(int i=0;i<num.length();i++){
    if(num[pointer1]==num[pointer2]){
      alert=true;
    }else{
      return false;
    }
  }*/
  string num2=num;
  reverse(begin(num2),end(num2));
  //cout<<num<<num2;
  if(num2==num){
    return true;
  }
  else{
    return false;
  }
  //return alert;
}
int nextpal(int n){
  for(int i=n+1;i<n*n;i++){
    if(checkpal(i)){
      return i;
    }
  }
}
int main(){
  int num,times;
  cin>>times;
  for(int i=0;i<times;i++){
    cin>>num;
    cout<<nextpal(num+1)<<endl;
  }
  return 0;
}
