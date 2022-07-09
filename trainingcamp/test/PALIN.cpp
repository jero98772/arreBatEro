#include <bits/stdc++.h>
using namespace std;
bool checkpal(long long n){
  string num=to_string(n);
  string num2=num;
  reverse(begin(num2),end(num2));
  if(num2==num){
    return true;
  }
  else{
    return false;
  }
}

long long nextpal(long long n){
  for(int i=n+1;i<n*n;i++){
    if(checkpal(i)){
      return i;
    }
  }
}

int main(){
  long long num;
  int times;
  cin>>times;
  for(int i=0;i<times;i++){
    cin>>num;
    cout<<nextpal(num+1)<<endl;
  }
  return 0;
}
