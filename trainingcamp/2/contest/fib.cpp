#include <bits/stdc++.h>
//#define ll long long
using namespace std;
const int MAXN =100000+10;
int dp[MAXN];

int fib(int n){
  if(dp[n]!=-1){
    return dp[n];
  }
  int ans;
  if(n==0 || n==1){
    ans=1;
  }else{
    ans=fib(n-1)+fib(n-2);
  }
  dp[n]=ans;
  return ans;
}
int dpfib(int n){
   dp[0]=0;
   dp[1]=1;
   for(int i=2;i<=n;i++){
     dp[i]=dp[i-1]+dp[i-2];
   }
   return dp[n];
}

int main(){
  int n;
  cin>>n;
  for(int i=0;i<=n;i++){
    dp[i]=-1;
  }
  cout<<dpfib(n)<<endl;
  cout<<fib(n)<<endl;
  return 0;
}

