#include <bits/stdc++.h>
#include <limits>
using namespace std;
vector<float> dp(10000000,-1);
float portions(float A, float B, float a,float b,int  n){
	//if(dp[n]!=-1){
	//	return dp[n];
	//}
	if (A/B ==a/b){
		return n;
	}
	else{
		if (a > A || b > B){
            return  std::numeric_limits<double>::infinity();;
			}
        return min(portions(A, B, a + b + a, b, n + 1), portions(A, B, a, a + b + b, n + 1));
	}
}
int main(){
	int a,b,i,j;
	float s;
	int t;cin>>t;
	for(int i=0;i<t;i++){
		cin>>a>>b>>i>>j;
		s=portions(a, b, 1, 1, 2);
		if (s== std::numeric_limits<double>::infinity()){
			s=-1;
		}
		cout<<s<<endl;
	}
}

