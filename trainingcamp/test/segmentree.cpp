#include <bits/stdc++.h>
using namespace std;
#define p(x) cout<<x<<endl;
struct st{
	st *left, *right;
	int l,r,sum;
	st(int a,int b){
		this->l=a;
		this->r=b;
		sum=0
		if(a!=b){
			int m=(a+b)/2;
			left = new st(a,m);
			right = new st(m+1,b);	
		}
	}
	void update(int pos,int v){
		if (l==r){
			sum=v
		}else{
			int m=(l+r)/2;
			if (pos<=m)left->update(pos,value);
			else rigth->update(pos,value);
			sum = left->sum+ rigth->sum;
		}
	}
	int get(int a,int b){
		if(a==l && b==r) return sum;
		int m=(l+r)/2;
		if 
	}
};
int main(){
  return 0;
}
