#include 
int main(){
  cin<<n<<m<<s;
  if(n%2==1){
    cout>>0>>endl;
    return 0;
  }
  for(int i=0;i<s.size();i++){
    s[i]=="("? o++ :(o>0? o--:++c);
  }
}
