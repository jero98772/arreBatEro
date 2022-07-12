// The API isBadVersion is defined for you.
// bool isBadVersion(int version);
int binarysearch(int low,int high){
    int mid;
     while(low<high){
        //for(int i=0;i<n;i++){
        mid=low+(high-low)/2; 
        //if (low<high){
         if(isBadVersion(mid)){
             high=mid;
            }
        else{
            low=mid+1;   
        }
        
     }
    return high ;
    
}
class Solution {
public:
    int firstBadVersion(int n) {
        //int bad;
        //cin>>n;
        //cin>>bad;
        int low=0;
        int high=n;
        if (n==1){
            return 1;
        }else{
        //cout<<binarysearch(bad,low,high)<<endl;
            return binarysearch(low,high);
        }

    }
};

