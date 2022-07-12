#include <bits/stdc++.h>
using namespace std;
int main(){
    //int search(vector<int>& nums, int target) {
    int sizearr,tmp,target;
    cin>>sizearr;
    vector<int> nums;
    for(int i=0;i<sizearr;i++){
    	cin>>tmp;
    	nums.push_back(tmp);
    }
    cin>>target;
    
    int low=0;
    int high=nums.size();
    int mid;
    for(int i=0;i<nums.size();i++){
        mid=low+(high-low)/2;
        //cout<<endl<<i<<endl<<mid<<endl<<low<<high<<endl;
        if(nums[mid]==target){
            //return i;
            cout<<mid<<endl;
        }
        else if(target>nums[mid]){
            low=mid+1;
        }
        else if(target<nums[mid]){
            high=mid-1;
        }
        else{
            //return -1;
            cout<<-1<<endl;
        }
    }       
}
/*
class Solution {
public:
    int search(vector<int>& nums, int target) {
    int low=0;
    int high=nums.size();
    int mid;
    for(int i=0;i<nums.size();i++){
        mid=low+(high-low)/2;
        cout<<endl<<i<<endl<<mid<<endl<<low<<high<<endl;
        if(nums[mid]==target){
            return i;
            //cout<<i;
        }
        else if(target>nums[mid]){
            low=mid+1;
        }
        else if(target<nums[mid]){
            high=mid-1;
        }
        else{
            return -1;
            //cout<<-1;
        }
    }
    }    
};*/