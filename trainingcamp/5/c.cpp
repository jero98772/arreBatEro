#include <bits/stdc++.h>
#define D(x) cout << #x << ": " << x << endl;
#define CYN(x) cout << (x ? "YES" : "NO" ) <<endl; 
#define forn(i,n) for(int i=0; i< (int)n; i++)
#define for1(i,n) for(int i=1; i<= (int)n; i++)
#define all(v) v.begin(),v.end()
#define precision(x) cout<<fixed<<setprecision(x);
#define cin_pro  ios::sync_with_stdio(0); cin.tie(NULL); cout.tie(NULL)
#define pb push_back
#define F first
#define S second
#define pf push_front
#define mp make_pair
#define rall(v) v.rbegin(), v.rend()
#define cases(t) while(t--)
#define rforn(i,n) for(int i = n - 1; i >= 0; i--)
#define rfor1(i,n) for(int i = n; i >= 1; i--)
#define foreach(it, v) for(auto it: v)
#define mem(v, val) memset(v, (val), sizeof(v))
#define inf (int) 1e9
#define pi 3.1415926535897932384626433832795
 
#define vi vector<int>
#define pii pair<int,int>
#define vii vector<pii>
#define vvi vector<vi>
#define mpii map<int,int>
#define umpii unordered_map<int,int>
#define seti set<int>
#define pqi priority_queue<int>
 
//Operaciones | Sumatorias | Otros
#define sumn(n) n*(n+1)/2
#define sumevens(n) n*(n+1)
#define sumodds(n) n*n
#define sumsquares(n) (n*(n+1)*(2*n+1))/6
#define sumcubes(n) sumn(n)*sumn(n)
 
int dr[] = {1,-1,0, 0,1,-1,-1, 1};
int dc[] = {0, 0,1,-1,1, 1,-1,-1};
 
using namespace std;
 
typedef long long ll;   
 
/*
DATA STRUCTURES
*/
 
template <typename T> struct SparseTable{
 
 
    private:
	    vector<vector<T>> st;
        T NEUTRO;
	    int size;
	    int H;
        T (*oper)(T&,T&);
 
        template <typename Iter> void readArray(const Iter &begin, const Iter &end){
            size = end - begin;
            H = ceil(log2(size)) + 1;
            st = vector<vector<T>>(H, vector<T>());
 
            forn(_, size) st[0].pb(*( begin + _ ));
            for1(_, H) st[_] = vector<T>(size, NEUTRO);
 
        }
	
    public:
        template <typename Container> SparseTable(const Container &data, const T NEUTRO, T oper(T&, T&)){
            this->oper = oper;
            this->NEUTRO = NEUTRO;
 
            readArray(begin(data), end(data));
            build();
        }
        
        void build(){
            for(int i = 1; i < H; i++){
                for(int j = 0; j + (1 << i ) <= size ; j++){
                    if(st[i][j] == NEUTRO){
                        st[i][j] = oper( st[i-1][j] , st[i-1][j + (1 << (i - 1))] );
                    }
                }
            }
        }
        
        T query(int l, int r){
            T ans = 0;
            
            int j = log2(r-l+1);
            
            ans = oper( st[j][l] , st[j][r - (1<<j) + 1] );
            
            return ans;
            
        }
		
};
 
template <typename T> struct SegmentTree{
    private:
        vector<T> st;
        int size;
        int NEUTRO;
        T (*oper)(T&,T&);
 
        template <typename Iter> void readArray(const Iter &begin, const Iter &end){
            size = end - begin;
 
            forn(_, size) st.pb(NEUTRO);
 
            forn(_, size) st.pb(*( begin + _ ));
        }
    
        void build(){
            for(int p = size - 1; p > 0; p--){
                st[p] = oper( st[p << 1] , st[p << 1 | 1]);
            }
        }
 
    public:
        template <typename Container> SegmentTree(const Container &data, const T &NEUTRO, T oper(T&, T&)){
            st = vector<T>();
            this->NEUTRO = NEUTRO;
            this->oper = oper;
 
            readArray(begin(data), end(data));
            build();
        }
 
        T query(int l, int r){
            T ans = NEUTRO;
 
            for(l += size, r += size; l < r; l >>= 1, r >>= 1){
                if(l & 1) ans = oper(ans, st[l++]);
                if(r & 1) ans = oper(ans, st[--r]);
            }
 
            return ans;
        }
 
        void update(int p, T val){
            for(st[p += size] = val; p > 1; p >>= 1) st[p >> 1] = oper(st[p], st[p ^ 1]);
        }
    
};
 
/*
UTILITIES
*/
 
/*
 
FUNCTIONS
 
*/
 
template <typename T> void amax(T &a, const T &b){ if( a < b) a = b; }
template <typename T> void amin(T &a, const T &b){ if( b < a) a = b; }
template <typename T> T gcd(T a, T b){
        if (!a || !b)
            return a | b;
        unsigned shift = __builtin_ctz(a | b);
        a >>= __builtin_ctz(a);
        do {
            b >>= __builtin_ctz(b);
            if (a > b)
                swap(a, b);
                b -= a;
        } while (b);
        return a << shift;
}
 
template <typename T> inline void prefix_sum(T arr, T& res){ 
    if(arr.size() > 0) res[0]=arr[0];
    for(int i = 1; i < arr.size(); i++) res[i] += res[i-1] + arr[i]; 
}
 
template <typename T> inline void sufix_sum(T arr, T& res){ 
    if(arr.size() > 0) res[arr.size()-1]=arr[arr.size()-1];
    for(int i = arr.size()-2; i >= 0; i--) res[i] += res[i+1] + arr[i]; 
}
 
void read_fast(){
    cin_pro;
    //#ifdef ONLINE_JUDGE
    #ifdef LOCAL
        freopen("input.txt", "r", stdin);
    #else
    #define endl '\n'
    #endif
}
 
/*
    END OF TEMPLATE
*/
 
int oper(int &a, int &b){ return a + b; }
 
bool isPalindrome(string &copia){
	return copia[0] == copia[4] && copia[1] == copia[3];
}
 
void applyFormat(string &copia, int hh, int mm){
	copia = (hh < 10 ? '0' + to_string(hh) : to_string(hh)) + ":" + (mm < 10 ? '0' + to_string(mm) : to_string(mm));
}
 
void updateTime(string &copia, int x){
	int hh = stoi(copia.substr(0,2));
	int mm = stoi(copia.substr(3));
	
	hh = (hh + (mm+x)/60)%24;
	mm = (mm+x)%60;
	
	applyFormat(copia, hh, mm);
}
 
void solve(){
	string hora; int x; cin>>hora>>x;
	string copia = hora;
	//
	int ans = 0;
 
	if(isPalindrome(copia)) ans++;
	updateTime(copia, x);
		
	while(hora != copia){
		if(isPalindrome(copia)){
		 ans++;	
			//cout<<copia<<endl<<endl;
		}
		cout<<copia<<endl;
		updateTime(copia, x);
	}
 
	
	cout<<ans<<endl;
}
 
int main(){
    read_fast();
    int t;cin>>t;
    cases(t){
        solve();
    }
    return 0;
}
//Compile: 
//g++ -std=c++14 -o template template.cpp
//How to run the file in any terminal
//./template
//How pass the input in the file
// Input data
//./template < input.txt 
// Input data and generate output file
//./template < input.txt > output.txt