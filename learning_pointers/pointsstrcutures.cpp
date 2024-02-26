#include <iostream>
#include <cstring>
using namespace std;

struct node{
	char id[50];
	int pos;
};
int main(){
	struct node *ptr=(struct node *)malloc(sizeof(struct node));
	strcpy(ptr->id,"pepepe");
	ptr->pos=1;
	cout<<ptr->id<<endl;
	cout<<ptr->pos<<endl;
	free(ptr);
}
