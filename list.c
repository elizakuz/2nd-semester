/* односвязный список
	Kuzmina Elizaveta */

#include <stdio.h>
#include <stdlib.h>

typedef struct node_t{
	int v;
	struct node_t *next;
}node_t;
node_t *hd = NULL, *p;
void add(){
	int n;
	scanf("%d", &n);
	node_t *p = (node_t *) malloc(sizeof (node_t));
	p->v = n;
	p->next = hd;
	hd = p;	
}
void print(){
	p = hd;
     	for(;p != NULL; p = p->next){
		if(p->next != NULL){
			printf("%d, ", p->v);
		}else {
			printf("%d\n", p->v);
			break;
		}
	}		
}
void del(){
	int n;
	scanf("%d", &n);
	node_t * q;
	for(p = hd; p && (p->v != n);q = p, p = p->next){}
		if(p == NULL){
			printf("no such simbol\n");
		} else {
			if (p == hd) {
				hd = hd->next;
			} else {
				q->next = p->next;
			}
		}	
	free(p);
}			
int main(){
	char k;
	scanf("%c", &k);
	while(k != 'q'){
		//if(k != ' '){
			switch(k) {
				case 'a': add();
					break;
				case 'p': print();
					break;
				case 'd': del();
					break;
				case 'q': break;
				default: printf("no such command\n");
					break;
			}
		//}
		scanf("%c", &k);
	}
}
