

typedef int (*fooo[10])(int);
typedef int (*foo)(int);
foo bar;
fooo baar;

int (*f)(int);
struct sss { int i; } vvv, www;

int main() {
  int n = baar[0](3);
}

int main2() {
  ((void ( * ) (void)) 0 ) ( );
  return 0;
}

void g(void (*)(void));

int * sum (int *a, int b) {*a += b; return a;}

char next (char a) { return a+1; }

void something ( int* (*fun1) (int*, int), char (*fun2) (char)) { return; }

int main3 () {
int a = 1;
int b = 2;
char c = 'x';
((void (*) (int * (*) (int*,int), char (* ) (char)) ) ((void*)something))(sum,next);
return 0;
} 
