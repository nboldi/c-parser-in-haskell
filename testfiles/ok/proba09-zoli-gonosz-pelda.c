int * sum (int *a, int b) {*a += b; return a;}

char next (char a) { return a+1; }

void something ( int* (*fun1) (int*, int), char (*fun2) (char)) { return; }

int main () {
  int a = 1;
  int b = 2;
  char c = 'x';
  typedef int* (*fst)(int*,int);
  void *pt, *pt2, *pt3;
  fst funcs[4];
  funcs[0] = sum;
  a = * funcs[3](&a,b);
  a = * 3[funcs](&a,b);
  a = * (*(funcs+3))(&a,b);
  a = * (*(funcs+3))(&a,b);
  pt = (void*)(funcs+2);
  a = * ((int* (*)(int*,int))pt)(&a,b);
  a = * (*(int* (*)(int*,int))&pt)(&a,b);
  pt2 = (void*)&pt;
  pt3 = (void*)&pt2;
  a = * (***(***(int* (*)(int*,int))pt3))(&a,b);
  ((void (*) (int *(*) (int*,int), char (*) (char)) ) ((void*)something))(sum,next);
  return 0;
}
