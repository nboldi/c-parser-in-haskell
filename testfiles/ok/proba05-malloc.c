int printf ( const char * format );
void exit (int status);
#define NULL 0
typedef unsigned int size_t;
void* malloc (size_t size);

  struct addr {
    char name[40];
    char street[40];
    char zip[10];
  };

  int main()
  {
    struct addr *p;

    p = malloc(sizeof(struct addr));

    if(p==NULL) {
      printf("Allocation Error\n");
      exit(1);
    }
    return p;
  }