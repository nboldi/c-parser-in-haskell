int printf ( const char * format );

#define FAIL_IF(condition, error_code) if (condition) return (error_code)

#ifdef SLRE_DEBUG
#define DBG(x) printf x
#else
#define DBG(x)
#endif

int f(int);
int a;
int b;

int foo() {
      DBG((""));
      FAIL_IF('(', 0);
      FAIL_IF(f((char) b),a);
}

int slre_match() {
  DBG((""));
}