int printf ( const char * format );
char * gets ( char * str );
typedef unsigned int size_t;
size_t strlen ( const char * str );

void bubble(char *items, int count);

int main(void)
{
  char s[255];

  printf("Enter a string:");
  gets(s);
  bubble(s, strlen(s));
  printf("The sorted string is: %s.\n", s);

  return 0;
}


void bubble(char *items, int count)
{
  register int i, j;
  register char t;

  for(i = 1; i < count; ++i)
    for( j = count-1; j >= i; --j) {
      if(items[j - 1] > items[ j ]) {
        /* exchange elements */
        t = items[j - 1];
        items[j - 1] = items[ j ];
        items[ j ] = t;
      }
    }
}