int printf ( const char * format );
int strcmp ( const char * str1, const char * str2 );
char * gets ( char * str );

int main(void){
    char s[80];

    printf("Enter password: ");
    gets(s);
    if(strcmp(s, "pass")) {
      printf("Invalid Password\n");
      return 0;
    }
    return 1;
}