struct FILE {};
#define NULL 0
int printf ( const char * format );
typedef unsigned int size_t;
size_t fwrite ( const void * ptr, size_t size, size_t count, FILE * stream );
int fclose ( FILE * stream );
int feof ( FILE * stream );
size_t fread ( void * ptr, size_t size, size_t count, FILE * stream );
void exit(int);
FILE * fopen ( const char * filename, const char * mode );

  int main(void)
  {
    FILE *fp;
    float bal[5];
    int i;
    
    bal[0] = 1.1L;
    bal[1] = 2.2L;
    bal[2] = 3.3L; 
    bal[3] = 4.4L;
    bal[5] = 5.5L;

    if((fp=fopen("test", "wb"))==NULL) {
      printf("Cannot open file.\n");
      exit(1);
    }

    if(fwrite(bal, sizeof(float), 5, fp) != 5)
      printf("File read error.");
      fclose(fp);

    if((fp=fopen("test", "rb"))==NULL) {
      printf("Cannot open file.\n");
      exit(1);
    }

    if(fread(bal, sizeof(float), 5, fp) != 5) {
      if(feof(fp)) {
         printf("Premature end of file.");
      }else {
         printf("File read error.");
      }
    }
    fclose(fp);

    for(i=0; i<5; i++){
      printf("%f ", bal[i]);
    }
    return 0;
  }

