// RESULT OF `remix proba24-array.c -ii main::a[]`
int main() {
    int *a[10];
    int b;
    b = (*a[0]);
    (*a[0] )= b;
    return 0;
}