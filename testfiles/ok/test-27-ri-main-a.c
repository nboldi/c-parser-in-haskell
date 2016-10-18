// RESULT OF `remix proba27-double-ptr.c -ri main::**a`
struct x { int y; };

int main() {
    x *a;
    x b;
    int i;
    i = (*a).y;
    b = *a;
    return 0;
}