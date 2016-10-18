// RESULT OF `remix proba25-ptr.c -ri main::*a`
struct x { int y; };

int main() {
    x a;
    x b;
    int i;
    i = a.y;
    b = a;
    return 0;
}