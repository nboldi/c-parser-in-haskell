
typedef int a;

void f(int a, int b) {
    a *b; // multiplication here
}

void g() {
    a *b; // declaration here
}

void h(int a) {
    //a *b; // would be multiplication, ERROR: b is not declared
}

void k(int b) {
    //a *b; // would be declaration, ERROR: declaration shadows a parameter
}

int main() {
    f(0,0); g(); h(0);
}