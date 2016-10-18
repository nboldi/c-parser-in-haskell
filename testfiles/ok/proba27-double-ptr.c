struct x { int y; };

int main() {
    x **a;
    x b;
    int i;
    i = (*a)->y;
    b = **a;
    return 0;
}