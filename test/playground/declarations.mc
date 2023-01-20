bool foo() {
    return false;
}

void main() {
    int a;
    int b = 10;
    int c = a;
    int d = c = b;
    bool e = foo();
}