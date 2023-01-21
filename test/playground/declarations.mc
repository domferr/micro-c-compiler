int foo() {
    return 10;
}

int a;
int b, c;
int d = 10;
int e, f = 2;
int g, h = e;
int i, j = f, k;
int n, o = 10, p = g;
int q, r = foo();
int s, t = s + foo(), u;

void no_decl() {

}

void main() {
    int x1;
    int x2 = 10;
    int x3 = x1;
    int x4 = x3 = x2;
    int x6, x7;
    int x8, x9 = 2;
    int x10, x11 = x8;
    int x12, x13 = x9;
    int x14, x15, x16;
    int x17, x18 = 10, x19 = x6;
    int x20, x21 = foo();
    int x22, x23 = x22 + foo(), x24;
}