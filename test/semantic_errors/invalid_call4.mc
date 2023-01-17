void foo(int a, char c) {

}

int main() { 
    int a;
    bool b;
    foo(a, b); /* foo requires int and char but int and bool is given */
}