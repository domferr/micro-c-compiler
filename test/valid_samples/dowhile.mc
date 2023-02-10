void main() {
    int i = 10;
    bool b = true;
    do {
        int a = i;
        print(a);
        i++;
        b = a < 20;
    } while(b);
}