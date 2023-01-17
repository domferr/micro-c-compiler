bool foo() { 
    return 1; /* foo returns boolean, not int */
}

int main() {
    foo();
    return 0;
}