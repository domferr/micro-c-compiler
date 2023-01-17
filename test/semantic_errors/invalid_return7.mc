bool foo() { 
    return false;
}

int main() {
    return foo(); /* foo returns boolean, not int */
}