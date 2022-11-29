int main() {
    if (b)
        if (a)
            x = 1;
        else
            x = 17;
    else
        x = 20;
  return x;
}

int test() {

    if (b) x = 1; x=2; else x = 1; //must fail because of bad 'else'
    return 0;
}