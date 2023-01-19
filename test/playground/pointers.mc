void main() {
    int val;
    int *ptr1;
    val = *ptr1;
    *ptr1 = val;
    int *ptr2;
    ptr2 = ptr1;
    ptr2 = &val;
    ptr2 = &*ptr1;
    int val2;
    val = *&val2;
}