int a[];

void foo(int c)
{
  int inner;
  int testing;
  testing = a[inner];
  a[inner] = c;
  return;
}

int b;
bool b2;

int without_params()
{
  int inside;
  inside = foo(b);
  inside = foo(10);
  inside = foo(10, b);
  return 0;
}

int brace_on_this_line() {
  int inside;
  return without_params();
}

int dummy() {
  int inside;
  return with_params(inside);
}

int _starting_with_underscore;
int _____starting_with_multiple_underscores;

void multiple_params(int c, int d)
{
  int inner;
  bool inside;
}

int mixed()
{
  int inside;
  inside = foo(b) + boo(c);
  inside = 10 - 20;
  inside = 10 + foo(10, b);
  return 0 - foo(g);
}

void empty() {}

void test_while() {
  bool loop;

  while(loop) {
    int a;
    a = 10;
  }
}

void test_and_or() {
  if (a || b) {
    a = 10;
  }

  if (a && b) {
    b = 1;
  }

  bool yes;
  yes = a || b;
}

int main() {
  int negation;
  negation = 10 - -8;

  int not;
  not = !true;
}

int test_for(int a) {
  int i;
  for(; ; ) {
    a = a + 10;
  }
  for(; ; i = i + 1) {
    a = a + 10;
  }
  for( ; i < 10; ) {
    a = a + 10;
  }
  for(; i < 10; i = i + 1) {
    a = a + 10;
  }
  for(i = 0; ; ) {
    a = a + 10;
  }
  for(i = 0; ; i = i + 1) {
    a = a + 10;
  }
  for(i = 0; i < 10; ) {
    a = a + 10;
  }
  for(i = 0; i < 10; i = i + 1) {
    a = a + 10;
  }

  i = 0;
  while(i<10) {
    a = a + 10;
    i = i + 1;
  }
}

void test_chars() {
  char ch;
  ch = 'd';
  ch = '\'';
  ch = '\b';
  ch = '\t';
  ch = '\\';
  ch = '\r';
  ch = '\n';
}