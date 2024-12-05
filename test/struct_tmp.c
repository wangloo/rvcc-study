/* Copyright (C) 1991-2021 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <https://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
int main() {
  // [49] 支持struct
  assert(1, ({ struct {int a; int b;} x; x.a=1; x.b=2; x.a; }), "({ struct {int a; int b;} x; x.a=1; x.b=2; x.a; })");
  // ASSERT(2, ({ struct {int a; int b;} x; x.a=1; x.b=2; x.b; }));
  // ASSERT(1, ({ struct {char a; int b; char c;} x; x.a=1; x.b=2; x.c=3; x.a; }));
  // ASSERT(2, ({ struct {char a; int b; char c;} x; x.b=1; x.b=2; x.c=3; x.b; }));
  // ASSERT(3, ({ struct {char a; int b; char c;} x; x.a=1; x.b=2; x.c=3; x.c; }));
  // ASSERT(0, ({ struct {char a; char b;} x[3]; char *p=x; p[0]=0; x[0].a; }));
  // ASSERT(1, ({ struct {char a; char b;} x[3]; char *p=x; p[1]=1; x[0].b; }));
  // ASSERT(2, ({ struct {char a; char b;} x[3]; char *p=x; p[2]=2; x[1].a; }));
  // ASSERT(3, ({ struct {char a; char b;} x[3]; char *p=x; p[3]=3; x[1].b; }));
  // ASSERT(6, ({ struct {char a[3]; char b[5];} x; char *p=&x; x.a[0]=6; p[0]; }));
  // ASSERT(7, ({ struct {char a[3]; char b[5];} x; char *p=&x; x.b[0]=7; p[3]; }));
  // ASSERT(6, ({ struct { struct { char b; } a; } x; x.a.b=6; x.a.b; }));
  // ASSERT(8, ({ struct {int a;} x; sizeof(x); }));
  // ASSERT(16, ({ struct {int a; int b;} x; sizeof(x); }));
  // ASSERT(16, ({ struct {int a, b;} x; sizeof(x); }));
  // ASSERT(24, ({ struct {int a[3];} x; sizeof(x); }));
  // ASSERT(32, ({ struct {int a;} x[4]; sizeof(x); }));
  // ASSERT(48, ({ struct {int a[3];} x[2]; sizeof(x); }));
  // ASSERT(2, ({ struct {char a; char b;} x; sizeof(x); }));
  // ASSERT(9, ({ struct {char a; int b;} x; sizeof(x); }));
  // ASSERT(0, ({ struct {} x; sizeof(x); }));
  printf("OK\n");
  return 0;
}
