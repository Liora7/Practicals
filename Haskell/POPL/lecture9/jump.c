#include <stdio.h>
#include <setjmp.h>

int example () {  
   int x = 5;
   int r = 1;

   jmp_buf l;
   setjmp(l);
   if (x==1)
     { return r; }
   else
     { r = x*r ; x = x-1; longjmp(l,0);}

}
int main () {
  printf("Value of r: %d\n",example());
}
