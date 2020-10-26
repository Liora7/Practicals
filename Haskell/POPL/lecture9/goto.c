#include <stdio.h>
 
int example () {  
   int x = 5;
   int r = 1;

 L :
   if (x==1)
     { return r; }
   else
     { r = x*r ; x = x-1; goto L;}

}
int main () {
  printf("Value of r: %d\n",example());
}
