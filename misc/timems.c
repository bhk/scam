#include <stdio.h>
#include <sys/time.h>

int main(int argc, char **argv)
{
   struct timeval tv;
   gettimeofday(&tv, (struct timezone *) NULL);
   printf("%lu.%03lu", tv.tv_sec, tv.tv_usec / 1000LU);
}
