Prime sieve implementation
  $ echo 100 >in.txt

  $ koka-zero interpret primes.kk <in.txt
  2 4 6 8 

  $ export PROJECT_ROOT=../../..
  $ ../compile.sh primes.kk
  $ ./primes <in.txt
  2 4 6 8 

