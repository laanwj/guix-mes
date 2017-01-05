#if __GNUC__

void
write (int fd, char const* s, int n)
{
  int r;
  //syscall (SYS_write, fd, s, n));
  asm (
       "mov %0, %%ebx\n\t"
       "mov %1, %%ecx\n\t"
       "mov %2, %%edx\n\t"

       "mov $0x4, %%eax\n\t"
       "int $0x80\n\t"
       : // no outputs "=" (r)
       : "" (fd), "" (s), "" (n)
       : "eax", "ebx", "ecx", "edx"
       );
}

void
exit (int code)
{
  asm (
       "movl %0, %%ebx\n\t"
       "movl $1,  %%eax\n\t"
       "int  $0x80"
       : // no outputs "=" (r)
       : "" (code)
       );
  // not reached
  exit (0);
}

#define STDOUT 1

typedef long size_t;
size_t
strlen (char const* s)
{
  int i = 0;
  while (s[i]) i++;
  return i;
}

int
puts (char const* s)
{
  //write (STDOUT, s, strlen (s));
  //int i = write (STDOUT, s, strlen (s));
  int i = strlen (s);
  write (1, s, i);
  return 0;
}
#endif

int
main ()
{
  puts ("Hi Mes!\n");
  for (int i = 0; i < 4; ++i)
    puts ("  Hello, world!\n");
  return 42;
}

#if __GNUC__
void
_start ()
{
  int r=main ();
  exit (r);
}
#endif
