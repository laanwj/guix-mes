
int
main ()
{
  static void *lbl = &&lbl_b;
  
  goto *lbl;
 lbl_a:
  return 1;
lbl_b:
  return 0;
}

