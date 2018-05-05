#include <stdio.h>

int main(void) {
	static void *lbls[] = { &&lbl_h, &&lbl_e, &&lbl_l, &&lbl_l, &&lbl_o, &&lbl_quit };
	static void **lbl = lbls;
	
	goto **lbl;

lbl_e:
	printf("e");
	lbl++;
	goto **lbl;

lbl_o:
	printf("o");
	lbl++;
	goto **lbl;
lbl_h:
	printf("h");
	lbl++;
	goto **lbl;

lbl_l:
	printf("l");
	lbl++;
	goto **lbl;

lbl_quit:
	puts("");
	return 0;

}


