rm -r .store src/mes.gcc src/mes.o lib/*.o
rm -f crt*
cp $GUIX_ENVIRONMENT/lib/crt* .
CC=i686-unknown-linux-gnu-gcc ./make.scm src/mes.gcc
/gnu/store/3kjpz1shbp7hqf5y1garxarz9d027qh6-profile/bin/patchelf --set-interpreter /gnu/store/vcix667q0565nsd9gxk42nj2pmph5i6v-glibc-cross-i686-unknown-linux-gnu-2.26.105-g0890d5379c/lib/ld-linux.so.2 src/mes.gcc
mv src/mes.gcc src/mes.gcc-32
