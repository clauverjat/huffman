GNATMAKE_CC ?= gnatmake
ADA_FLAGS=-gnata -gnatwa -I../specifications/ -I../src/
SOURCES=../src/compress.o ../src/decompress.o
TEST_SOURCES=../tests/test_arbre.o ../tests/test_compression.o ../tests/test_decompression.o ../tests/test_file_a_priorite.o ../tests/test_liste.o

all: ${SOURCES}

%.o: %.adb
	${GNATMAKE_CC} ${ADA_FLAGS} $< -o `basename -s .o $@`

test: ${TEST_SOURCES}
	./test.sh
