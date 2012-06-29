SOURCES=migrations_1.dot migrations_2.dot migrations_3.dot migrations_4.dot
IMAGES=$(SOURCES:.dot=.png)
VECTORS=$(SOURCES:.dot=.svg)

install: $(IMAGES)
	cp $(IMAGES) ../../static/img/graphs/

all: $(IMAGES)

$(IMAGES) : %.png : %.svg
	convert $< -resize 500x500\> $@

vectors: $(VECTORS)

.PHONY: clean

clean:
	rm -f *.png *.svg

$(VECTORS) : %.svg : %.dot
	dot -Tsvg $< -o $@
