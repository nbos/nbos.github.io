TARGETS = clean t1.svg t2.svg t3.svg t4.svg t5.svg

all: $(TARGETS)

%.svg: %.dot
	dot -Tsvg $< -o $@

clean:
	rm -f $(TARGETS)
