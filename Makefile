INCLUDE=include/
OUTDIR=ebin

all:
	if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	erlc -I $(INCLUDE) -o $(OUTDIR) src/*.erl

clean:
	rm $(OUTDIR)/*.beam
