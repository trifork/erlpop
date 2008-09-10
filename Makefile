OUTDIR=ebin

all:
	if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	erlc -o $(OUTDIR) src/*.erl

clean:
	rm $(OUTDIR)/*.beam
