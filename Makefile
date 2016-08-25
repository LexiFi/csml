all:
	$(MAKE) -C src all

demo:
	$(MAKE) -C examples/getting_started all

demo_winforms:
	$(MAKE) -C examples/winforms addin_byt

clean:
	$(MAKE) -C src clean
	$(MAKE) -C examples/getting_started clean
	$(MAKE) -C examples/winforms clean
	rm -f *~
