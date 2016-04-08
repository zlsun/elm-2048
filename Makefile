
MAKE = make --no-print-directory
OPEN = xdg-open

HTML = index.html

$(HTML): src/2048.elm
	elm make $< --output $@

run: $(HTML)
	$(OPEN) $<

clean:
	$(RM) $(HTML)

rebuild:
	$(MAKE) clean
	$(MAKE)

