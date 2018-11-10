.PHONY: run

run: mistery_word
	./mistery_word

mistery_word: mistery_word.idr
	idris -p effects -p contrib mistery_word.idr -o mistery_word
