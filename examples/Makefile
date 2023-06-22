# Quick and dirty Makefile

_build/test: _build/test.c rust_build
	@echo "[BUILDING C]";\
	gcc -I../runtime/include _build/test.c ../target/debug/libruntime.a -o _build/test

_build/test.c: test.js rust_build
	@echo "[GENERATING C]";\
	mkdir -p _build; \
	../target/debug/codegen test.js > _build/test.c

rust_build:
	@echo "[BUILDING RUST]";\
	cd ..;\
	cargo build;\
	cd examples

run: _build/test
	@echo "[RUNNING]"; \
	_build/test

format: _build/test.c
	@echo "[FORMATTING]"; \
	clang-format -i _build/test.c
	
clean:
	@rm -rf _build;\
	echo "[CLEAN]"
