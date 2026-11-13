EUID    := $(shell id -u -r)

all: purge-disk-cache evict-disk-cache

purge-disk-cache: purge-disk-cache.c
	gcc purge-disk-cache.c -o purge-disk-cache

# Unprivileged fallback (no setuid); `make evict-disk-cache` needs no root.
evict-disk-cache: evict-disk-cache.c
	gcc -O2 -Wall evict-disk-cache.c -o evict-disk-cache

install: purge-disk-cache evict-disk-cache
ifneq ($(EUID),0)
	@echo "Root or sudo is required" >&2
	@exit 1
endif
	mkdir -p /usr/local/sbin/ /usr/local/bin/
	cp purge-disk-cache /usr/local/sbin/
	chown root /usr/local/sbin/purge-disk-cache
	chmod 6711 /usr/local/sbin/purge-disk-cache
	cp evict-disk-cache /usr/local/bin/
	chmod 0755 /usr/local/bin/evict-disk-cache
