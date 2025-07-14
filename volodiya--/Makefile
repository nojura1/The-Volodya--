CC?=gcc
CFLAGS?=-Wall -Wextra -std=c17
TARGET?=volodya

all: $(TARGET)

$(TARGET): src/main.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -f $(TARGET)