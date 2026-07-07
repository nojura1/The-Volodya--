CC := gcc
TARGET := vmm

SRC_DIR := src
BUILD_DIR := build

SOURCES := $(shell find $(SRC_DIR) -type f -name '*.c')
OBJECTS := $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(SOURCES))
DEPS := $(OBJECTS:.o=.d)

CPPFLAGS := \
	-I$(SRC_DIR) \
	-I$(SRC_DIR)/lexer \
	-I$(SRC_DIR)/parser \
	-I$(SRC_DIR)/semantic \
	-I$(SRC_DIR)/codegen \
	-I$(SRC_DIR)/Token

BASE_CFLAGS := -std=c2x -Wall -Wextra -Wconversion -Wpedantic
CFLAGS := $(BASE_CFLAGS) -O2
LDFLAGS :=
LDLIBS :=

.PHONY: all clean rebuild debug test

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(LDFLAGS) $^ $(LDLIBS) -o $@

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) -MMD -MP -c $< -o $@

debug:
	$(MAKE) clean
	$(MAKE) \
		CFLAGS="$(BASE_CFLAGS) -O0 -g3 -fsanitize=address,undefined" \
		LDFLAGS="-fsanitize=address,undefined"

test: $(TARGET)
	@mkdir -p build
	./$(TARGET) tests/FINAL_TEST.vol -o build/final_test
	./build/final_test
	rm -f a.out

rebuild: clean all

clean:
	rm -rf $(BUILD_DIR) $(TARGET)

-include $(DEPS)
