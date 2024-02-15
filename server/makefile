TARGET = $(BIN_DIR)/program
CFLAGS = -Wall -Wextra -std=c99
SRC_DIR = src
OBJS_DIR = obj
BIN_DIR = bin
OBJS = $(OBJS_DIR)/arr_func.o \
$(OBJS_DIR)/bin_manage.o $(OBJS_DIR)/cache.o \
$(OBJS_DIR)/common.o $(OBJS_DIR)/evict.o \
$(OBJS_DIR)/list.o $(OBJS_DIR)/malloc_interface.o \
$(OBJS_DIR)/memcached.o $(OBJS_DIR)/parser.o \
$(OBJS_DIR)/stats.o $(OBJS_DIR)/run.o \
$(OBJS_DIR)/text_manage.o $(OBJS_DIR)/user_data.o

$(TARGET): $(OBJS)
	mkdir -p $(BIN_DIR)
	gcc $(CFLAGS) $(OBJS) -o $(TARGET)

$(OBJS_DIR)/%.o: $(SRC_DIR)/%.c
	mkdir -p $(OBJS_DIR)
	gcc -c $(CFLAGS) $< -o $@

.PHONY: clean
clean:
	@rm -r $(OBJS_DIR) $(BIN_DIR)