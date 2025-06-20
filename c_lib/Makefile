# Makefile for building the static C library

# Compiler and flags
CC = clang
CFLAGS = -O2 -Wall

# Target library
TARGET = libkeylistener.a

# Source files
SRCS = key_listener.c
OBJS = $(SRCS:.c=.o)

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJS)
	# Create a static library (archive) from the object files
	ar rcs $@ $(OBJS)
	@echo "C library built successfully: $(TARGET)"

%.o: %.c key_listener.h
	# Compile the C source file into an object file
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGET)

