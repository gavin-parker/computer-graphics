# Header files
HDIR = include

# Source files
SRCDIR = src
SRCS = $(wildcard $(SRCDIR)/*.cpp)

# Object files
BUILDDIR = build
OBJS = $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%.o,$(basename $(SRCS)))

# Binary files
BINDIR = bin
BINARYNAME = computer_graphics
BINARY = $(BINDIR)/$(BINARYNAME)

# Compilation options
CXXFLAGS = -std=c++11 -Wall -Wextra -Werror -ggdb -g3 -I$(HDIR) $(shell sdl-config --cflags)
COMPILE = $(CXX) -o $@ -c $^ $(CXXFLAGS)

# Link Options
LDFLAGS += $(shell sdl-config --libs)
LINK = $(CXX) -o $@ $^ $(LDFLAGS) $(LDLIBS)

.PHONY: all clean
all: $(BINARY)
clean:
	@$(RM) $(BUILDDIR)/*.o $(BINARY) screenshot.bmp

# Compiling Objects
$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp
	@echo $@
	@$(COMPILE)

# Compiling Release Binary
$(BINARY): $(OBJS)
	@echo $@
	@$(LINK)
