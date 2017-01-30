# Header files
HDIR := include $(GLMDIR)

# Source files
SRCDIR = src
SRCS = $(wildcard $(SRCDIR)/*.cpp)

# Dependencies
DEPDIR = dependencies
DEPFLAGS = -MT $@ -MMD -MP -MF $(DEPDIR)/$*.d

# Object files
BUILDDIR = build
OBJS = $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%.o,$(basename $(SRCS)))

# Binary files
BINDIR = bin
BINARYNAME = computer_graphics
BINARY = $(BINDIR)/$(BINARYNAME)

# Compilation options
CXXFLAGS = -std=c++11 -Wall -Werror -ggdb -g3 $(addprefix -I, $(HDIR)) $(shell sdl-config --cflags)
COMPILE = $(CXX) -o $@ -c $< $(CXXFLAGS)

# Link Options
LDFLAGS += $(shell sdl-config --libs)
LINK = $(CXX) -o $@ $^ $(LDFLAGS) $(LDLIBS)

.PHONY: all clean
all: $(BINARY)
clean:
	@$(RM) $(BUILDDIR)/*.o $(DEPDIR)/*.d $(BINARY) screenshot.bmp

# Compiling Objects
$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp
	$(info $@)
	@$(COMPILE)

# Compiling Release Binary
$(BINARY): $(OBJS)
	$(info $@)
	@$(LINK)

include $(wildcard $(DEPDIR)/*.d)
