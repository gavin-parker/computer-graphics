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
CXXFLAGS = -std=c++11 -Wall -Werror -Og -fopenmp -ggdb -g3 -pg $(addprefix -I, $(HDIR)) $(shell sdl-config --cflags) $(DEPFLAGS)
COMPILE = $(CXX) -o $@ -c $< $(CXXFLAGS)

# Link Options
LDFLAGS += $(shell sdl-config --libs) -fopenmp
LINK = $(CXX) -o $@ $^ $(LDFLAGS) $(LDLIBS)

.PHONY: all clean
all: $(BUILDDIR) $(DEPDIR) $(BINDIR) $(BINARY)
clean:
	@$(RM) $(BUILDDIR)/*.o $(DEPDIR)/*.d $(BINARY) screenshot.bmp

$(BUILDDIR) $(DEPDIR) $(BINDIR):
	mkdir $@

# Compiling Objects
$(BUILDDIR)/%.o: $(SRCDIR)/%.cpp
	$(info $@)
	@$(COMPILE)

# Compiling Release Binary
$(BINARY): $(OBJS)
	$(info $@)
	@$(LINK)

include $(wildcard $(DEPDIR)/*.d)
