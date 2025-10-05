# COBOL Makefile
# ==============

# Compiler settings
COBC = cobc
COBCFLAGS = -x -free -Wall -Wno-unused -Wno-ignored
COBCFLAGS_DEBUG = -g -debug
COBCFLAGS_RELEASE = -O2

# Directories
SRCDIR = src
BINDIR = bin
LIBDIR = lib
SAMPLEDIR = samples

# Source files
COBOL_SOURCES = $(wildcard $(SRCDIR)/*.cob)
COBOL_OBJECTS = $(COBOL_SOURCES:$(SRCDIR)/%.cob=$(BINDIR)/%.exe)

# Sample files
SAMPLE_SOURCES = $(wildcard $(SAMPLEDIR)/*.cob)
SAMPLE_OBJECTS = $(SAMPLE_SOURCES:$(SAMPLEDIR)/%.cob=$(BINDIR)/%.exe)

# Default target
all: $(COBOL_OBJECTS) $(SAMPLE_OBJECTS)

# Compile COBOL programs
$(BINDIR)/%.exe: $(SRCDIR)/%.cob
	@echo Compiling $<...
	@mkdir -p $(BINDIR)
	$(COBC) $(COBCFLAGS) $(COBCFLAGS_RELEASE) -o $@ $<

# Compile sample programs
$(BINDIR)/%.exe: $(SAMPLEDIR)/%.cob
	@echo Compiling sample $<...
	@mkdir -p $(BINDIR)
	$(COBC) $(COBCFLAGS) $(COBCFLAGS_RELEASE) -o $@ $<

# Debug build
debug: COBCFLAGS += $(COBCFLAGS_DEBUG)
debug: all

# Clean build artifacts
clean:
	@echo Cleaning build artifacts...
	@if exist $(BINDIR) rmdir /s /q $(BINDIR)
	@echo Clean complete.

# Run all programs
run: all
	@echo Running all programs...
	@for %%f in ($(BINDIR)\*.exe) do (
		@echo Running %%f...
		@%%f
		@echo.
	)

# Install dependencies (placeholder)
install:
	@echo Please install GnuCOBOL manually:
	@echo 1. Download from: https://sourceforge.net/projects/gnucobol/
	@echo 2. Or use package manager: choco install gnucobol
	@echo 3. Or use MSYS2: pacman -S mingw-w64-x86_64-gnucobol

# Help
help:
	@echo COBOL Development Makefile
	@echo ========================
	@echo.
	@echo Available targets:
	@echo   all      - Build all COBOL programs (default)
	@echo   debug    - Build with debug information
	@echo   clean    - Remove all build artifacts
	@echo   run      - Build and run all programs
	@echo   install  - Show installation instructions
	@echo   help     - Show this help message
	@echo.
	@echo Usage:
	@echo   make [target]
	@echo.

.PHONY: all debug clean run install help
