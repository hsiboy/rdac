# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.22

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /Users/stuart.taylor/.homebrew/Cellar/cmake/3.22.2/bin/cmake

# The command to remove a file.
RM = /Users/stuart.taylor/.homebrew/Cellar/cmake/3.22.2/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/stuart.taylor/repos/rdac/rdac2wav

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/stuart.taylor/repos/rdac/build

# Include any dependencies generated for this target.
include CMakeFiles/rdac2wav.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include CMakeFiles/rdac2wav.dir/compiler_depend.make

# Include the progress variables for this target.
include CMakeFiles/rdac2wav.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/rdac2wav.dir/flags.make

CMakeFiles/rdac2wav.dir/src/wav.c.o: CMakeFiles/rdac2wav.dir/flags.make
CMakeFiles/rdac2wav.dir/src/wav.c.o: /Users/stuart.taylor/repos/rdac/rdac2wav/src/wav.c
CMakeFiles/rdac2wav.dir/src/wav.c.o: CMakeFiles/rdac2wav.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/stuart.taylor/repos/rdac/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object CMakeFiles/rdac2wav.dir/src/wav.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -MD -MT CMakeFiles/rdac2wav.dir/src/wav.c.o -MF CMakeFiles/rdac2wav.dir/src/wav.c.o.d -o CMakeFiles/rdac2wav.dir/src/wav.c.o -c /Users/stuart.taylor/repos/rdac/rdac2wav/src/wav.c

CMakeFiles/rdac2wav.dir/src/wav.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/rdac2wav.dir/src/wav.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/stuart.taylor/repos/rdac/rdac2wav/src/wav.c > CMakeFiles/rdac2wav.dir/src/wav.c.i

CMakeFiles/rdac2wav.dir/src/wav.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/rdac2wav.dir/src/wav.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/stuart.taylor/repos/rdac/rdac2wav/src/wav.c -o CMakeFiles/rdac2wav.dir/src/wav.c.s

CMakeFiles/rdac2wav.dir/src/decode.c.o: CMakeFiles/rdac2wav.dir/flags.make
CMakeFiles/rdac2wav.dir/src/decode.c.o: /Users/stuart.taylor/repos/rdac/rdac2wav/src/decode.c
CMakeFiles/rdac2wav.dir/src/decode.c.o: CMakeFiles/rdac2wav.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/stuart.taylor/repos/rdac/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building C object CMakeFiles/rdac2wav.dir/src/decode.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -MD -MT CMakeFiles/rdac2wav.dir/src/decode.c.o -MF CMakeFiles/rdac2wav.dir/src/decode.c.o.d -o CMakeFiles/rdac2wav.dir/src/decode.c.o -c /Users/stuart.taylor/repos/rdac/rdac2wav/src/decode.c

CMakeFiles/rdac2wav.dir/src/decode.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/rdac2wav.dir/src/decode.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/stuart.taylor/repos/rdac/rdac2wav/src/decode.c > CMakeFiles/rdac2wav.dir/src/decode.c.i

CMakeFiles/rdac2wav.dir/src/decode.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/rdac2wav.dir/src/decode.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/stuart.taylor/repos/rdac/rdac2wav/src/decode.c -o CMakeFiles/rdac2wav.dir/src/decode.c.s

CMakeFiles/rdac2wav.dir/src/main.c.o: CMakeFiles/rdac2wav.dir/flags.make
CMakeFiles/rdac2wav.dir/src/main.c.o: /Users/stuart.taylor/repos/rdac/rdac2wav/src/main.c
CMakeFiles/rdac2wav.dir/src/main.c.o: CMakeFiles/rdac2wav.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/stuart.taylor/repos/rdac/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building C object CMakeFiles/rdac2wav.dir/src/main.c.o"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -MD -MT CMakeFiles/rdac2wav.dir/src/main.c.o -MF CMakeFiles/rdac2wav.dir/src/main.c.o.d -o CMakeFiles/rdac2wav.dir/src/main.c.o -c /Users/stuart.taylor/repos/rdac/rdac2wav/src/main.c

CMakeFiles/rdac2wav.dir/src/main.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/rdac2wav.dir/src/main.c.i"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /Users/stuart.taylor/repos/rdac/rdac2wav/src/main.c > CMakeFiles/rdac2wav.dir/src/main.c.i

CMakeFiles/rdac2wav.dir/src/main.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/rdac2wav.dir/src/main.c.s"
	/Library/Developer/CommandLineTools/usr/bin/cc $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /Users/stuart.taylor/repos/rdac/rdac2wav/src/main.c -o CMakeFiles/rdac2wav.dir/src/main.c.s

# Object files for target rdac2wav
rdac2wav_OBJECTS = \
"CMakeFiles/rdac2wav.dir/src/wav.c.o" \
"CMakeFiles/rdac2wav.dir/src/decode.c.o" \
"CMakeFiles/rdac2wav.dir/src/main.c.o"

# External object files for target rdac2wav
rdac2wav_EXTERNAL_OBJECTS =

rdac2wav: CMakeFiles/rdac2wav.dir/src/wav.c.o
rdac2wav: CMakeFiles/rdac2wav.dir/src/decode.c.o
rdac2wav: CMakeFiles/rdac2wav.dir/src/main.c.o
rdac2wav: CMakeFiles/rdac2wav.dir/build.make
rdac2wav: CMakeFiles/rdac2wav.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/stuart.taylor/repos/rdac/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Linking C executable rdac2wav"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/rdac2wav.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/rdac2wav.dir/build: rdac2wav
.PHONY : CMakeFiles/rdac2wav.dir/build

CMakeFiles/rdac2wav.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/rdac2wav.dir/cmake_clean.cmake
.PHONY : CMakeFiles/rdac2wav.dir/clean

CMakeFiles/rdac2wav.dir/depend:
	cd /Users/stuart.taylor/repos/rdac/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/stuart.taylor/repos/rdac/rdac2wav /Users/stuart.taylor/repos/rdac/rdac2wav /Users/stuart.taylor/repos/rdac/build /Users/stuart.taylor/repos/rdac/build /Users/stuart.taylor/repos/rdac/build/CMakeFiles/rdac2wav.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/rdac2wav.dir/depend

