#!/usr/bin/perl -w
#
# (C) Copyright M. D Mackey 2004. This program may be freely modified and redistributed, 
# but I would appreciate feedback to mark\@swallowtail\.org.
#
# A short program to patch an executable compiled with the Intel Fortran Compiler.
# Run with -h for help/
#
# The patch removes the check for the string "GenuineIntel" in the 
# CPUID flags of the processor, thus making SSE/SSE2 code work on AMD chips that
# support it.
#
# Tested on executables compiled with compiler versions 7.1.040, 8.1.028 and 9.0.024, 
# and should work on other builds.
#
# Creates a backup file before changing anything, so should be easy to undo if
# things go wrong.
#
# If you want to check for a successful patch, do
#
# objdump -d <executable>~ > old
# objdump -d <executable> > new
# 
# and examine the differences between 'old' and 'new'. Three lines
# should have changed in __intel_cpu_indicator_init:
#
# 81 fa 47 65 6e 75          cmp    $0x756e6547,%edx
# ...
# 81 fa 69 6e 65 49          cmp    $0x49656e69,%edx
# ...
# 81 fa 6e 74 65 6c          cmp    $0x6c65746e,%edx
#
# (these check for "GenuineIntel"). These lines should all be changed to 
# f7 c2 00 00 00 00          testl  $0x00000000,%edx
#
# (the registers might be %eax, %ebx, %ecx or %edx, depending on compiler version)

use strict;
use Getopt::Std;

# Handle options
my(%opts);
my($ok)=getopts('Hhb:v',\%opts);
usage() if ($opts{h} or $opts{H} or !$ok or !@ARGV);
my($backup)="~";
$backup=$opts{b} if ($opts{b});
my($verbose)=$opts{v};

my($file);
foreach $file (@ARGV) {
  print STDERR "Patching file $_...\n" if ($verbose);

  # Check backup file
  if ($backup and -e "$file$backup") {
    warn("Error: backup file '$file$backup' for '$file' already exists, so not patching!");
    next;
  }

  open(FILE,$file) or die("Couldn't open file '$file': $!");
  binmode(FILE);
  my($contents)=join('',<FILE>); # Shlurp up file into one long binary string
  close(FILE);

  my($unp)=unpack('H*',$contents); # Unpack into a sequence of hex bytes

  my($count);

  # Try patching "cmp    $0x756e6547,%eax" to "test   $0x00000000,%eax" etc
  $count+=($unp=~s/3d47656e75/a900000000/);
  $count+=($unp=~s/3d696e6549/a900000000/);
  $count+=($unp=~s/3d6e74656c/a900000000/);

  # Now try patching "cmp    $0x756e6547,%ebx" to "testl  $0x00000000,%ebx" etc
  $count+=($unp=~s/81fb47656e75/f7c300000000/);
  $count+=($unp=~s/81fb696e6549/f7c300000000/);
  $count+=($unp=~s/81fb6e74656c/f7c300000000/);

  # Now try patching "cmp    $0x756e6547,%ecx" to "testl  $0x00000000,%ecx" etc
  $count+=($unp=~s/81f947656e75/f7c100000000/);
  $count+=($unp=~s/81f9696e6549/f7c100000000/);
  $count+=($unp=~s/81f96e74656c/f7c100000000/);

  # Now try patching "cmp    $0x756e6547,%edx" to "testl  $0x00000000,%edx" etc
  $count+=($unp=~s/81fa47656e75/f7c200000000/);
  $count+=($unp=~s/81fa696e6549/f7c200000000/);
  $count+=($unp=~s/81fa6e74656c/f7c200000000/);

  next if (!$count); # Don't patch if no substitutions made
  if ($count % 3) {  # Number of substitutions must be a multiple of 3!
    warn "WARNING: $count lines were to be patched in '$file': should be a multiple of 3! Skipping this file.\n";
    next;
  }
  print STDERR "Patching $file in $count places...";
  die "Error: wanted to create temp file '$file.$$.tmp' but it already exists!" if (-e "$file.$$.tmp");
  open(OUTPUT,">$file.$$.tmp") or die("Can't create temporary output file '$file.$$.tmp':$!");
  binmode(OUTPUT);
  print OUTPUT pack('H*',$unp);
  close(OUTPUT) or die "Can't close file '$file.$$.tmp'";

  `chmod --reference="$file" "$file.$$.tmp"`; # chmod to original file's mode

  # Rename original file to backup
  if ($backup) {
    rename($file,"$file$backup") or die("Couldn't rename '$file' to '$file$backup': $!\n");
  }
  rename("$file.$$.tmp",$file) or die("Couldn't rename '$file.$$.tmp' to '$file': $!\n");
  if ($backup) {
    print STDERR "Patch operation for '$file' successful, original file at '$file$backup'\n" if ($verbose);
  } else {
    print STDERR "Patch operation for '$file' successful, no backup created\n" if ($verbose);
  }
}
exit(0);

sub usage {
  print STDERR <<EOUSAGE;

Usage: intel_check_executable_patch.pl [options] <filename> [<filename>...]

Options:
	-b <suffix>	Use the supplied suffix to name the backup file. The
			default is '~', and setting this to '' disables backups.
	-v 		Be more verbose.

Some versions of the Intel Fortran Compiler produce code which checks whether
the CPU is made by Intel or not, and disables MMX/SSE/SSE2 code if it isn't. 
This program patches executables produced by the Intel compiler to remove this 
check, so that e.g. programs compiled with -axW will use SSE2 code on Athlon 
64 chips.

This script was tested against ifc versions 7.1.040, 8.1.028 and 9.0.024 and 
should work on other version 7 to 9 releases.  The patch process will almost
certainly work with executables compiled with the Intel C compiler as well, and
with Windows executables as well as Linux binaries, but has not been extensively 
tested on these, so be careful! If you do test this on icc-compiled
executables, then please let me know how it goes!

Patched files are backed up (by default to 'filename~'), see the -b flag to 
change this.

This program is provided with NO WARRANTY: please check that the patched executables
behaves as you would expect! I can't really foresee any problems that could reasonably
be caused by the use of this patch, but you never know :).

EOUSAGE
  exit(1);
}
