use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

use vars qw($OS_win $version);
$version = '2.03';

print <<INTRO;

                     ControlX10::CM11 VERSION $version
                     Test with:  perl test.pl [0..5]

   The Test sends data to the emulator, does not require a CM11 attached

INTRO

BEGIN {
        $OS_win = ($^O eq "MSWin32") ? 1 : 0;

        if ($OS_win) {
            require 5.005;
        }
} # End BEGIN

unless ($OS_win) {
    WriteMakefile(
        'NAME'	=> 'ControlX10::CM11',
        'VERSION_FROM' => 'CM11.pm', # finds $VERSION
        'SKIP'	=> [qw(tool_autosplit)],
    );
    exit;
}

# On Windows, create substitute scripts for the "make deprived"

use File::Copy;
use File::Path;

my $INST_LIBDIR = "./lib/ControlX10";
my $INST_FILES = "CM11.pm";

print <<INTRO3;
                      No 'Makefile' will be created
                      Install with: perl install.pl

INTRO3

File::Path::mkpath([ "$INST_LIBDIR" ],1,0777) or
    die "ERROR creating directories: ($!)\n";
File::Copy::copy($INST_FILES,$INST_LIBDIR) or
    die "ERROR copying files: ($!)\n";

my $dfile = "install.pl";
unlink $dfile;
print "Creating new $dfile\n";
open (DEFAULT, "> $dfile") or die "Can't create $dfile: $!\n";

print DEFAULT <<INST5;
# Created by Makefile.PL
# VERSION $version

BEGIN { require 5.005; }

use Config qw(\%Config);
use strict;
use ExtUtils::Install qw( install );

my \$FULLEXT = "ControlX10/CM11";
my \$INST_LIB = "./lib";

install({
	   read => "\$Config{sitearchexp}/auto/\$FULLEXT/.packlist",
	   write => "\$Config{installsitearch}/auto/\$FULLEXT/.packlist",
	   \$INST_LIB => "\$Config{installsitelib}",
	  },1,0);

__END__
INST5

close DEFAULT;
