#!/usr/bin/perl

# rehtml.pl
#
# Undo the action of dehtml.pl
#

use MIME::Base64;

$bound = "XYZ_thIS_shoULdnt_aPpeaR_123_in_AnyTHing";

#
# To handle non-dehtml'd mail, just echo until we find the boundary marker
# (which should be in the Content-Type line).
#
while (<>) {
    last if /$bound/;
    print $_;
}

#
# If there's no boundary marker, then the message wasn't dehtml'd.
#
if (eof STDIN) {
    exit 0;
}

#
# Fix the content type
#
if (!/Content-Type/) {
    print STDERR "ERROR: unexpected boundary line!!\n";
    exit 1;
}
s|^Content-Type:.*|Content-Type: text/html|i;
print $_;

#
# Echo the rest of the header part.
#
while (<>) {
    last if /^$/;

    # Strip Content-Length and Lines
    next if /^Content-Length/;
    next if /^Lines/;

    print $_;
}

#
# Need a blank line after the header
#
print "\n";

#
# Skip the part before the first bound
#
while (<>) {
    last if /$bound/;
}

#
# And the part up to the next bound
#
while (<>) {
    last if /$bound/;
}

#
# And the next three lines
#
$content_type = <>;
$content_disposition = <>;
$blank_line = <>;

#
# Echo the old html body
#
while (<>) {
    last if /$bound/;
    print $_;
}

#
# C'est fini.
#
