#!/usr/bin/perl

# dehtml.pl
#
# Intended to be run as a procmail filter rule. It takes as input, an
# email message of type text/html. It runs the html body through lynx
# and thereby parses out a plaintext file. It then outputs a multipart
# message with the plaintext inline and the original html contents as
# an attachment.
#
# The procmail rule should be:
#
# :0bhf
# * ^[Cc]ontent-[Tt]ype: text/html
# | dehtml.pl

$hdr  = "";
$html = "";
$body = "";

$host = `hostname`;
chomp $host;
$tmpf = "/tmp/dehtml-$host-$$";

$bound = "XYZ_thIS_shoULdnt_aPpeaR_123_in_AnyTHing";

while (<>) {
    last if /^$/;

    # Fix the content type
    s|[Cc]ontent-[Tt]ype: text/html|Content-Type: multipart/mixed; boundary="$bound"|;
    
    $hdr .= $_;
}

while (<>) {
    $html .= $_;
}

if (open(TMP, "> $tmpf")) {
    print(TMP $html);
    close(TMP);
}

# Note that if this command fails for any reason, most likely because
# lynx isn't in the path, then all that happens is that $body ends up
# as an empty string. The original html is still in $html and so will
# be attached properly.
if (open(LYNX, "lynx -dump -force_html $tmpf |")) {
    while(<LYNX>) {
	s/^   //;
	$body .= $_;
    }
    close LYNX;
}

unlink($tmpf);

print($hdr);
print("\n\n");

print("--$bound\n");
print("Content-Type: text/plain; charset=us-ascii\n");
print("Content-Disposition: inline\n");
print("\n");
print($body);

print("--$bound\n");
print("Content-Type: text/html; charset=us-ascii\n");
print("Content-Disposition: attachment; filename=\"original.html\"\n");
print("\n");
print($html);

print("\n");
print("--$bound--\n");
print("\n");
